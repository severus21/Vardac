open Core
open Graph
open IR

(* We use undirected graphs with nodes containing a pair of integers *)
module Node = struct 
    type t = Atom.atom
    let compare = Pervasives.compare                                                 
   let hash = Hashtbl.hash                                                          
   let equal = (=)    
end

module Edge = struct 
    type t = tbridge option 
    let compare = Pervasives.compare                                                 
    let default = None 

    let hash = Hashtbl.hash                                                          
    let equal = (=)    
end

(* TODO FIXME move pp_st elsewhere *)
let rec 
pp_ctype { AstUtils.place ; AstUtils.value}= match value with
| TArrow (mt1, mt2) -> (pp_mtype mt1 ^"->"^pp_mtype mt2)
| TVar x -> Atom.hint x
| TFlatType ft -> begin
    match ft with
    | TBool -> "bool"
    | TInt -> "int"
    | TFloat -> "float"
    | TStr -> "str"
    | TVoid -> "Void"
end
| TDict (mt1, mt2) -> "dict<"^(pp_mtype mt1)^", "^(pp_mtype mt2)^">"
| TList mt -> "list<"^(pp_mtype mt)^">"
| TOption mt -> "option<"^(pp_mtype mt)^">"
| TResult (mt1, mt2) -> "list<"^(pp_mtype mt1)^", "^(pp_mtype mt2)^">"
| TSet mt -> "list<"^(pp_mtype mt)^">"
| TTuple mts -> "tuple<"^(List.fold_left (fun acc mt -> acc^(pp_mtype mt)^", ") "" mts)^">"
and pp_branch (label, st, _) = (Atom.hint label)^":"^(pp_stype st)
and pp_stype { AstUtils.place ; AstUtils.value}= 

match value with
| STEnd -> "."
| STVar x -> "-"^Atom.hint x
| STSend (mt, st) -> "!"^(pp_mtype mt)^(pp_stype st)
| STRecv (mt, st) -> "?"^(pp_mtype mt)^(pp_stype st)
| STBranch branches -> "+{"^(List.fold_left (fun acc x -> acc^"; "^(pp_branch x)) "" branches)^"}"
| STSelect branches -> "&"^(List.fold_left (fun acc x -> acc^"; "^(pp_branch x)) "" branches)^"}"
| STRec (x,st) -> "µ"^(Atom.hint x)^"."^(pp_stype st)
and pp_mtype { AstUtils.place ; AstUtils.value}= match value with
| CType ct -> pp_ctype ct
| SType st -> pp_stype st
| ConstrainedType (mt, _) -> pp_mtype mt (* TODO represent the onstraint*)


module type Params = sig
    val component2target : (Atom.atom, Atom.atom) Hashtbl.t
end

module type Sig = sig
    val generate_static_logical_topology : Fpath.t -> string -> IR.program -> unit 
end


module Make (Args:Params) : Sig = struct
    include Args
    let port2component : (Atom.atom, Atom.atom) Hashtbl.t = Hashtbl.create 128
    let clinline2component : (Atom.atom, Atom.atom) Hashtbl.t = Hashtbl.create 128

    module G = Imperative.Digraph.ConcreteBidirectionalLabeled(Node)(Edge)
    module GPrinter = struct
        include G
        

        let edge_attributes e = [`Label (match (G.E.label e)with
        | Some {protocol={value=SType st}} -> pp_stype st 
        |_ -> ""
        (*| Some b -> IR.show_tbridge b*)
        )]
        let default_edge_attributes _ = []

        (* 
            One subgraph per target
            tODO Maybe one  subgraph per isolation context
        *)
        let get_subgraph v :  Graphviz.DotAttributes.subgraph option = 
            let sources = [component2target;clinline2component; port2component] in
            let parent = Lazy.force (List.fold_left 
                (fun continuation source -> 
                    (Option.fold 
                        ~none: continuation 
                        ~some: (fun x -> (Lazy.from_val x)) 
                        (Hashtbl.find_opt source v))
                ) 
                (lazy(raise (Error.DeadbranchError (Printf.sprintf "component/clinline/port [%s] not found in component2target/port2component/clinline2component" (Atom.to_string v)))))
                sources 
                )
            in
            
            Some {
                sg_name = (Atom.to_string parent);
                sg_attributes = [
                    `HtmlLabel (Atom.hint parent);
                    `Style `Bold
                ];
                sg_parent = Option.map Atom.to_string (Hashtbl.find_opt component2target parent); (* FIXME does not yet support nested components *)    
            } 
        let vertex_attributes v = [`Shape `Box; `Label (Atom.hint v)]
        let vertex_name v = Atom.to_string v 
        let default_vertex_attributes _ = []
        let graph_attributes _ = []
    end

    module DotExport = Graphviz.Dot(GPrinter)
    let g = G.create ()

    let generate_possible_edges_from_tbridge {in_type; out_type; protocol} = 
        let rec explore error_header { AstUtils.place ; AstUtils.value} =
            match value with
            | CType {value=TVar x} -> [x] 
            | CType {value=TUnion (nt1, nt2)} -> (explore error_header nt1) @(explore error_header nt2) 
            | CompType {value=CompTUid cname} -> [cname] 
            | _ -> [] (* e.g. builtin signature -> polyvar *) 
        in
        let lefts = explore "fst" in_type in 
        let rights = explore "snd" out_type in 
        
        List.iter (function left ->
            (List.iter 
                (function right -> 
                    G.add_edge_e g (G.E.create left (Some {in_type; out_type; protocol}) right) )
                rights
            )
        ) lefts

    let generate_static_logical_topology export_dir name program =
        let components = 
        collect_term_program
            true
            (function | Component {value = ComponentStructure _ } -> true | _ -> false)
            (fun parent place (Component { value = ComponentStructure cstruct}) -> [cstruct])
            program
        in

        (* Create component nodes *)
        List.iter (function (cstruct:component_structure) -> 
            let v = G.V.create cstruct.name in 
                G.add_vertex g v) components;

        let _,tbridges,_ = collect_type_program Atom.Set.empty 
            (function | CType {value=TBridge _} -> true | _ -> false)
            (fun parent_opt env {value=CType {value=TBridge tbridge}} -> [tbridge])
            program
        in
        let tbridges : tbridge list = Utils.deduplicate [%hash: tbridge] tbridges in

        (* Create possible edges from tbridge *)
        List.iter generate_possible_edges_from_tbridge tbridges;

        (* Add subclass when represents inline component *)
        let cls = List.map (function (cstruct:component_structure) -> 
            List.filter_map (map0_place (transparent0_plgannot(fun _ -> function 
                | Term {value={v=Class cl}} -> 
                    let re_is_inlined = Str.regexp {|Inline_[A-Z][a-zA-Z0-9_]*[0-9]+_in_[A-Z][a-zA-Z0-9_]*[0-9]+_$|} in
                    if Str.string_match re_is_inlined (Atom.hint cl.name) 0 then (
                        G.add_vertex g (G.V.create cl.name);
                        Hashtbl.add clinline2component cl.name cstruct.name;
                        Some cl 
                    )else None
                | _ -> None))) cstruct.body
        ) components in
        
        (* Add ports *)
        let inports = List.map (function (cstruct:component_structure) -> 
            List.filter_map (map0_place (transparent0_plgannot(fun _ -> function | Inport p -> 
                Hashtbl.add port2component (fst p.value).name cstruct.name;
                Some (fst p.value) | _ -> None))) cstruct.body
        ) components in
        List.iter (function ps -> 
            List.iter (function (p:_port) ->
            let v = G.V.create p.name in 
                G.add_vertex g v) ps
            ) inports;

        let outports = List.map (function (cstruct:component_structure) -> 
            List.filter_map (map0_place (transparent0_plgannot(fun _ -> function | Outport p -> 
                Hashtbl.add port2component (fst p.value).name cstruct.name;
                Some (fst p.value) | _ -> None))) cstruct.body
        ) components in
        List.iter (function ps -> 
            List.iter (function (p:_outport) ->
            let v = G.V.create p.name in 
                G.add_vertex g v) ps
            ) outports;

        let eports = List.map (function (cstruct:component_structure) -> 
            List.filter_map (map0_place (transparent0_plgannot(fun _ -> function | Eport p -> 
                Hashtbl.add port2component (fst p.value).name cstruct.name;
                Some (fst p.value) | _ -> None))) cstruct.body
        ) components in
        List.iter (function ps -> 
            List.iter (function (p:_eport) ->
            let v = G.V.create p.name in 
                G.add_vertex g v) ps
            ) eports;

        let export_path = Fpath.to_string (Fpath.add_seg export_dir ("sltopology_"^name^".dot")) in
        let out = open_out export_path in 
        DotExport.output_graph out g; 
        close_out out 
        (* DotExport.fprint_graph Format.std_formatter g *)
end