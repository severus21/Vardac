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
    type t = IR.tbridge option 
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
    | TVoid -> "void"
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
    val component2target : (Atom.atom, string) Hashtbl.t
end

module type Sig = sig
    val generate_static_logical_topology : Fpath.t -> IR.program -> unit 
end


module Make (Args:Params) : Sig = struct
    include Args

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
            let target = 
                try
                    Hashtbl.find component2target v 
                with Not_found -> raise (Error.DeadbranchError (Printf.sprintf "component [%s] not found in component2target" (Atom.to_string v)))
            in

            Some {
                sg_name = target;
                sg_attributes = [
                    `HtmlLabel target;
                    `Style `Bold
                ];
                sg_parent = None; (* Since we are only spliting in a per target basis *)    
            } 
        let vertex_attributes _ = [`Shape `Box]
        let vertex_name v = Atom.hint v 
        let default_vertex_attributes _ = []
        let graph_attributes _ = []
    end

    module DotExport = Graphviz.Dot(GPrinter)
    let g = G.create ()

    let rec 
    generate_slt_ctype { AstUtils.place ; AstUtils.value}= match value with
    | TActivationRef _ | TVar _ | TVPlace _ -> () 
    | TArrow (mt1, mt2) | TDict (mt1, mt2) | TResult (mt1, mt2)-> generate_slt_mtype mt1; generate_slt_mtype mt2
    | TFlatType _ -> ()
    | TList mt | TOption mt | TSet mt -> generate_slt_mtype mt
    | TTuple mts -> List.iter generate_slt_mtype mts
    | TBridge {in_type; out_type; protocol} ->
        let rec explore error_header { AstUtils.place ; AstUtils.value} =
            match value with
            | CType {value=TVar x} -> [x] 
            | CType {value=TUnion (nt1, nt2)} -> (explore error_header nt1) @(explore error_header nt2) 
            | CompType {value=CompTUid cname} -> [cname] 
            | _ -> Error.error (out_type.place) "bridge %s type parameter must be a component type (name, union, universal)" error_header
        in
        let lefts = explore "fst" in_type in 
        let rights = explore "snd" out_type in 
        
        List.iter (function left ->
            (List.iter 
                (function right -> G.add_edge_e g (G.E.create left (Some {in_type; out_type; protocol}) right) )
                rights
            )
        ) lefts

    
    and generate_slt_mtype { AstUtils.place ; AstUtils.value}= match value with
    | CType ct -> generate_slt_ctype ct 
    | SType _ -> ()
    | CompType _ -> ()
    | ConstrainedType (mt,_) -> generate_slt_mtype mt

    (* TODO bridge could hidden inside lambda body therefore we need to unfold expr *)

    and generate_slt_stmt { AstUtils.place ; AstUtils.value}= match value with
    | LetStmt (mt,_,_) -> generate_slt_mtype mt
    | ForStmt (_,_,_,stmt) -> generate_slt_stmt stmt
    | IfStmt (_, stmt, None) -> generate_slt_stmt stmt
    | IfStmt (_, stmt1, Some stmt2) -> generate_slt_stmt stmt1; generate_slt_stmt stmt2
    | MatchStmt (_, branches) -> List.iter (function (_,stmt) -> generate_slt_stmt stmt) branches
    | BlockStmt stmts -> List.iter generate_slt_stmt stmts
    | GhostStmt stmt -> generate_slt_stmt stmt
    | _ -> ()

    and generate_slt_method ({ AstUtils.place ; AstUtils.value} : method0) = match value with
    | {ret_type; body} ->
        generate_slt_mtype ret_type;
        List.iter generate_slt_stmt body

    and generate_slt_state { AstUtils.place ; AstUtils.value}= match value with
    | StateDcl {type0;body=None} ->
        generate_slt_mtype type0
    | StateDcl {type0;body=Some _} -> generate_slt_mtype type0
    and generate_slt_citem { AstUtils.place ; AstUtils.value}= match value with
    |Inport p -> () 
    | Method m -> generate_slt_method m 
    | State s -> generate_slt_state s
    | Term t -> generate_slt_term t
    | _ -> () (*Nothing todo *)
    and generate_slt_cdcl { AstUtils.place ; AstUtils.value}= match value with
    | ComponentStructure {name; body; _} ->
        let v = G.V.create name in 
            G.add_vertex g v; 
    | _ -> failwith "TODO topology"

    and generate_slt_term { AstUtils.place ; AstUtils.value}= match value with
    (* Search for Vertices *)
    | Component cdcl ->
        generate_slt_cdcl cdcl
    (* Search for Edges *)
    | Stmt stmt -> generate_slt_stmt stmt

    | Typealias (x, (Some mt)) -> 
        generate_slt_mtype mt
        (* N.B. Typedef not not have any body before pairing with impl *)
    | _ -> () (* nothing todo *)
    let generate_static_logical_topology export_dir program =
        List.iter generate_slt_term program;

        let export_path = Fpath.to_string (Fpath.add_seg export_dir "sltopology.dot") in
        let out = open_out export_path in 
        DotExport.output_graph out g; 
        close_out out 
        (* DotExport.fprint_graph Format.std_formatter g *)
end