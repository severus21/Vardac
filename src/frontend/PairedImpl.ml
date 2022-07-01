open Core 
open Error
open Builtin
open Fieldslib
open Easy_logging
open AstUtils
open IR.IRUtils
 

(* The source calculus. *)
module S1 = Impl
module S2 = IR
(* The target calculus. *)
module T = IRI 

let key_to_string key = List.fold_left (fun acc x -> if acc <> "" then acc^"::"^x else x) "" key

module SeenSet = Set.Make(struct 
    type t = string list 
    let compare = List.compare String.compare
end);;

let check_seen_all seen_set htbl : unit =
    Hashtbl.iter (fun key (value: 'a AstUtils.placed) -> 
        match (SeenSet.find_opt key seen_set) with
        | Some _ -> ()
        | None -> Error.perror value.place "%s is not defined in the spec" (key_to_string key)
    ) htbl

let functions_seen = ref SeenSet.empty
let mark_function key = 
    functions_seen := SeenSet.add key !functions_seen
let function_impls : (string list, S1.function_impl AstUtils.placed) Hashtbl.t = Hashtbl.create 256
let methods_seen = ref SeenSet.empty
let mark_method key = 
    methods_seen := SeenSet.add key !methods_seen
let method_impls : (string list, S1.method_impl AstUtils.placed) Hashtbl.t = Hashtbl.create 256
let states_seen = ref SeenSet.empty
let mark_state key = 
    states_seen := SeenSet.add key !states_seen
let state_impls : (string list, S1.state_impl  AstUtils.placed) Hashtbl.t = Hashtbl.create 256

let componentheaders_impls : (string list, S1.blackbox_term) Hashtbl.t = Hashtbl.create 256
let types_seen = ref SeenSet.empty
let mark_type key = 
    types_seen := SeenSet.add key !types_seen
let type_impls : (string list, S1.type_impl  AstUtils.placed) Hashtbl.t = Hashtbl.create 256

let name2key : (Atom.atom, string list) Hashtbl.t = Hashtbl.create 16
let component2target : (string list, string) Hashtbl.t = Hashtbl.create 256
let target2headers : (string, IRI.blackbox_term list) Hashtbl.t = Hashtbl.create 256
let target2dependencies : (string, IRI.blackbox_term list) Hashtbl.t = Hashtbl.create 256
let plgannotations = Hashtbl.create 256


let show_htblimpls htbl pp_value = 
    Printf.fprintf stdout "Htbl has %d entries\n" (Hashtbl.length htbl);
    Hashtbl.iter (fun key value ->
        Printf.fprintf stdout "- entry %s -> %a\n" (key_to_string key) pp_value value
    ) htbl 


module type ArgSig = sig
    val sealed_envs : (Atom.t, Cook.env) Hashtbl.t 
    val gamma : Cook.gamma_t
    val gamma_types : Cook.gamma_t
    val eliminline_env: (Atom.atom, ((Atom.atom, Atom.atom * Atom.atom) Hashtbl.t)) Hashtbl.t
end

module Make (Arg: ArgSig) = struct
    let logger = Core.Utils.make_log_of "PairedImpl" 


    let sealed_envs = Arg.sealed_envs
    let eliminline_env = Arg.eliminline_env

    module Cook = Cook.Make(struct 
        let _places = [] 
        let gamma = Arg.gamma
        let gamma_types = Arg.gamma_types
    end)

    (************************************ Pass 1 *****************************)
    (*
    at the end of pass 1 :
    method_impls
        Component1::Component12::..Component1n::methodA
        Component1::Component12::..Component1n::methodB
        Component2::Component22::..Component1m::methodC
    state_impls and type_impls same structure 
    *)


    (* TODO
    FIXME in this pass we do not take scope into account only parenship relation
    FIXME we are only using name to pair entities with implm (not signatures)
    *)

    let rec scan_component_item_impl parents ({place; value={plg_annotations; v}} : S1.component_item_impl) : unit = 
        match v with
        | S1.MethodImpl m -> 
            Hashtbl.add plgannotations (parents@m.name) plg_annotations;
            Hashtbl.add method_impls (parents@m.name) {place; value=m}
        | S1.StateImpl s ->  
            Hashtbl.add plgannotations (parents@s.name) plg_annotations;
            Hashtbl.add state_impls (parents@s.name) {place; value=s}
        | S1.ComponentHeadersImpl s -> 
            assert(plg_annotations = []);
            Hashtbl.add componentheaders_impls parents s

    and scan_term parents ({place; value = {plg_annotations; v}} : S1.term) = 
        match v with
        | S1.ComponentImpl c -> 
            Hashtbl.add plgannotations (parents@c.name) plg_annotations;
            Hashtbl.add component2target (parents@c.name) c.target;
            List.iter (scan_component_item_impl (parents@c.name)) c.body
        | S1.TypeImpl tdef -> 
            Hashtbl.add plgannotations (parents@tdef.name) plg_annotations;
            Hashtbl.add type_impls (parents@tdef.name) {place; value = tdef}
        | S1.FunctionImpl fdef -> 
            Hashtbl.add plgannotations (parents@fdef.name) plg_annotations;
            Hashtbl.add function_impls (parents@fdef.name) {place; value = fdef}
        | S1.HeadersImpl h | S1.DependenciesImpl h -> 
            let aux = map_place (fun place {S1.language; body} ->
                {
                    T.language = language;
                    body = List.map (function 
                        | S1.Text t -> T.Text t
                        | S1.Varda e -> Error.perror e.place "varda expression can not be embedded into headers definitions"
                    ) body
                })
            in

            match v with
            | S1.HeadersImpl _ -> begin
                match Hashtbl.find_opt target2headers h.target with
                | None -> Hashtbl.add target2headers h.target [aux h.body] 
                | Some headers -> Hashtbl.replace target2headers h.target (aux h.body :: headers) 
            end
            | S1.DependenciesImpl _ -> begin
                match Hashtbl.find_opt target2dependencies h.target with
                | None -> Hashtbl.add target2dependencies h.target [aux h.body] 
                | Some dependencies -> Hashtbl.replace target2dependencies h.target (aux h.body :: dependencies) 
            end
    let scan_program terms =    
        List.iter (scan_term []) terms  

    (************************************ Pass 2 *****************************)

    (* 
        @param name - name of the implemented construct (e.g., name of the method)
    *)
    let cbb_term name place {S1.language; body} =
            (* cook bb_term using the env where the implemented construct (e.g., method) lives *)
            let env = 
                try
                    Hashtbl.find sealed_envs name 
                with Not_found -> raise (Error.PlacedDeadbranchError (
                    place, 
                    Printf.sprintf "[%s] not found in sealed_envs" (Atom.to_string name)))
            in
            
            {
                T.language = language;
                body = List.map (function
                    | S1.Text t -> T.Text t 
                    | S1.Varda e -> T.Varda (Cook.cook_expression_env env e)
                ) body
            }
    let cook_bb_term name : S1.blackbox_term -> T.blackbox_term = map_place (cbb_term name)

    let rec paired_state parents place : S2._state -> plg_annotation list * T._state = function
    | { ghost; type0; name; body=None} as s-> begin
        try 
            let key = List.rev ((Atom.hint name)::parents) in 
            mark_state key;
            let s_impl = 
                try Hashtbl.find state_impls key 
                with Not_found -> 
                    raise (Error.PlacedDeadbranchError(
                        place, 
                        Printf.sprintf "impl for state [%s] not found in state_impls" (List.fold_left (fun x y -> x ^"::"^y) "" key)))
            in
            let bb_impl = cook_bb_term name s_impl.value.body in
            let plg_annots = 
                match Hashtbl.find_opt plgannotations key with
                | None -> [] 
                | Some xs -> xs
            in

            plg_annots, { ghost; type0; name; body= T.InitBB bb_impl}
        with Not_found -> 
            logger#warning "State %s is not initialized (neither abstract nor blackbox)" (Atom.to_string name);
            [], { ghost; type0; name; body=NoInit}
    end
    | { ghost; type0; name; body=Some body} -> begin
        try 
            let bb_impl = Hashtbl.find state_impls (List.rev ((Atom.hint name)::parents))  in
            Error.perror (place@bb_impl.place) "State has two implementations : one abstract and one blackbox"
        with Not_found -> [], { ghost; type0; name; body= T.InitExpr body } 
    end
    and ustate parents : IR.state -> plg_annotation list * T.state = map2_place (paired_state parents)

    and paired_method0 parents place : S2._method0 -> plg_annotation list * T._method0 = function
    |{ annotations; ghost; ret_type; name; args; body=[]; contract_opt; on_destroy; on_startup} -> begin
        let key = List.rev ((Atom.hint name)::parents) in 
        let plg_annots = match Hashtbl.find_opt plgannotations key with | None -> [] |Some l -> l in
        mark_method key;

        match Hashtbl.find_opt method_impls key with
        | Some m_impl when m_impl.value.body <> None -> 
            let bb_impl = cook_bb_term name (Option.get m_impl.value.body) in

            plg_annots, { annotations; ghost; ret_type; name; args; body= T.BBImpl bb_impl; contract_opt; on_destroy; on_startup }
        | _ -> begin
            (* Methods that accept an empty body *)
            match ret_type with
            | {value=S2.CType{value=S2.TFlatType AstUtils.TVoid;}} ->
                plg_annots, { annotations; ghost; ret_type; name; args; body= T.AbstractImpl []; contract_opt; on_destroy; on_startup } 
            | _ when on_destroy || on_startup -> 
                plg_annots, { annotations; ghost; ret_type; name; args; body= T.AbstractImpl []; contract_opt; on_destroy; on_startup } 
            | _ -> Error.perror place "Method \"%s\" has no implementation (neither abstract nor blackbox)" (Atom.hint name) 
        end
    end
    |{ annotations; ghost; ret_type; name; args; body= body; contract_opt; on_destroy; on_startup} -> begin 
        let key = List.rev ((Atom.hint name)::parents) in 
        let plg_annots = match Hashtbl.find_opt plgannotations key with | None -> [] |Some l -> l in
        mark_method key;

        match Hashtbl.find_opt method_impls key with
        | Some m_impl when m_impl.value.body <> None -> 
            Error.perror (place@m_impl.place) "Method has two implementations : one abstract and one blackbox"
        | _ -> plg_annots, { annotations; ghost; ret_type; name; args; body= T.AbstractImpl body; contract_opt; on_destroy; on_startup }
    end
    and umethod0 parents: S2.method0 -> plg_annotation list * T.method0 = map2_place (paired_method0 parents)

    and paired_component_item parents place {v}: T._component_item plg_annotated= 
    match v with
    | S2.Contract c -> auto_plgannot (T.Contract c)
    | S2.Include _  -> failwith "paired: component include not suported yet"
    | S2.Method m   -> 
        let plg_annotations, m = umethod0 parents m in 
        {plg_annotations; v=T.Method m}
    | S2.Inport p   -> auto_plgannot(T.Inport p)
    | S2.Eport p    -> auto_plgannot(T.Eport p)
    | S2.Outport p  -> auto_plgannot(T.Outport p)
    | S2.State s    -> 
        let plg_annotations, s = ustate parents s in 
        {plg_annotations; v= T.State s} 
    | S2.Term t     -> auto_plgannot(T.Term (uterm parents t))
    and ucitem parents: S2.component_item -> T.component_item  = map_place (paired_component_item parents)

    and paired_class_item parents place {v}: T._class_item plg_annotated= 
    match v with
    | S2.CLMethod m   -> 
        let plg_annotations, m = umethod0 parents m in 
        {plg_annotations; v=T.CLMethod m}
    | S2.CLState s    -> 
        let plg_annotations, s = ustate parents s in 
        {plg_annotations; v= T.CLState s} 
    and uclitem parents: S2.class_item -> T.class_item  = map_place (paired_class_item parents)

    and paired_component_dcl parents place : S2._component_dcl ->  plg_annotation list * T._component_dcl = function
    | S2.ComponentStructure {target_name; name; annotations; body; headers} -> begin 
        let key = match target_name with 
            | UserDefined -> begin
                match Hashtbl.find_opt name2key name with
                | None -> failwith (Printf.sprintf "[%s] not found in name2key" (Atom.to_string name))
                | Some key -> key
            end
            | SameAs as_name -> begin
                match Hashtbl.find_opt name2key as_name with
                | None -> failwith (Printf.sprintf "[%s].target = SameAs [%s].target, [%s].target can not be a SameAs indirection" (Atom.to_string name) (Atom.to_string as_name) (Atom.to_string as_name))
                | Some key -> key
            end
        in
        let plg_annots = 
            match Hashtbl.find_opt plgannotations key with
            | None -> []
            | Some xs -> xs
        in

        let target_name = 
            match Hashtbl.find_opt component2target key with
            | None ->
                raise (Error.PlacedDeadbranchError (place, Printf.sprintf "A target should have been assign to component [%s]." (List.fold_left (fun x y -> if x <> "" then x^"::"^y else y) "" key)))
            | Some target -> target
        in
        let body = List.map (ucitem ((Atom.hint name)::parents)) body in
        plg_annots, T.ComponentStructure {
            target_name; 
            annotations; 
            name; 
            body; 
            headers =  
                match Hashtbl.find_opt componentheaders_impls (List.rev ((Atom.hint name)::parents)) with
                | None -> []
                | Some {place; value={body=headers}} -> 
                    List.map (function
                        | S1.Text t -> t 
                        | S1.Varda _ -> Error.perror place "headers can not contain Varda templating code") 
                    headers
        }
    end
    | S2.ComponentAssign {name; value} -> 
        [], (T.ComponentAssign {name; value})
    and ccdcl parents: S2.component_dcl ->  plg_annotation list * T.component_dcl = map2_place (paired_component_dcl parents)

    and ccl (parents:string list) (cl:S2.class_structure) : T.class_structure =
        (* If class comes from InlineElim, origins <> empty *)
        let origins = Common.InlineElim.cl2c_get_origins eliminline_env cl.name in

        if origins <> Atom.Set.empty then
        begin
            let parent = 
                try List.nth parents ((List.length parents)-1) 
                with Not_found -> raise (Error.DeadbranchError "An inlined class generated during InlineElim, MUST always be defined inside a component!")
            in
            let parent_key = List.rev parents in
            let parent_target_name = 
                match Hashtbl.find_opt component2target parent_key with 
                | None -> 
                    Error.error "No target found for parent_key=[%s]" (key_to_string parent_key)
                | Some x -> x
            in

            Atom.Set.iter (function origin -> 
                let origin_key = 
                    match Hashtbl.find_opt name2key origin with
                    | None ->
                        Error.error "No key found for origin=[%s]" (Atom.to_string origin)
                    | Some key -> key
                in
                let origin_target_name = 
                    match Hashtbl.find_opt component2target origin_key with 
                    | None -> 
                        Error.error "No target found for origin_key=[%s]" (key_to_string origin_key)
                    | Some x -> x
                in
                (* Assert Origin.target = Parent.target *)
                if origin_target_name <> parent_target_name then
                    Error.error 
                        "Targets mismatched for inlined class [%s]<%s> in component [%s]<%s>" 
                        (Atom.to_string cl.name)
                        origin_target_name 
                        parent
                        parent_target_name
            ) origins;
        end;

        (* All cases *)
        {
            T.annotations = cl.annotations;
            name = cl.name;
            body = List.map (uclitem parents) cl.body; 
        }

    and paired_function_dcl parents place : S2._function_dcl ->  plg_annotation list * T._function_dcl = function
    | {ret_type; targs; name; args; body=[] } -> begin
        try 
            let key = List.rev ((Atom.hint name)::parents) in 
            mark_function key;
            let f_impl = Hashtbl.find function_impls key in
            let bb_impl = cook_bb_term name f_impl.value.body in
            let plg_annots = Hashtbl.find plgannotations key in

            plg_annots, { ret_type; targs; name; args; body= T.BBImpl bb_impl }
        with Not_found -> Error.perror place "Function \"%s\" has no implementation (neither abstract nor blackbox)" (Atom.hint name) 
    end
    | { ret_type; name; targs; args; body= body } -> begin 
        try 
            let key = List.rev ((Atom.hint name)::parents) in 
            mark_function key;
            let bb_impl = Hashtbl.find function_impls key in
            Error.perror (place@bb_impl.place) "Function has two implementations : one abstract and one blackbox"
        with | Not_found -> [], { ret_type; targs; name; args; body= T.AbstractImpl body }
    end
    and ufunction_dcl parents: S2.function_dcl -> plg_annotation list * T.function_dcl = map2_place (paired_function_dcl parents)

    and paired_term parents place {v} : T._term plg_annotated = 
    match v with
    | S2.EmptyTerm -> auto_plgannot (T.EmptyTerm)
    | S2.Comments c -> auto_plgannot (T.Comments c)
    | S2.Component c -> 
        let plg_annotations, c = ccdcl parents c in
        {plg_annotations; v = T.Component c }
    | S2.Class cl -> 
        auto_plgannot (T.Class (ccl parents cl))
    | S2.Stmt stmt -> auto_plgannot (T.Stmt stmt)
    | S2.Function f  -> 
        let plg_annotations, f = ufunction_dcl parents f in
        {plg_annotations; v=T.Function f}
    | S2.Typealias (x, None) -> begin
        try 
            let key = List.rev ((Atom.hint x)::parents) in
            mark_type key;
            let t_impl = Hashtbl.find type_impls key in
            let bb_impl = cook_bb_term x t_impl.value.body in
            let plg_annots = Hashtbl.find plgannotations key in

            {plg_annotations = plg_annots; v=T.Typealias (x, BBTypealias bb_impl)}
        with Not_found -> Error.perror place "Typealias has no implementation (neither abstract nor blackbox)" 
    end
    (* FIXME TODO we do not distinguish between classical type and event when checkin gsould be done by addint an event_impls*)
    | S2.Typealias (x, Some mt) -> begin
        try 
            let key = List.rev ((Atom.hint x)::parents) in
            let bb_impl = Hashtbl.find type_impls key in
            let plg_annots = Hashtbl.find plgannotations key in
            Error.perror (place@bb_impl.place) "Type alias has two implementations : one abstract and one blackbox"
        with Not_found -> auto_plgannot(T.Typealias (x, AbstractTypealias mt))
    end
    | S2.Typedef {value=ClassicalDef(x, args, ()); place} -> begin
        try 
            let key = List.rev ((Atom.hint x)::parents) in
            let t_impl = Hashtbl.find type_impls key in
            let bb_impl = cook_bb_term x t_impl.value.body in
            let plg_annots = Hashtbl.find plgannotations key in

            mark_type key;
            { plg_annotations = plg_annots; v=T.Typedef { 
                place; 
                value = ClassicalDef (x, args, Some bb_impl) 
            }}
        with Not_found -> auto_plgannot(T.Typedef { 
            place; 
            value = ClassicalDef (x, args, None) 
        })  (* Implict constructor *)
    end
    | S2.Typedef {value=EventDef(x, args, ()); place} -> begin
        try 
            let key = List.rev ((Atom.hint x)::parents) in
            let t_impl = Hashtbl.find type_impls key in
            let bb_impl = cook_bb_term x t_impl.value.body in
            let plg_annots = Hashtbl.find plgannotations key in

            mark_type key;
            Error.perror (place@bb_impl.place) "an event can not have a blackbox implementation yet"
        with Not_found -> auto_plgannot(T.Typedef { 
            place; 
            value = EventDef (x, args, None) 
          })  (* Implict constructor *)
    end
    | S2.Typedef {value=ProtocolDef(x, mt); place} -> begin
        try 
            let key = List.rev ((Atom.hint x)::parents) in
            let bb_impl = Hashtbl.find type_impls key in
            let plg_annots = Hashtbl.find plgannotations key in
            mark_type key;
            Error.perror (place@bb_impl.place) "a protocol can not have a blackbox implementation yet"
        with Not_found -> auto_plgannot(T.Typedef { 
            place; 
            value = ProtocolDef (x, mt) 
        })  (* Implict constructor *)
    end
    | S2.Typedef {value=VPlaceDef x; place} -> begin
        try 
            let key = List.rev ((Atom.hint x)::parents) in
            let bb_impl = Hashtbl.find type_impls key in
            let plg_annots = Hashtbl.find plgannotations key in
            mark_type key;
            Error.perror (place@bb_impl.place) "a vplace can not have a blackbox implementation yet"
        with Not_found -> auto_plgannot(T.Typedef { 
            place; 
            value = VPlaceDef x 
        })  (* Implict constructor *)
    end
    | S2.Derive {name; cargs; targs; eargs; } -> auto_plgannot(T.Derive {name; cargs; targs; eargs})
    and uterm parents: S2.term -> T.term = map_place (paired_term parents)

    let paired_program targets terms impl_terms =    
        (* Pass 1 *)
        scan_program impl_terms;
        (* Pass 2 *)

        (*TODO to support multiple SameAs indirection - loop until all SameAs have been seen + cycle detection *)
        collect_term_program
            true
            (function | Component {value = S2.ComponentStructure _ } -> true | _ -> false)
            (fun parents place -> function 
                | Component { value = S2.ComponentStructure {target_name; name}} -> 
                    let key = List.rev ((Atom.hint name)::(List.map Atom.hint parents)) in
                    logger#debug "%s <-> %s" (Atom.to_string name) (List.fold_left (fun x y -> x^"::"^y)"" key);
                    Hashtbl.add name2key name key;
                    []
                | _ -> raise (DeadbranchError "selector prevents accessing this branch")
            ) terms;

        let program = List.map (uterm []) terms in
        check_seen_all !methods_seen method_impls; 
        check_seen_all !states_seen state_impls; 
        check_seen_all !types_seen type_impls; 
        (target2dependencies, target2headers), program
end