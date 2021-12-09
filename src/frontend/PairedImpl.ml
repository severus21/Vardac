open Core 
open Core.Error
open Core.Builtin
open Fieldslib
open Easy_logging
open AstUtils
let logger = Logging.make_logger "_1_ compspec.frontend" Debug [];;

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
        | None -> Error.error value.place "%s is not defined in the spec" (key_to_string key)
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
let types_seen = ref SeenSet.empty
let mark_type key = 
    types_seen := SeenSet.add key !types_seen
let type_impls : (string list, S1.type_impl  AstUtils.placed) Hashtbl.t = Hashtbl.create 256

let component2target : (string list, string) Hashtbl.t = Hashtbl.create 256

let show_htblimpls htbl = 
    Printf.fprintf stdout "Htbl has %d entries\n" (Hashtbl.length htbl);
    Hashtbl.iter (fun key _ ->
        Printf.fprintf stdout "- entry %s\n" (key_to_string key)
    ) htbl 


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

let rec scan_component_item_impl parents ({place; value} : S1.component_item_impl) : unit = 
match value with
| S1.MethodImpl m -> Hashtbl.add method_impls (parents@m.name) {place; value=m}
| S1.StateImpl s ->  Hashtbl.add state_impls (parents@s.name) {place; value=s}

and scan_term parents ({place; value} : S1.term) = 
match value with
| S1.ComponentImpl c -> 
    Hashtbl.add component2target (parents@c.name) c.target;
    List.iter (scan_component_item_impl (parents@c.name)) c.body
| S1.TypeImpl tdef -> Hashtbl.add type_impls (parents@tdef.name) {place; value = tdef}
| S1.FunctionImpl fdef -> Hashtbl.add function_impls (parents@fdef.name) {place; value = fdef}

let scan_program terms =    
    List.iter (scan_term []) terms  

(************************************ Pass 2 *****************************)

let rec paired_state parents place : S2._state -> T._state = function
| S2.StateDcl { ghost; type0; name; body=None} as s-> begin
    try 
        let key = List.rev ((Atom.hint name)::parents) in 
        mark_state key;
        let bb_impl = Hashtbl.find state_impls key in

        T.StateDcl { ghost; type0; name; body= T.InitBB bb_impl.value.body}
    with Not_found -> 
        logger#warning "State %s is not initialized (neither abstract nor blackbox)" (Atom.to_string name);
        T.StateDcl { ghost; type0; name; body=NoInit}
end
| S2.StateDcl { ghost; type0; name; body=Some body} -> begin
    try 
        let bb_impl = Hashtbl.find state_impls (List.rev ((Atom.hint name)::parents))  in
        Error.error (place@bb_impl.place) "State has two implementations : one abstract and one blackbox"
    with Not_found -> T.StateDcl { ghost; type0; name; body= T.InitExpr body } 
end
| S2.StateAlias _ -> failwith "paired: state alias not yet supported" (*TODO*)
and ustate parents : IR.state -> T.state = map_place (paired_state parents)

and paired_method0 parents place : S2._method0 -> T._method0 = function
|{ ghost; ret_type; name; args; body=[]; contract_opt; on_destroy; on_startup} -> begin
    try 
        let key = List.rev ((Atom.hint name)::parents) in 
        mark_method key;
        let bb_impl = Hashtbl.find method_impls key in
        { ghost; ret_type; name; args; body= T.BBImpl bb_impl.value.body; contract_opt; on_destroy; on_startup }
    with Not_found -> begin
        (* Methods that accept an empty body *)
        match ret_type with
        | {value=S2.CType{value=S2.TFlatType AstUtils.TVoid;}} ->
            { ghost; ret_type; name; args; body= T.AbstractImpl []; contract_opt; on_destroy; on_startup } 
        | _ when on_destroy || on_startup -> 
            { ghost; ret_type; name; args; body= T.AbstractImpl []; contract_opt; on_destroy; on_startup } 
        | _ -> Error.error place "Method \"%s\" has no implementation (neither abstract nor blackbox)" (Atom.hint name) 
    end
end
|{ ghost; ret_type; name; args; body= body; contract_opt; on_destroy; on_startup} -> begin 
    try 
        let key = List.rev ((Atom.hint name)::parents) in 
        mark_method key;
        let bb_impl = Hashtbl.find method_impls key in
        Error.error (place@bb_impl.place) "Method has two implementations : one abstract and one blackbox"
    with | Not_found -> { ghost; ret_type; name; args; body= T.AbstractImpl body; contract_opt; on_destroy; on_startup }
end
and umethod0 parents: S2.method0 -> T.method0 = map_place (paired_method0 parents)

and paired_component_item parents place : S2._component_item -> T._component_item = function
| S2.Contract c -> T.Contract c
| S2.Include _ -> failwith "paired: component include not suported yet"
| S2.Method m -> T.Method (umethod0 parents m)
| S2.Port p -> T.Port p
| S2.State s -> T.State (ustate parents s)
| S2.Term t -> T.Term (uterm parents t)
and ucitem parents: S2.component_item -> T.component_item  = map_place (paired_component_item parents)

and paired_component_dcl parents place : S2._component_dcl -> T._component_dcl = function
| S2.ComponentStructure {name; annotations; args; body} -> begin 
    try 
        let key = List.rev ((Atom.hint name)::parents) in 
        let target_name = Hashtbl.find component2target key in
        let body = List.map (ucitem ((Atom.hint name)::parents)) body in
        T.ComponentStructure {target_name; annotations; name; args; body}
    with | Not_found -> raise (Error.PlacedDeadbranchError (place, "A target should have been assign to each component"))
end
| S2.ComponentAssign {name; args; value} -> T.ComponentAssign {name; args; value} 
and ccdcl parents: S2.component_dcl -> T.component_dcl = map_place (paired_component_dcl parents)

and paired_function_dcl parents place : S2._function_dcl -> T._function_dcl = function
| {ret_type; targs; name; args; body=[] } -> begin
    try 
        let key = List.rev ((Atom.hint name)::parents) in 
        mark_function key;
        let bb_impl = Hashtbl.find function_impls key in
        { ret_type; targs; name; args; body= T.BBImpl bb_impl.value.body }
    with Not_found -> Error.error place "Function \"%s\" has no implementation (neither abstract nor blackbox)" (Atom.hint name) 
end
| { ret_type; name; targs; args; body= body } -> begin 
    try 
        let key = List.rev ((Atom.hint name)::parents) in 
        mark_function key;
        let bb_impl = Hashtbl.find function_impls key in
        Error.error (place@bb_impl.place) "Function has two implementations : one abstract and one blackbox"
    with | Not_found -> { ret_type; targs; name; args; body= T.AbstractImpl body }
end
and ufunction_dcl parents: S2.function_dcl -> T.function_dcl = map_place (paired_function_dcl parents)

and paired_term parents place : S2._term -> T._term = function
| S2.EmptyTerm -> T.EmptyTerm
| S2.Comments c -> T.Comments c
| S2.Component c -> T.Component (ccdcl parents c)
| S2.Stmt stmt -> T.Stmt stmt
| S2.Function f  -> T.Function (ufunction_dcl parents f)
| S2.Typealias (x, None) -> begin
    try 
        let key = List.rev ((Atom.hint x)::parents) in
        mark_type key;
        let bb_impl = Hashtbl.find type_impls key in
        T.Typealias (x, BBTypealias bb_impl.value.body) 
    with Not_found -> Error.error place "Typealias has no implementation (neither abstract nor blackbox)" 
end
(* FIXME TODO we do not distinguish between classical type and event when checkin gsould be done by addint an event_impls*)
| S2.Typealias (x, Some mt) -> begin
    try 
        let key = List.rev ((Atom.hint x)::parents) in
        let bb_impl = Hashtbl.find type_impls key in
        Error.error (place@bb_impl.place) "Type alias has two implementations : one abstract and one blackbox"
    with Not_found -> T.Typealias (x, AbstractTypealias mt) 
end
| S2.Typedef {value=ClassicalDef(x, args, ()); place} -> begin
    try 
        let key = List.rev ((Atom.hint x)::parents) in
        let bb_impl = Hashtbl.find type_impls key in
        mark_type key;
        T.Typedef { 
            place; 
            value = ClassicalDef (x, args, Some bb_impl.value.body) 
        }
    with Not_found -> T.Typedef { 
        place; 
        value = ClassicalDef (x, args, None) 
    }  (* Implict constructor *)
end
| S2.Typedef {value=EventDef(x, args, ()); place} -> begin
    try 
        let key = List.rev ((Atom.hint x)::parents) in
        let bb_impl = Hashtbl.find type_impls key in
        mark_type key;
        Error.error (place@bb_impl.place) "an event can not have a blackbox implementation yet"
    with Not_found -> T.Typedef { 
        place; 
        value = EventDef (x, args, None) 
    }  (* Implict constructor *)
end
| S2.Typedef {value=ProtocolDef(x, mt); place} -> begin
    try 
        let key = List.rev ((Atom.hint x)::parents) in
        let bb_impl = Hashtbl.find type_impls key in
        mark_type key;
        Error.error (place@bb_impl.place) "a protocol can not have a blackbox implementation yet"
    with Not_found -> T.Typedef { 
        place; 
        value = ProtocolDef (x, mt) 
    }  (* Implict constructor *)
end
| S2.Derive {name; cargs; targs; eargs; } -> T.Derive {name; cargs; targs; eargs}
and uterm parents: S2.term -> T.term = map_place (paired_term parents)

let paired_program targets terms impl_terms =    
    (* Pass 1 *)
    scan_program impl_terms;
    (* Pass 2 *)
    let program = List.map (uterm []) terms in
    check_seen_all !methods_seen method_impls; 
    check_seen_all !states_seen state_impls; 
    check_seen_all !types_seen type_impls; 
    program