open Core 
open Core.Error
open Core.Builtin
open Fieldslib
open Easy_logging
let logger = Logging.make_logger "_1_ compspec.frontend" Debug [Cli Debug];;

(* The source calculus. *)
module S1 = Ast_impl
module S2 = IR
(* The target calculus. *)
module T = IRI 

let method_impls : (string list, S1.method_impl) Hashtbl.t = Hashtbl.create 256
let state_impls : (string list, S1.state_impl) Hashtbl.t = Hashtbl.create 256
let type_impls : (string list, S1.type_impl) Hashtbl.t = Hashtbl.create 256

let show_htblimpls htbl = 
    logger#warning "TOTO";
    Printf.fprintf stdout "Htbl has %d entries\n" (Hashtbl.length htbl);
    Hashtbl.iter (fun key _ ->
        Printf.fprintf stdout "- entry %s\n" (List.fold_left (fun acc x -> if acc <> "" then acc^"::"^x else x) "" key)
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
| S1.MethodImpl m -> Hashtbl.add method_impls (parents@m.name) m
| S1.StateImpl s ->  Hashtbl.add state_impls (parents@s.name) s

and scan_term parents ({place; value} : S1.term) = 
match value with
| S1.ComponentImpl c -> List.iter (scan_component_item_impl (parents@c.name)) c.body
| S1.TypedefImpl tdef -> Hashtbl.add type_impls (parents@tdef.name) tdef

let scan_program terms =    
    List.iter (scan_term []) terms  

(************************************ Pass 2 *****************************)
let rec paired_place paired_value parents ({ Core.AstUtils.place ; Core.AstUtils.value}: 'a Core.AstUtils.placed) = 
    let value = paired_value parents place value in
    {Core.AstUtils.place; Core.AstUtils.value}

let rec paired_state parents place : S2._state -> T._state = function
| S2.StateDcl { ghost; kind; type0; name; body=None} -> begin
    try 
        let bb_impl = Hashtbl.find state_impls (List.rev ((Atom.hint name)::parents))  in
        T.StateDcl { ghost; kind; type0; name; body= T.InitBB bb_impl.body}
    with Not_found -> Error.error place "State has no implementation (neither abstract nor blackbox)" 
end
| S2.StateDcl { ghost; kind; type0; name; body=Some body} -> begin
    try 
        let bb_impl = Hashtbl.find state_impls (List.rev ((Atom.hint name)::parents))  in
        Error.error (place@bb_impl.body.place) "State has two implementations : one abstract and one blackbox"
    with Not_found -> T.StateDcl { ghost; kind; type0; name; body= T.InitExpr body } 
end
| S2.StateAlias _ -> failwith "paired: state alias not yet supported" (*TODO*)
and ustate parents : IR.state -> T.state = paired_place paired_state parents 

and paired_method0 parents place : S2._method0 -> T._method0 = function
| S2.CustomMethod { ghost; ret_type; name; args; body=None; contract_opt} -> begin
    try 
        let bb_impl = Hashtbl.find method_impls (List.rev ((Atom.hint name)::parents)) in
        T.CustomMethod { ghost; ret_type; name; args; body= T.BBImpl bb_impl.body; contract_opt }
    with Not_found -> Error.error place "Method \"%s\" has no implementation (neither abstract nor blackbox)" (Atom.hint name) 
end
| S2.CustomMethod { ghost; ret_type; name; args; body=Some body; contract_opt} -> 
    T.CustomMethod { ghost; ret_type; name; args; body= T.AbstractImpl body; contract_opt }
| S2.OnStartup m -> T.OnStartup (umethod0 parents m) 
| S2.OnDestroy m -> T.OnDestroy (umethod0 parents m)
and umethod0 parents: S2.method0 -> T.method0 = paired_place paired_method0 parents

and paired_component_item parents place : S2._component_item -> T._component_item = function
| S2.Contract c -> T.Contract c
| S2.Include _ -> failwith "paired: component include not suported yet"
| S2.Method m -> T.Method (umethod0 parents m)
| S2.Port p -> T.Port p
| S2.State s -> T.State (ustate parents s)
| S2.Term t -> T.Term (uterm parents t)
and ucitem parents: S2.component_item -> T.component_item  = paired_place paired_component_item parents 

and paired_component_dcl parents place : S2._component_dcl -> T._component_dcl = function
| S2.ComponentStructure {name; args; body} -> 
    let body = List.map (ucitem ((Atom.hint name)::parents)) body in
    T.ComponentStructure {name; args; body}
| S2.ComponentAssign {name; args; value} -> T.ComponentAssign {name; args; value} 
and ccdcl parents: S2.component_dcl -> T.component_dcl = paired_place paired_component_dcl parents 

and paired_term parents place : S2._term -> T._term = function
| S2.Comments c -> T.Comments c
| S2.Component c -> T.Component (ccdcl parents c)
| S2.Stmt stmt -> T.Stmt stmt
| S2.Typedef (x, None) -> begin
    try 
        let bb_impl = Hashtbl.find type_impls (List.rev ((Atom.hint x)::parents)) in
        T.Typedef (x, BBTypedef bb_impl.body) 
    with Not_found -> Error.error place "Typedef has no implementation (neither abstract nor blackbox)" 
end
| S2.Typedef (x, Some mt) -> T.Typedef (x, AbstractTypedef mt)  
and uterm parents: S2.term -> T.term = paired_place paired_term parents 

let paired_program terms impl_terms =    
    (* Pass 1 *)
    scan_program impl_terms;
    show_htblimpls state_impls;
    (* Pass 2 *)
    List.map (uterm []) terms  