open AstUtils

module IRCParams : (AstUtils.IRParams with type Variable.t = Atom.t)= struct
    module Variable = Atom
end 

module IRC  : (IR_common.TIRC with module Variable = Atom.AtomVariable )= IR_common.Make(Atom.AtomVariable)

type ir_target_name = unit 
and ir_state_dcl_body = IRC.expr option 
and ir_custom_method0_body = IRC.stmt list 
and ir_custom_function_body = ir_custom_method0_body 
and ir_typealias_body = IRC.main_type option
and ir_typedef_body = unit
[@@deriving show { with_path = false }]

(*
    IRC et Make dans IR_template sont deux module différent ...
    variable ok les même mais les constructeurs sont différents
*)

module Params : (
    IR_template.IRParams with   
        type target_name = ir_target_name and
        type _state_dcl_body = ir_state_dcl_body and 
        type _custom_method0_body = ir_custom_method0_body and
        type _custom_function_body = ir_custom_function_body and
        type _typealias_body = ir_typealias_body and
        type _typedef_body = ir_typedef_body
) = struct
    module Variable = Atom
    type target_name = ir_target_name 
    and _state_dcl_body = ir_state_dcl_body
    and _custom_method0_body = ir_custom_method0_body
    and _custom_function_body = ir_custom_function_body
    and _typealias_body = ir_typealias_body
    and _typedef_body = ir_typedef_body
    [@@deriving show { with_path = false }]
end

include IR_common
module IR = IR_template.Make(IRC)(Params) 
include IR

let rec free_vars_contract_ (already_binded:Atom.Set.t) place _contract = failwith "TODO FIXME free vars contract" 
and free_vars_contract (already_binded:Atom.Set.t) = map0_place (free_vars_contract_ already_binded) 

and free_vars_port_ (already_binded:Atom.Set.t) place (_port, _) =
    let _, fvars1 = free_vars_expr already_binded _port.input in
    let _, fvars2 = free_vars_mtype already_binded _port.expecting_st in
    let _, fvars3 = free_vars_expr already_binded _port.callback in
    already_binded, fvars1@fvars2@fvars3
and free_vars_port (already_binded:Atom.Set.t) = map0_place (free_vars_port_ already_binded) 

and free_vars_state_ (already_binded:Atom.Set.t) place = function 
| StateDcl sdcl -> 
    let _, fvars1 = free_vars_mtype already_binded sdcl.type0 in
    let _, fvars2 = match sdcl.body with
    | Some e -> free_vars_expr already_binded e
    | None _ -> already_binded, []
    in
    already_binded, fvars1@fvars2
and free_vars_state (already_binded:Atom.Set.t) = map0_place (free_vars_state_ already_binded) 

and free_vars_function_dcl_ (already_binded:Atom.Set.t) place m =
    let _, fvars1 = free_vars_mtype already_binded m.ret_type in
    let _, fvars2 = List.fold_left (fun (set,fvars) {value=mt, x} -> set, (snd (free_vars_mtype set mt))@fvars) (already_binded, []) m.args in
    let _, fvars3 = List.fold_left (fun (set,fvars) stmt -> set, (snd (free_vars_stmt set stmt))@fvars) (already_binded, []) m.body 
    in
    already_binded, fvars1@fvars2@fvars3
and free_vars_function_dcl (already_binded:Atom.Set.t) = map0_place (free_vars_function_dcl_ already_binded) 

and free_vars_method0_ (already_binded:Atom.Set.t) place (m:_method0) =
    let _, fvars1 = free_vars_function_dcl_ already_binded place {
        name        = m.name;
        ret_type    = m.ret_type;
        args        = m.args;
        body        = m.body;
    } in 
    let _, fvars4 = match m.contract_opt with
        | Some c -> free_vars_contract already_binded c
        | None -> already_binded, []
    in
    already_binded, fvars1@fvars4
and free_vars_method0 (already_binded:Atom.Set.t) = map0_place (free_vars_method0_ already_binded) 

and free_vars_component_item_ (already_binded:Atom.Set.t) place = function 
    | Contract c -> free_vars_contract already_binded c
    | Method m -> free_vars_method0 already_binded m
    | State s -> free_vars_state already_binded s 
    | Port p  -> free_vars_port already_binded p
    | Term t -> free_vars_term already_binded t    
and free_vars_component_item (already_binded:Atom.Set.t) = map0_place (free_vars_component_item_ already_binded) 

and free_vars_component_dcl_ (already_binded:Atom.Set.t) place = function 
| ComponentStructure cdcl ->
    assert(cdcl.args = []);
    (* FIXME TODO do i need to propagate field/method name binding ???*)
    List.fold_left (fun (set, fvars) citem -> set, snd(free_vars_component_item set citem)@fvars) (already_binded, []) cdcl.body
and free_vars_component_dcl (already_binded:Atom.Set.t) = map0_place (free_vars_component_dcl_ already_binded) 

and free_vars_typedef_ (already_binded:Atom.Set.t) place = function 
(* already binded left unchanged since it is type binder *)
| ClassicalDef  (x, targs, body) -> already_binded, []
| EventDef (x, targs, body) -> already_binded, []
| ProtocolDef (x, mt) -> free_vars_mtype already_binded mt
and free_vars_typedef (already_binded:Atom.Set.t) = map0_place (free_vars_typedef_ already_binded) 


and free_vars_term_ (already_binded:Atom.Set.t) place = function 
    | EmptyTerm | Comments _ -> already_binded, []
    | Stmt stmt -> free_vars_stmt already_binded stmt
    | Component cdcl -> free_vars_component_dcl already_binded cdcl
    | Function fdcl -> free_vars_function_dcl already_binded fdcl
    | Typealias _ -> already_binded, [] (* type binder but not an expr binder so already_binded is left unchanged*)
    | Typedef typedef -> free_vars_typedef already_binded typedef
and free_vars_term (already_binded:Atom.Set.t) = map0_place (free_vars_term_ already_binded) 