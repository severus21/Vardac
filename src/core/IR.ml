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
and free_vars_contract (already_binded:Atom.Set.t) c = 
   let already_binded, fvars = map0_place (free_vars_contract_ already_binded) c in
    already_binded, Utils.deduplicate snd fvars

and free_vars_port_ (already_binded:Atom.Set.t) place (_port, _) =
    let _, fvars1 = free_vars_expr already_binded _port.input in
    let _, fvars2 = free_vars_mtype already_binded _port.expecting_st in
    let _, fvars3 = free_vars_expr already_binded _port.callback in
    already_binded, fvars1@fvars2@fvars3
and free_vars_port (already_binded:Atom.Set.t) p = 
    let already_binded, fvars = map0_place (free_vars_port_ already_binded) p in
    already_binded, Utils.deduplicate snd fvars

and free_vars_state_ (already_binded:Atom.Set.t) place = function 
| StateDcl sdcl -> 
    let _, fvars1 = free_vars_mtype already_binded sdcl.type0 in
    let _, fvars2 = match sdcl.body with
    | Some e -> free_vars_expr already_binded e
    | None _ -> already_binded, []
    in
    already_binded, fvars1@fvars2
and free_vars_state (already_binded:Atom.Set.t) s = 
    let already_binded, fvars =  map0_place (free_vars_state_ already_binded) s in
    already_binded, Utils.deduplicate snd fvars

and free_vars_function_dcl_ (already_binded:Atom.Set.t) place m =
    let _, fvars1 = free_vars_mtype already_binded m.ret_type in
    let _, fvars2 = List.fold_left (fun (set,fvars) {value=mt, x} -> set, (snd (free_vars_mtype set mt))@fvars) (already_binded, []) m.args in

    let already_binded = Atom.Set.add m.name already_binded in (*rec support*)
    let already_binded = List.fold_left (fun set {value=_,x} -> Atom.Set.add x set) already_binded m.args in
    let _, fvars3 = List.fold_left_map (fun already_binded stmt -> free_vars_stmt already_binded stmt) already_binded m.body  in
    let fvars3 = List.flatten fvars3 in

    already_binded, fvars1@fvars2@fvars3
and free_vars_function_dcl (already_binded:Atom.Set.t) fdcl = 
   let already_binded, fvars =  map0_place (free_vars_function_dcl_ already_binded) fdcl in
    already_binded, Utils.deduplicate snd fvars

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
and free_vars_method0 (already_binded:Atom.Set.t) m = 
    let already_binded, fvars = map0_place (free_vars_method0_ already_binded) m in
    already_binded, Utils.deduplicate snd fvars

and free_vars_component_item_ (already_binded:Atom.Set.t) place = function 
    | Contract c -> free_vars_contract already_binded c
    | Method m -> free_vars_method0 already_binded m
    | State s -> free_vars_state already_binded s 
    | Port p  -> free_vars_port already_binded p
    | Term t -> free_vars_term already_binded t    
and free_vars_component_item (already_binded:Atom.Set.t) citem =              
    let already_binded, fvars = map0_place (free_vars_component_item_ already_binded) citem in
    already_binded, Utils.deduplicate snd fvars

and free_vars_component_dcl_ (already_binded:Atom.Set.t) place = function 
| ComponentStructure cdcl ->
    assert(cdcl.args = []);
    (* FIXME TODO do i need to propagate field/method name binding ???*)

    (* Shallow scan because fields and methods could be recursive *)
    let already_binded = List.fold_left (
        fun already_binded citem -> 
            match citem.value with
            | Contract _ -> already_binded
            | Method m -> Atom.Set.add m.value.name already_binded
            | State s -> Atom.Set.add(match s.value with 
                | StateDcl {name} -> name
                | StateAlias {name} -> name
            ) already_binded
            | Port p -> Atom.Set.add (fst p.value).name already_binded
            | Term t -> already_binded
    ) already_binded cdcl.body in

    let _, fvars = List.fold_left_map (fun already_binded citem -> free_vars_component_item already_binded citem) already_binded cdcl.body in
    already_binded, List.flatten fvars
and free_vars_component_dcl (already_binded:Atom.Set.t) cdcl = 
    let already_binded, fvars = map0_place (free_vars_component_dcl_ already_binded) cdcl in
    already_binded, Utils.deduplicate snd fvars

and free_vars_typedef_ (already_binded:Atom.Set.t) place = function 
(* already binded left unchanged since it is type binder *)
| ClassicalDef  (x, targs, body) -> already_binded, []
| EventDef (x, targs, body) -> already_binded, []
| ProtocolDef (x, mt) -> free_vars_mtype already_binded mt
and free_vars_typedef (already_binded:Atom.Set.t) tdef= 
    let already_binded, fvars = map0_place (free_vars_typedef_ already_binded) tdef in
    already_binded, Utils.deduplicate snd fvars


and free_vars_term_ (already_binded:Atom.Set.t) place = function 
    | EmptyTerm | Comments _ -> already_binded, []
    | Stmt stmt -> free_vars_stmt already_binded stmt
    | Component cdcl -> free_vars_component_dcl already_binded cdcl
    | Function fdcl -> free_vars_function_dcl already_binded fdcl
    | Typealias _ -> already_binded, [] (* type binder but not an expr binder so already_binded is left unchanged*)
    | Typedef typedef -> free_vars_typedef already_binded typedef
and free_vars_term (already_binded:Atom.Set.t) t = 
    let already_binded, fvars = map0_place (free_vars_term_ already_binded) t in
    already_binded, Utils.deduplicate snd fvars


let rec replace_expr_contract_  x_to_replace ((replaceby_x_opt, replaceby_e_opt)as replaceby) place _contract =
    {_contract with  
        pre_binders = List.map (function (mt, x, e) -> (mt, x, replace_expr_expr x_to_replace replaceby e)) _contract.pre_binders; (*TODO replace for mt*)
        ensures = Option.map (replace_expr_expr x_to_replace replaceby) _contract.ensures;
        returns = Option.map (replace_expr_expr x_to_replace replaceby) _contract.returns;
    }
and replace_expr_contract x_to_replace replaceby = map_place (replace_expr_contract_ x_to_replace replaceby) 

and replace_expr_port_  x_to_replace ((replaceby_x_opt, replaceby_e_opt)as replaceby) place (_port, mt) =
    ({ _port with
        input = replace_expr_expr x_to_replace replaceby _port.input; 
        (* TODO replace_expr_mt expecting_st*)
        callback = replace_expr_expr x_to_replace replaceby _port.callback; 
    }, mt)
    
and replace_expr_port x_to_replace replaceby = map_place (replace_expr_port_ x_to_replace replaceby) 

and replace_expr_state_  x_to_replace ((replaceby_x_opt, replaceby_e_opt)as replaceby) place = function 
| StateDcl sdcl -> StateDcl {
    sdcl with body = Option.map (replace_expr_expr x_to_replace replaceby) sdcl.body;
}
and replace_expr_state x_to_replace replaceby = map_place (replace_expr_state_ x_to_replace replaceby) 

and replace_expr_function_dcl_  x_to_replace ((replaceby_x_opt, replaceby_e_opt)as replaceby) place m =
    { m with body = List.map (replace_expr_stmt x_to_replace replaceby) m.body }
and replace_expr_function_dcl x_to_replace replaceby = map_place (replace_expr_function_dcl_ x_to_replace replaceby) 

and replace_expr_method0_  x_to_replace ((replaceby_x_opt, replaceby_e_opt)as replaceby) place (m:_method0) =
    { m with 
        body = List.map (replace_expr_stmt x_to_replace replaceby) m.body;
        contract_opt = Option.map (replace_expr_contract x_to_replace replaceby) m.contract_opt;
     }
and replace_expr_method0 x_to_replace replaceby = map_place (replace_expr_method0_ x_to_replace replaceby) 

and replace_expr_component_item_  x_to_replace ((replaceby_x_opt, replaceby_e_opt)as replaceby) place = function 
    | Contract c -> Contract (replace_expr_contract x_to_replace replaceby c)
    | Method m -> Method (replace_expr_method0 x_to_replace replaceby m)
    | State s -> State (replace_expr_state x_to_replace replaceby s )
    | Port p  -> Port (replace_expr_port x_to_replace replaceby p)
    | Term t -> Term (replace_expr_term x_to_replace replaceby t)
and replace_expr_component_item x_to_replace replaceby = map_place (replace_expr_component_item_ x_to_replace replaceby) 

and replace_expr_component_dcl_  x_to_replace ((replaceby_x_opt, replaceby_e_opt)as replaceby) place = function 
| ComponentStructure cdcl -> 
    ComponentStructure { cdcl with body = List.map (replace_expr_component_item x_to_replace replaceby) cdcl.body}
and replace_expr_component_dcl x_to_replace replaceby = map_place (replace_expr_component_dcl_ x_to_replace replaceby) 

and replace_expr_term_  x_to_replace ((replaceby_x_opt, replaceby_e_opt)as replaceby) place = function 
| EmptyTerm -> EmptyTerm 
| Comments c -> Comments c
| Stmt stmt -> Stmt (replace_expr_stmt x_to_replace replaceby stmt)
| Component cdcl -> Component (replace_expr_component_dcl x_to_replace replaceby cdcl)
| Function fdcl -> Function (replace_expr_function_dcl x_to_replace replaceby fdcl)
| (Typealias _ as t) |(Typedef _ as t) -> t
and replace_expr_term x_to_replace replaceby = map_place (replace_expr_term_ x_to_replace replaceby) 