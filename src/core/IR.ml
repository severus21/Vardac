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

let rec collect_expr_contract_ (already_binded:Atom.Set.t) selector collector place _contract = 
    let inner_already_binded = List.fold_left (fun already_binded (mt, x, e) ->
        Variable.Set.add x already_binded
    ) already_binded _contract.pre_binders in
    let res = List.map (function (_, _, e) -> collect_expr_expr already_binded selector collector e) _contract.pre_binders in
    let collected_elts1 = List.flatten (List.map (function (_,x,_) -> x) res) in
    let fvars1 = List.flatten (List.map (function (_,_,x) -> x) res) in

    let _, collected_elts2, fvars2 = 
    match _contract.ensures with
    | None -> already_binded, [], []
    | Some ensures -> collect_expr_expr already_binded selector collector ensures 
    in

    let _, collected_elts3, fvars3 = 
    match _contract.returns with
    | None -> already_binded, [], []
    | Some returns -> collect_expr_expr already_binded selector collector returns 
    in

    already_binded, collected_elts1@collected_elts2@collected_elts3, fvars1@fvars2@fvars3

and collect_expr_contract (already_binded:Atom.Set.t) selector collector c = 
    map0_place (collect_expr_contract_ already_binded selector collector) c 
and collect_expr_port_ (already_binded:Atom.Set.t) selector collector place (_port, _) =
    let _, collected_elts1, fvars1 = collect_expr_expr already_binded  selector collector _port.input in
    let _, collected_elts2, fvars2 = collect_expr_mtype already_binded selector collector _port.expecting_st in
    let _, collected_elts3, fvars3 = collect_expr_expr already_binded  selector collector _port.callback in
    already_binded, collected_elts1@collected_elts2@collected_elts3, fvars1@fvars2@fvars3
and collect_expr_port (already_binded:Atom.Set.t) selector collector p = 
    map0_place (collect_expr_port_ already_binded selector collector) p

and collect_expr_state_ (already_binded:Atom.Set.t) selector collector place = function 
| StateDcl sdcl -> 
    let _, collected_elts1, fvars1 = collect_expr_mtype already_binded selector collector sdcl.type0 in
    let _, collected_elts2, fvars2 = match sdcl.body with
    | Some e -> collect_expr_expr already_binded selector collector e
    | None _ -> already_binded, [], []
    in

    already_binded, collected_elts1@collected_elts2, fvars1@fvars2
and collect_expr_state (already_binded:Atom.Set.t) selector collector s = 
    map0_place (collect_expr_state_ already_binded selector collector) s 

and collect_expr_function_dcl_ (already_binded:Atom.Set.t) selector collector place m =
    let _, collected_elts1, fvars1 = collect_expr_mtype already_binded selector collector m.ret_type in
    let _, collected_elts2, fvars2 = List.fold_left (fun (set, collected_elts0, fvars0) {value=mt, x} -> 
        let _, collected_elts, fvars = collect_expr_mtype set selector collector mt in
        set, collected_elts0@collected_elts, fvars0@fvars
    ) (already_binded, [], []) m.args in

    let already_binded = Atom.Set.add m.name already_binded in (*rec support*)
    let already_binded = List.fold_left (fun set {value=_,x} -> Atom.Set.add x set) already_binded m.args in
    let _, res = List.fold_left_map (fun already_binded stmt ->         
        let env, a,b  = collect_expr_stmt already_binded selector collector stmt in
        env, (a,b)
    ) already_binded m.body  in
    let collected_elts3 = List.flatten (List.map fst res) in
    let fvars3 = List.flatten (List.map snd res) in

    already_binded, collected_elts1@collected_elts2@collected_elts3, fvars1@fvars2@fvars3
and collect_expr_function_dcl (already_binded:Atom.Set.t) selector collector fdcl = 
    map0_place (collect_expr_function_dcl_ already_binded selector collector) fdcl

and collect_expr_method0_ (already_binded:Atom.Set.t) selector collector place (m:_method0) =
    let _, collected_elts1, fvars1 = collect_expr_function_dcl_ already_binded selector collector place {
        name        = m.name;
        ret_type    = m.ret_type;
        args        = m.args;
        body        = m.body;
    } in 
    let _, collected_elts4, fvars4 = match m.contract_opt with
        | Some c -> collect_expr_contract already_binded selector collector c
        | None -> already_binded, [],[]
    in
    already_binded, collected_elts1@collected_elts4, fvars1@fvars4
and collect_expr_method0 (already_binded:Atom.Set.t) selector collector m = 
    map0_place (collect_expr_method0_ already_binded selector collector) m 
and collect_expr_component_item_ (already_binded:Atom.Set.t) selector collector place = function 
    | Contract c -> collect_expr_contract already_binded selector collector c
    | Method m -> collect_expr_method0 already_binded selector collector m
    | State s -> collect_expr_state already_binded selector collector s 
    | Port p  -> collect_expr_port already_binded selector collector p
    | Term t -> collect_expr_term already_binded selector collector t    
and collect_expr_component_item (already_binded:Atom.Set.t) selector collector citem =              
    map0_place (collect_expr_component_item_ already_binded selector collector) citem

and free_vars_component_item already_binded citem = 
    let already_binded, _, fvars = collect_expr_component_item  already_binded (function e -> false) (fun env e -> []) citem in
    already_binded, Utils.deduplicate snd fvars 

and collect_expr_component_dcl_ (already_binded:Atom.Set.t) selector collector place = function 
| ComponentStructure cdcl ->
    assert(cdcl.args = []);
    (* FIXME TODO do i need to propagate field/method name binding ???*)

    (* Shallow scan because fields and methods could be recursive *)
    let already_binded = List.fold_left (
        fun already_binded citem -> 
            match citem.value with
            | Contract _ -> already_binded
            | Method m -> Atom.Set.add m.value.name already_binded
            | State s -> 
                Atom.Set.add(match s.value with 
                | StateDcl s -> s.name
                | StateAlias s -> s.name
            ) already_binded
            | Port p -> Atom.Set.add (fst p.value).name already_binded
            | Term t -> already_binded
    ) already_binded cdcl.body in
    logger#info "collect component %s [%d]" (Atom.to_string cdcl.name) (List.length cdcl.body);
    logger#info "%s\n\n" (Atom.Set.show already_binded);

    let _, res = List.fold_left_map (fun already_binded citem -> 
        let env, a,b = collect_expr_component_item already_binded selector collector citem in
        env, (a,b)    
    ) already_binded cdcl.body in
    let collected_elts = List.flatten (List.map fst res) in
    let fvars = List.flatten (List.map snd res) in
    already_binded, collected_elts, fvars
and collect_expr_component_dcl (already_binded:Atom.Set.t) selector collector cdcl = 
    map0_place (collect_expr_component_dcl_ already_binded selector collector ) cdcl

and free_vars_component_dcl already_binded cdcl = 
    let already_binded, _, fvars = collect_expr_component_dcl  already_binded (function e -> false) (fun env e -> []) cdcl in
    already_binded, Utils.deduplicate snd fvars 

and collect_expr_typedef_ (already_binded:Atom.Set.t) selector collector place = function 
(* already binded left unchanged since it is type binder *)
| ClassicalDef  (x, targs, body) -> already_binded, [], []
| EventDef (x, targs, body) -> already_binded, [], []
| ProtocolDef (x, mt) -> collect_expr_mtype already_binded selector collector mt
and collect_expr_typedef (already_binded:Atom.Set.t) selector collector tdef= 
    map0_place (collect_expr_typedef_ already_binded selector collector) tdef


and collect_expr_term_ (already_binded:Atom.Set.t) selector collector place = function 
    | EmptyTerm | Comments _ -> already_binded, [], []
    | Stmt stmt -> collect_expr_stmt already_binded selector collector stmt
    | Component cdcl -> collect_expr_component_dcl already_binded selector collector cdcl
    | Function fdcl -> collect_expr_function_dcl already_binded selector collector fdcl
    | Typealias _ -> already_binded, [], [] (* type binder but not an expr binder so already_binded is left unchanged*)
    | Typedef typedef -> collect_expr_typedef already_binded selector collector typedef
and collect_expr_term (already_binded:Atom.Set.t) selector collector t = 
    map0_place (collect_expr_term_ already_binded selector collector) t

and free_vars_term already_binded citem = 
    let already_binded, _, fvars = collect_expr_term  already_binded (function e -> false) (fun env e -> []) citem in
    already_binded, Utils.deduplicate snd fvars 

let rec rewrite_expr_contract_ selector rewriter place _contract =
    {_contract with  
        pre_binders = List.map (function (mt, x, e) -> (mt, x, rewrite_expr_expr selector rewriter e)) _contract.pre_binders; (*TODO replace for mt*)
        ensures = Option.map (rewrite_expr_expr selector rewriter) _contract.ensures;
        returns = Option.map (rewrite_expr_expr selector rewriter) _contract.returns;
    }
and rewrite_expr_contract selector rewriter = map_place (rewrite_expr_contract_ selector rewriter) 

and rewrite_expr_port_  selector rewriter place (_port, mt) =
    ({ _port with
        input = rewrite_expr_expr selector rewriter _port.input; 
        (* TODO rewrite_expr_mt expecting_st*)
        callback = rewrite_expr_expr selector rewriter _port.callback; 
    }, mt)
    
and rewrite_expr_port selector rewriter = map_place (rewrite_expr_port_ selector rewriter) 

and rewrite_expr_state_  selector rewriter place = function 
| StateDcl sdcl -> StateDcl {
    sdcl with body = Option.map (rewrite_expr_expr selector rewriter) sdcl.body;
}
and rewrite_expr_state selector rewriter = map_place (rewrite_expr_state_ selector rewriter) 

and rewrite_expr_function_dcl_  selector rewriter place m =
    { m with body = List.map (rewrite_expr_stmt selector rewriter) m.body }
and rewrite_expr_function_dcl selector rewriter = map_place (rewrite_expr_function_dcl_ selector rewriter) 

and rewrite_expr_method0_  selector rewriter place (m:_method0) =
    { m with 
        body = List.map (rewrite_expr_stmt selector rewriter) m.body;
        contract_opt = Option.map (rewrite_expr_contract selector rewriter) m.contract_opt;
     }
and rewrite_expr_method0 selector rewriter = map_place (rewrite_expr_method0_ selector rewriter) 

and rewrite_expr_component_item_  selector rewriter place = function 
    | Contract c -> Contract (rewrite_expr_contract selector rewriter c)
    | Method m -> Method (rewrite_expr_method0 selector rewriter m)
    | State s -> State (rewrite_expr_state selector rewriter s )
    | Port p  -> Port (rewrite_expr_port selector rewriter p)
    | Term t -> Term (rewrite_expr_term selector rewriter t)
and rewrite_expr_component_item selector rewriter = map_place (rewrite_expr_component_item_ selector rewriter) 

and rewrite_expr_component_dcl_  selector rewriter place = function 
| ComponentStructure cdcl -> 
    ComponentStructure { cdcl with body = List.map (rewrite_expr_component_item selector rewriter) cdcl.body}
and rewrite_expr_component_dcl selector rewriter = map_place (rewrite_expr_component_dcl_ selector rewriter) 

and rewrite_expr_term_ selector rewriter place = function 
| EmptyTerm -> EmptyTerm 
| Comments c -> Comments c
| Stmt stmt -> Stmt (rewrite_expr_stmt selector rewriter stmt)
| Component cdcl -> Component (rewrite_expr_component_dcl selector rewriter cdcl)
| Function fdcl -> Function (rewrite_expr_function_dcl selector rewriter fdcl)
| (Typealias _ as t) |(Typedef _ as t) -> t
and rewrite_expr_term selector rewriter = map_place (rewrite_expr_term_ selector rewriter) 

let make x_to_replace ((replaceby_x_opt, replaceby_e_opt)as replaceby) = 
    let selector = function |VarExpr x when x = x_to_replace -> true | _ -> false in
    let rewriter e = match replaceby_x_opt with | Some x -> VarExpr x | None -> Option.get replaceby_e_opt in
    selector, rewriter
let replace_expr_component_item x_to_replace replaceby = 
    let selector, rewriter = make x_to_replace replaceby in
    rewrite_expr_component_item selector rewriter