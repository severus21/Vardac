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

let rec collect_expr_contract_ parent_opt (already_binded:Atom.Set.t) selector collector place _contract = 
    let inner_already_binded = List.fold_left (fun already_binded (mt, x, e) ->
        Variable.Set.add x already_binded
    ) already_binded _contract.pre_binders in
    let res = List.map (function (_, _, e) -> collect_expr_expr parent_opt already_binded selector collector e) _contract.pre_binders in
    let collected_elts1 = List.flatten (List.map (function (_,x,_) -> x) res) in
    let fvars1 = List.flatten (List.map (function (_,_,x) -> x) res) in

    let _, collected_elts2, fvars2 = 
    match _contract.ensures with
    | None -> already_binded, [], []
    | Some ensures -> collect_expr_expr parent_opt already_binded selector collector ensures 
    in

    let _, collected_elts3, fvars3 = 
    match _contract.returns with
    | None -> already_binded, [], []
    | Some returns -> collect_expr_expr parent_opt already_binded selector collector returns 
    in

    already_binded, collected_elts1@collected_elts2@collected_elts3, fvars1@fvars2@fvars3

and collect_expr_contract parent_opt (already_binded:Atom.Set.t) selector collector c = 
    map0_place (collect_expr_contract_ parent_opt already_binded selector collector) c 
and collect_expr_port_ parent_opt (already_binded:Atom.Set.t) selector collector place (_port, _) =
    let _, collected_elts1, fvars1 = collect_expr_expr parent_opt already_binded  selector collector _port.input in
    let _, collected_elts2, fvars2 = collect_expr_mtype parent_opt already_binded selector collector _port.expecting_st in
    let _, collected_elts3, fvars3 = collect_expr_expr parent_opt already_binded  selector collector _port.callback in
    already_binded, collected_elts1@collected_elts2@collected_elts3, fvars1@fvars2@fvars3
and collect_expr_port parent_opt (already_binded:Atom.Set.t) selector collector p = 
    map0_place (collect_expr_port_ parent_opt already_binded selector collector) p

and collect_expr_state_ parent_opt (already_binded:Atom.Set.t) selector collector place = function 
| StateDcl sdcl -> 
    let _, collected_elts1, fvars1 = collect_expr_mtype parent_opt already_binded selector collector sdcl.type0 in
    let _, collected_elts2, fvars2 = match sdcl.body with
    | Some e -> collect_expr_expr parent_opt already_binded selector collector e
    | None _ -> already_binded, [], []
    in

    already_binded, collected_elts1@collected_elts2, fvars1@fvars2
and collect_expr_state parent_opt (already_binded:Atom.Set.t) selector collector s = 
    map0_place (collect_expr_state_ parent_opt already_binded selector collector) s 

and collect_expr_function_dcl_ parent_opt (already_binded:Atom.Set.t) selector collector place m =
    let _, collected_elts1, fvars1 = collect_expr_mtype parent_opt already_binded selector collector m.ret_type in
    let _, collected_elts2, fvars2 = List.fold_left (fun (set, collected_elts0, fvars0) {value=mt, x} -> 
        let _, collected_elts, fvars = collect_expr_mtype parent_opt set selector collector mt in
        set, collected_elts0@collected_elts, fvars0@fvars
    ) (already_binded, [], []) m.args in

    let already_binded = Atom.Set.add m.name already_binded in (*rec support*)
    let already_binded = List.fold_left (fun set {value=_,x} -> Atom.Set.add x set) already_binded m.args in
    let _, res = List.fold_left_map (fun already_binded stmt ->         
        let env, a,b  = collect_expr_stmt parent_opt already_binded selector collector stmt in
        env, (a,b)
    ) already_binded m.body  in
    let collected_elts3 = List.flatten (List.map fst res) in
    let fvars3 = List.flatten (List.map snd res) in

    already_binded, collected_elts1@collected_elts2@collected_elts3, fvars1@fvars2@fvars3
and collect_expr_function_dcl parent_opt (already_binded:Atom.Set.t) selector collector fdcl = 
    map0_place (collect_expr_function_dcl_ parent_opt already_binded selector collector) fdcl

and collect_expr_method0_ parent_opt (already_binded:Atom.Set.t) selector collector place (m:_method0) =
    let _, collected_elts1, fvars1 = collect_expr_function_dcl_ parent_opt already_binded selector collector place {
        name        = m.name;
        ret_type    = m.ret_type;
        args        = m.args;
        body        = m.body;
    } in 
    let _, collected_elts4, fvars4 = match m.contract_opt with
        | Some c -> collect_expr_contract parent_opt already_binded selector collector c
        | None -> already_binded, [],[]
    in
    already_binded, collected_elts1@collected_elts4, fvars1@fvars4
and collect_expr_method0 parent_opt (already_binded:Atom.Set.t) selector collector m = 
    map0_place (collect_expr_method0_ parent_opt already_binded selector collector) m 
and collect_expr_component_item_ parent_opt (already_binded:Atom.Set.t) selector collector place = function 
    | Contract c -> collect_expr_contract parent_opt already_binded selector collector c
    | Method m -> collect_expr_method0 parent_opt already_binded selector collector m
    | State s -> collect_expr_state parent_opt already_binded selector collector s 
    | Port p  -> collect_expr_port parent_opt already_binded selector collector p
    | Term t -> collect_expr_term  parent_opt already_binded selector collector t    
and collect_expr_component_item parent_opt (already_binded:Atom.Set.t) selector collector citem =              
    map0_place (collect_expr_component_item_ parent_opt already_binded selector collector) citem

and free_vars_component_item already_binded citem = 
    let already_binded, _, fvars = collect_expr_component_item None  already_binded (function e -> false) (fun parent_opt env e -> []) citem in
    already_binded, Utils.deduplicate snd fvars 

and collect_expr_component_dcl_ parent_opt (already_binded:Atom.Set.t) selector collector place = function 
| ComponentStructure cdcl ->
    let parent_opt = Some cdcl.name in
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
        let env, a,b = collect_expr_component_item parent_opt already_binded selector collector citem in
        env, (a,b)    
    ) already_binded cdcl.body in
    let collected_elts = List.flatten (List.map fst res) in
    let fvars = List.flatten (List.map snd res) in
    already_binded, collected_elts, fvars
and collect_expr_component_dcl parent_opt (already_binded:Atom.Set.t) selector collector cdcl = 
    map0_place (collect_expr_component_dcl_ parent_opt already_binded selector collector ) cdcl

and free_vars_component_dcl already_binded cdcl = 
    let already_binded, _, fvars = collect_expr_component_dcl None  already_binded (function e -> false) (fun parent_opt env e -> []) cdcl in
    already_binded, Utils.deduplicate snd fvars 

and collect_expr_typedef_ parent_opt (already_binded:Atom.Set.t) selector collector place = function 
(* already binded left unchanged since it is type binder *)
| ClassicalDef  (x, targs, body) -> already_binded, [], []
| EventDef (x, targs, body) -> already_binded, [], []
| ProtocolDef (x, mt) -> collect_expr_mtype parent_opt already_binded selector collector mt
and collect_expr_typedef parent_opt (already_binded:Atom.Set.t) selector collector tdef= 
    map0_place (collect_expr_typedef_ parent_opt already_binded selector collector) tdef

and collect_expr_derivation parent_opt (already_binded:Atom.Set.t) selector collector place derive =
    let _, tmp1 = [], [] in (*List.fold_left_map (collect_expr_cexpr parent_opt already_binded selector collector) derive.cargs  in*)
    let _, tmp2 = List.fold_left_map (fun already_binded truc -> 
        let env, a,b = collect_expr_mtype parent_opt already_binded selector collector truc in
        env, (a,b)    
    ) already_binded derive.targs in
    let _, tmp3 = List.fold_left_map (fun already_binded truc -> 
        let env, a,b =  collect_expr_expr parent_opt already_binded selector collector truc in
        env, (a,b)    
    ) already_binded derive.eargs  in
    let res = tmp1@tmp2@tmp3 in

    let collected_elts = List.flatten (List.map fst res) in
    let fvars = List.flatten (List.map snd res) in
    already_binded, collected_elts, fvars

and collect_expr_term_ parent_opt (already_binded:Atom.Set.t) selector collector place = function 
    | EmptyTerm | Comments _ -> already_binded, [], []
    | Stmt stmt -> collect_expr_stmt parent_opt already_binded selector collector stmt
    | Component cdcl -> collect_expr_component_dcl parent_opt already_binded selector collector cdcl
    | Function fdcl -> collect_expr_function_dcl parent_opt already_binded selector collector fdcl
    | Typealias _ -> already_binded, [], [] (* type binder but not an expr binder so already_binded is left unchanged*)
    | Typedef typedef -> collect_expr_typedef parent_opt already_binded selector collector typedef
    | Derive derive ->  collect_expr_derivation parent_opt already_binded selector collector place derive 
and collect_expr_term parent_opt (already_binded:Atom.Set.t) selector collector t = 
    map0_place (collect_expr_term_ parent_opt already_binded selector collector) t

and collect_expr_program already_binded selector collector program = 
    let _, res = List.fold_left_map (fun already_binded term -> 
        let env, a,b = collect_expr_term None already_binded selector collector term in
        env, (a,b)    
    ) already_binded program in
    let collected_elts = List.flatten (List.map fst res) in
    let fvars = List.flatten (List.map snd res) in
    already_binded, collected_elts, fvars

and free_vars_term already_binded citem = 
    let already_binded, _, fvars = collect_expr_term None  already_binded (function e -> false) (fun parent_opt env e -> []) citem in
    already_binded, Utils.deduplicate snd fvars 


and free_vars_program already_binded program = 
    let already_binded, _, fvars = collect_expr_program  already_binded (function e -> false) (fun parent_opt env e -> []) program in
    already_binded, Utils.deduplicate snd fvars 


(******************************************************************)


let rec collect_type_contract_ parent_opt (already_binded:Atom.Set.t) selector collector place _contract = 
    let inner_already_binded = List.fold_left (fun already_binded (mt, x, e) ->
        Variable.Set.add x already_binded
    ) already_binded _contract.pre_binders in
    let res = List.map (function (_, _, e) -> collect_type_expr parent_opt already_binded selector collector e) _contract.pre_binders in
    let collected_elts1 = List.flatten (List.map (function (_,x,_) -> x) res) in
    let fvars1 = List.flatten (List.map (function (_,_,x) -> x) res) in

    let _, collected_elts2, fvars2 = 
    match _contract.ensures with
    | None -> already_binded, [], []
    | Some ensures -> collect_type_expr parent_opt already_binded selector collector ensures 
    in

    let _, collected_elts3, fvars3 = 
    match _contract.returns with
    | None -> already_binded, [], []
    | Some returns -> collect_type_expr parent_opt already_binded selector collector returns 
    in

    already_binded, collected_elts1@collected_elts2@collected_elts3, fvars1@fvars2@fvars3

and collect_type_contract parent_opt (already_binded:Atom.Set.t) selector collector c = 
    map0_place (collect_type_contract_ parent_opt already_binded selector collector) c 
and collect_type_port_ parent_opt (already_binded:Atom.Set.t) selector collector place (_port, _) =
    let _, collected_elts1, fvars1 = collect_type_expr parent_opt already_binded  selector collector _port.input in
    let _, collected_elts2, fvars2 = collect_type_mtype parent_opt already_binded selector collector _port.expecting_st in
    let _, collected_elts3, fvars3 = collect_type_expr parent_opt already_binded  selector collector _port.callback in
    already_binded, collected_elts1@collected_elts2@collected_elts3, fvars1@fvars2@fvars3
and collect_type_port parent_opt (already_binded:Atom.Set.t) selector collector p = 
    map0_place (collect_type_port_ parent_opt already_binded selector collector) p

and collect_type_state_ parent_opt (already_binded:Atom.Set.t) selector collector place = function 
| StateDcl sdcl -> 
    let _, collected_elts1, fvars1 = collect_type_mtype parent_opt already_binded selector collector sdcl.type0 in
    let _, collected_elts2, fvars2 = match sdcl.body with
    | Some e -> collect_type_expr parent_opt already_binded selector collector e
    | None _ -> already_binded, [], []
    in

    already_binded, collected_elts1@collected_elts2, fvars1@fvars2
and collect_type_state parent_opt (already_binded:Atom.Set.t) selector collector s = 
    map0_place (collect_type_state_ parent_opt already_binded selector collector) s 

and collect_type_function_dcl_ parent_opt (already_binded:Atom.Set.t) selector collector place m =
    let _, collected_elts1, fvars1 = collect_type_mtype parent_opt already_binded selector collector m.ret_type in
    let _, collected_elts2, fvars2 = List.fold_left (fun (set, collected_elts0, fvars0) {value=mt, x} -> 
        let _, collected_elts, fvars = collect_type_mtype parent_opt set selector collector mt in
        set, collected_elts0@collected_elts, fvars0@fvars
    ) (already_binded, [], []) m.args in

    let already_binded = Atom.Set.add m.name already_binded in (*rec support*)
    let already_binded = List.fold_left (fun set {value=_,x} -> Atom.Set.add x set) already_binded m.args in
    let _, res = List.fold_left_map (fun already_binded stmt ->         
        let env, a,b  = collect_type_stmt parent_opt already_binded selector collector stmt in
        env, (a,b)
    ) already_binded m.body  in
    let collected_elts3 = List.flatten (List.map fst res) in
    let fvars3 = List.flatten (List.map snd res) in

    already_binded, collected_elts1@collected_elts2@collected_elts3, fvars1@fvars2@fvars3
and collect_type_function_dcl parent_opt (already_binded:Atom.Set.t) selector collector fdcl = 
    map0_place (collect_type_function_dcl_ parent_opt already_binded selector collector) fdcl

and collect_type_method0_ parent_opt (already_binded:Atom.Set.t) selector collector place (m:_method0) =
    let _, collected_elts1, fvars1 = collect_type_function_dcl_ parent_opt already_binded selector collector place {
        name        = m.name;
        ret_type    = m.ret_type;
        args        = m.args;
        body        = m.body;
    } in 
    let _, collected_elts4, fvars4 = match m.contract_opt with
        | Some c -> collect_type_contract parent_opt already_binded selector collector c
        | None -> already_binded, [],[]
    in
    already_binded, collected_elts1@collected_elts4, fvars1@fvars4
and collect_type_method0 parent_opt (already_binded:Atom.Set.t) selector collector m = 
    map0_place (collect_type_method0_ parent_opt already_binded selector collector) m 
and collect_type_component_item_ parent_opt (already_binded:Atom.Set.t) selector collector place = function 
    | Contract c -> collect_type_contract parent_opt already_binded selector collector c
    | Method m -> collect_type_method0 parent_opt already_binded selector collector m
    | State s -> collect_type_state parent_opt already_binded selector collector s 
    | Port p  -> collect_type_port parent_opt already_binded selector collector p
    | Term t -> collect_type_term  parent_opt already_binded selector collector t    
and collect_type_component_item parent_opt (already_binded:Atom.Set.t) selector collector citem =              
    map0_place (collect_type_component_item_ parent_opt already_binded selector collector) citem

and free_tvars_component_item already_binded citem = 
    let already_binded, _, ftvars = collect_type_component_item None  already_binded (function e -> false) (fun parent_opt env e -> []) citem in
    already_binded, Utils.deduplicate Fun.id ftvars 

and collect_type_component_dcl_ parent_opt (already_binded:Atom.Set.t) selector collector place = function 
| ComponentStructure cdcl ->
    let parent_opt = Some cdcl.name in
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
        let env, a,b = collect_type_component_item parent_opt already_binded selector collector citem in
        env, (a,b)    
    ) already_binded cdcl.body in
    let collected_elts = List.flatten (List.map fst res) in
    let fvars = List.flatten (List.map snd res) in
    already_binded, collected_elts, fvars
and collect_type_component_dcl parent_opt (already_binded:Atom.Set.t) selector collector cdcl = 
    map0_place (collect_type_component_dcl_ parent_opt already_binded selector collector ) cdcl

and free_tvars_component_dcl already_binded cdcl = 
    let already_binded, _, ftvars = collect_type_component_dcl None  already_binded (function e -> false) (fun parent_opt env e -> []) cdcl in
    already_binded, Utils.deduplicate Fun.id ftvars 

and collect_type_typedef_ parent_opt (already_binded:Atom.Set.t) selector collector place = function 
(* already binded left unchanged since it is type binder *)
| ClassicalDef  (x, targs, body) -> already_binded, [], []
| EventDef (x, targs, body) -> already_binded, [], []
| ProtocolDef (x, mt) -> collect_type_mtype parent_opt already_binded selector collector mt
and collect_type_typedef parent_opt (already_binded:Atom.Set.t) selector collector tdef= 
    map0_place (collect_type_typedef_ parent_opt already_binded selector collector) tdef

and collect_type_derivation parent_opt (already_binded:Atom.Set.t) selector collector place derive =
    let _, tmp1 = [], [] in (*List.fold_left_map (collect_type_cexpr parent_opt already_binded selector collector) derive.cargs  in*)
    let _, tmp2 = List.fold_left_map (fun already_binded truc -> 
        let env, a,b = collect_type_mtype parent_opt already_binded selector collector truc in
        env, (a,b)    
    ) already_binded derive.targs in
    let _, tmp3 = List.fold_left_map (fun already_binded truc -> 
        let env, a,b =  collect_type_expr parent_opt already_binded selector collector truc in
        env, (a,b)    
    ) already_binded derive.eargs  in
    let res = tmp1@tmp2@tmp3 in

    let collected_elts = List.flatten (List.map fst res) in
    let fvars = List.flatten (List.map snd res) in
    already_binded, collected_elts, fvars

and collect_type_term_ parent_opt (already_binded:Atom.Set.t) selector collector place = function 
    | EmptyTerm | Comments _ -> already_binded, [], []
    | Stmt stmt -> collect_type_stmt parent_opt already_binded selector collector stmt
    | Component cdcl -> collect_type_component_dcl parent_opt already_binded selector collector cdcl
    | Function fdcl -> collect_type_function_dcl parent_opt already_binded selector collector fdcl
    | Typealias _ -> already_binded, [], [] (* type binder but not an expr binder so already_binded is left unchanged*)
    | Typedef typedef -> collect_type_typedef parent_opt already_binded selector collector typedef
    | Derive derive ->  collect_type_derivation parent_opt already_binded selector collector place derive 
and collect_type_term parent_opt (already_binded:Atom.Set.t) selector collector t = 
    map0_place (collect_type_term_ parent_opt already_binded selector collector) t

and collect_type_program already_binded selector collector program = 
    let _, res = List.fold_left_map (fun already_binded term -> 
        let env, a,b = collect_type_term None already_binded selector collector term in
        env, (a,b)    
    ) already_binded program in
    let collected_elts = List.flatten (List.map fst res) in
    let fvars = List.flatten (List.map snd res) in
    already_binded, collected_elts, fvars

and free_tvars_term already_binded citem = 
    let already_binded, _, ftvars = collect_type_term None  already_binded (function e -> false) (fun parent_opt env e -> []) citem in
    already_binded, Utils.deduplicate Fun.id ftvars 


and free_tvars_program already_binded program = 
    let already_binded, _, ftvars = collect_type_program  already_binded (function e -> false) (fun parent_opt env e -> []) program in
    already_binded, Utils.deduplicate Fun.id ftvars 










(*****************************************************************)

let rec rewrite_type_contract_ selector rewriter place _contract =
    let rewrite_expr = rewrite_type_expr selector rewriter in
    let rewrite_mtype = rewrite_type_mtype selector rewriter in
    {_contract with  
        pre_binders = List.map (function (mt, x, e) -> (
            rewrite_mtype mt, x, rewrite_expr e)) _contract.pre_binders;
        ensures = Option.map rewrite_expr _contract.ensures;
        returns = Option.map rewrite_expr _contract.returns;
    }
and rewrite_type_contract selector rewriter = map_place (rewrite_type_contract_ selector rewriter) 

and rewrite_type_port_  selector rewriter place (_port, mt) =
    let rewrite_expr = rewrite_type_expr selector rewriter in
    let rewrite_mtype = rewrite_type_mtype selector rewriter in
    ({ _port with
        input = rewrite_expr  _port.input; 
        expecting_st  = rewrite_mtype _port.expecting_st;
        callback = rewrite_expr _port.callback; 
    }, mt)
    
and rewrite_type_port selector rewriter = map_place (rewrite_type_port_ selector rewriter) 

and rewrite_type_state_  selector rewriter place = function 
| StateDcl sdcl -> StateDcl {
    sdcl with 
        type0 = rewrite_type_mtype selector rewriter sdcl.type0;
        body = Option.map (rewrite_type_expr selector rewriter) sdcl.body;
}
and rewrite_type_state selector rewriter = map_place (rewrite_type_state_ selector rewriter) 

and rewrite_type_function_dcl_  selector rewriter place m =
    let rewrite_expr = rewrite_type_expr selector rewriter in
    let rewrite_mtype = rewrite_type_mtype selector rewriter in
    { m with 
        ret_type = rewrite_mtype m.ret_type;
        args = List.map (function {place; value=(mt,x)} -> 
            {place; value=rewrite_mtype mt, x}
        ) m.args;
        body = List.map (rewrite_type_stmt selector rewriter) m.body 
    }
and rewrite_type_function_dcl selector rewriter = map_place (rewrite_type_function_dcl_ selector rewriter) 

and rewrite_type_method0_  selector rewriter place (m:_method0) =
    let rewrite_expr = rewrite_type_expr selector rewriter in
    let rewrite_mtype = rewrite_type_mtype selector rewriter in
    { m with 
        ret_type = rewrite_mtype m.ret_type;
        args = List.map (function {place; value=(mt,x)} -> 
            {place; value=rewrite_mtype mt, x}
        ) m.args;
        body = List.map (rewrite_type_stmt selector rewriter) m.body;
        contract_opt = Option.map (rewrite_type_contract selector rewriter) m.contract_opt;
     }
and rewrite_type_method0 selector rewriter = map_place (rewrite_type_method0_ selector rewriter) 

and rewrite_type_component_item_  selector rewriter place = function 
    | Contract c -> Contract (rewrite_type_contract selector rewriter c)
    | Method m -> Method (rewrite_type_method0 selector rewriter m)
    | State s -> State (rewrite_type_state selector rewriter s )
    | Port p  -> Port (rewrite_type_port selector rewriter p)
    | Term t -> Term (rewrite_type_term selector rewriter t)
and rewrite_type_component_item selector rewriter = map_place (rewrite_type_component_item_ selector rewriter) 

and rewrite_type_component_dcl_  selector rewriter place = function 
| ComponentStructure cdcl -> 
    ComponentStructure { cdcl with body = List.map (rewrite_type_component_item selector rewriter) cdcl.body}
and rewrite_type_component_dcl selector rewriter = map_place (rewrite_type_component_dcl_ selector rewriter) 

and rewrite_type_term_ selector rewriter place = function 
| EmptyTerm -> EmptyTerm 
| Comments c -> Comments c
| Stmt stmt -> Stmt (rewrite_type_stmt selector rewriter stmt)
| Component cdcl -> Component (rewrite_type_component_dcl selector rewriter cdcl)
| Function fdcl -> Function (rewrite_type_function_dcl selector rewriter fdcl)
| (Typealias _ as t) |(Typedef _ as t) -> t
| Derive derive -> Derive { derive with eargs = List.map (rewrite_type_expr selector rewriter) derive.eargs}
and rewrite_type_term selector rewriter = map_place (rewrite_type_term_ selector rewriter) 
and rewrite_type_program selector rewriter (program : program) : program = List.map (rewrite_type_term selector rewriter) program








(*****************************************************************)

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
    (* TODO FIXME rewrite type0*)
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
| Derive derive -> Derive { derive with eargs = List.map (rewrite_expr_expr selector rewriter) derive.eargs}
and rewrite_expr_term selector rewriter = map_place (rewrite_expr_term_ selector rewriter) 
and rewrite_expr_program selector rewriter (program : program) : program = List.map (rewrite_expr_term selector rewriter) program

let make x_to_replace ((replaceby_x_opt, replaceby_e_opt)as replaceby) = 
    let selector = function |VarExpr x when x = x_to_replace -> true | _ -> false in
    let rewriter e = match replaceby_x_opt with | Some x -> VarExpr x | None -> Option.get replaceby_e_opt in
    selector, rewriter
let replace_expr_component_item x_to_replace replaceby = 
    let selector, rewriter = make x_to_replace replaceby in
    rewrite_expr_component_item selector rewriter


let rec rewrite_term_component_item_  selector rewriter place = function 
| Term t -> List.map (function x -> Term x) (rewrite_term_term selector rewriter t)
| citem -> [citem]
and rewrite_term_component_item selector rewriter = map_places (rewrite_term_component_item_ selector rewriter) 

and rewrite_term_component_dcl_  selector rewriter place = function 
| ComponentStructure cdcl -> ComponentStructure { cdcl with body = List.flatten (List.map (rewrite_term_component_item selector rewriter) cdcl.body)}
| x -> x
and rewrite_term_component_dcl selector rewriter = map_place (rewrite_term_component_dcl_ selector rewriter) 

and rewrite_term_term_ selector rewriter place = function 
| t when selector t -> rewriter place t
| Component cdcl -> [Component (rewrite_term_component_dcl selector rewriter cdcl)]
| t -> [ t ]
and rewrite_term_term selector rewriter = map_places (rewrite_term_term_ selector rewriter) 

and rewrite_term_program (selector : _term -> bool) (rewriter : Error.place -> _term -> _term list) program = List.flatten (List.map (rewrite_term_term selector rewriter) program )

let rewrite_component_program (selector : component_structure -> bool) (rewriter : Error.place -> component_structure -> component_structure list) program : term list = 
    let selector_t = function
    | Component {value=ComponentStructure cdcl} when selector cdcl -> true
    | _ -> false
    in
    let rewriter_t place = function
    | Component {value=ComponentStructure cdcl} -> List.map (function x -> Component {place; value=ComponentStructure x}) (rewriter place cdcl)
    in
    rewrite_term_program selector_t rewriter_t program


let rec rewrite_scopeterm_component_item_  selector rewriter place = function 
| Term t -> List.map (function x -> Term x) (rewrite_scopeterm_term selector rewriter t)
| citem -> [citem]
and rewrite_scopeterm_component_item selector rewriter = map_places (rewrite_scopeterm_component_item_ selector rewriter) 

and rewrite_scopeterm_component_dcl_  selector rewriter place = function 
| ComponentStructure cdcl -> 
    
    if List.exists (function |{value=Term t} -> selector t | _ -> false) cdcl.body then (* Scope of the searched term is the current component *)
        let scope = List.map (function | {value=Term t} -> t) (List.filter (function |{value=Term _} -> true | _ -> false) cdcl.body) in
        let remaining_body = List.filter (function |{value=Term _} -> false | _ -> true) cdcl.body in

        ComponentStructure { cdcl with body = (List.map (function t -> {place=t.place; value=Term t}) (rewriter scope)) @ remaining_body }
    else
        ComponentStructure { cdcl with body = List.flatten (List.map (rewrite_scopeterm_component_item selector rewriter) cdcl.body)}
| x -> x
and rewrite_scopeterm_component_dcl selector rewriter = map_place (rewrite_scopeterm_component_dcl_ selector rewriter) 

and rewrite_scopeterm_term_ selector rewriter place = function 
| Component cdcl -> [Component (rewrite_scopeterm_component_dcl selector rewriter cdcl)]
| t -> [ t ]
and rewrite_scopeterm_term selector rewriter = map_places (rewrite_scopeterm_term_ selector rewriter) 

and rewrite_scopeterm_program (selector : term -> bool) (rewriter : term list -> term list) program = 
    if List.exists selector program then( (* Scope of the searched term is [Top-level]*)
        logger#debug "all program";
        rewriter program
    )else
        List.flatten (List.map (rewrite_scopeterm_term selector rewriter) program )