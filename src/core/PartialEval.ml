open IR
open Easy_logging
open Utils

let logger = Logging.make_logger "_1_ compspec" Debug [Cli Debug];;

(*
 TODO add 
 - Data-flow analysis - https://en.wikipedia.org/wiki/Data-flow_analysis
 - Static single assignment form : https://en.wikipedia.org/wiki/Static_single_assignment_form#Converting_out_of_SSA_form

*)

(* Environment *)
module Env = Atom.AtomMap 

type env = { 
    named_types: (main_type option) Env.t; (* namely the list of typealias with their current definition, 
    none means that it is an abstract type *)
    terminal_expr_assignements: expr Env.t (* x -> LitExpr _*)
} [@@deriving fields] 

let fresh_env () = { 
    named_types = Env.empty;
    terminal_expr_assignements = Env.empty;
}

let bind_named_types (env:env) key value : env = 
    { env with named_types = Env.add key value env.named_types}
let bind_terminal_expr (env:env) key value : env = 
    { env with terminal_expr_assignements = Env.add key value env.terminal_expr_assignements}

let map_snd f = function (x,y) -> (x, f y)

(*debug only
let print_env env =
    let print_keys env = Env.iter (fun x _ -> Printf.printf "%s;" (Atom.to_string x)) env in

    print_newline ();
    print_string "Env = {";

    List.iter (
        function (name, l) -> print_string ("\t"^name^"\n\t\t"); print_keys (l env);print_newline (); 
    ) [("named_types", named_types)];
    
    print_string "}";
    print_newline ()
*)

let rec peval_place peval_value env ({ AstUtils.place ; AstUtils.value}: 'a AstUtils.placed) = 
    let env, value = peval_value env place value in
    env, {AstUtils.place; AstUtils.value}

let rec peval_composed_type env place : _composed_type -> env * _composed_type = function
| TActivationInfo mt -> env, TActivationInfo ((snd <-> pe_mtype env) mt)
| TArrow (mt1, mt2) -> 
    let _, mt1 = pe_mtype env mt1 in
    let _, mt2 = pe_mtype env mt2 in
    env, TArrow (mt1, mt2)
| TVar x ->  env, TVar x (*TVar is process by the pe_main_type because it should be able to be evaluated to any type*)

| TFlatType ft -> env, TFlatType ft 
| TDict (mt1, mt2) -> 
    let _, mt1 = pe_mtype env mt1 in
    let _, mt2 = pe_mtype env mt2 in
    env, TDict (mt1, mt2)
| TList mt ->
    let _, mt = pe_mtype env mt in 
    env, TList mt 
| TOption mt ->
    let _, mt = pe_mtype env mt in 
    env, TOption mt
| TResult (mt1, mt2) -> 
    let _, mt1 = pe_mtype env mt1 in
    let _, mt2 = pe_mtype env mt2 in
    env, TResult (mt1, mt2)
| TSet mt ->
    let _, mt = pe_mtype env mt in 
    env, TSet mt
| TTuple mts ->
    let mts = List.map (snd <-> pe_mtype env) mts in
    env, TTuple mts

(** Message-passing *)
| TBridge { in_type; out_type; protocol} ->
    env, TBridge {
        in_type = (snd <-> pe_mtype env) in_type;
        out_type = (snd <-> pe_mtype env) out_type;
        protocol = (snd <-> pe_mtype env) protocol 
    }
and pe_ctype env: composed_type -> env * composed_type = peval_place peval_composed_type env


and peval_stype env place : _session_type -> env * _session_type = 
    let aux_entry (x, st, cst_opt) = 
        let _, st' = pe_stype env st in
        let cst_opt' = Option.map (fun elmt -> snd (peval_applied_constraint env elmt)) cst_opt in
        (x, st', cst_opt')
    in function  
    | STEnd -> env, STEnd
    | STInline x -> begin
        try
            let mt = Env.find x env.named_types in
            (* assert_is_stype mt.value;
            env, mt.value*)
            match mt with 
            | Some {value=SType st; _} -> env, st.value 
            (* FIXME do we want to have the place of the inline (current behviour) or the place of the typealias ??*)
            | _ -> Error.error place "STInline parameter can not be resolved to a session types. It is resolved to %s" (Error.show place)
            
        with Not_found -> raise (Error.PlacedDeadbranchError (place, "Unbounded inline variable, this should have been checked by the cook pass."))
    end
    | STBranch entries -> 
        env, STBranch (List.map aux_entry entries) 
    | STTimeout (time, st) -> begin 
        let _, time = pe_expr env time in
        match time.value with 
        | LitExpr _ -> env, STTimeout (time, snd (pe_stype env st))
        | _ -> Error.error time.place "the time argument of a timeout can not be resolve during partial evaluation" 
    end    
    | STRec (x, st) -> 
        env, STRec (x, snd (pe_stype env st))
    | STRecv (mt, st) -> 
        let _, mt' = pe_mtype env mt in
        let _, st' = pe_stype env st in
        env, STRecv (mt', st')
    | STSelect entries -> 
        env, STSelect (List.map aux_entry entries) 
    | STSend (mt, st) -> 
        let _, mt' = pe_mtype env mt in
        let _, st' = pe_stype env st in
        env, STSend (mt', st')
    | STVar (x, constraint_opt) -> 
        env, STVar (x, Option.map (function elmt -> snd (peval_applied_constraint env elmt)) constraint_opt )
and pe_stype env: session_type -> env * session_type = peval_place peval_stype env

and is_type_of_component x = Str.string_match (Str.regexp "[A-Z].*") (Atom.hint x) 0

and peval_mtype env place : _main_type -> env * _main_type = function 
| CType {value=TVar x; _} as mt when not(Atom.is_builtin x) && not(is_type_of_component x) -> (* when the type do not denote a  x) 0component *)
    (* get ride of types aliasing *)
    (* TODO rename aux *)
    let rec aux already_seen x =
        if (Atom.Set.find_opt x already_seen) <> None then
            Error.error place "cyclic type alias detected"
        ;
        let already_seen = Atom.Set.add x already_seen in
            
        try 
            let mt1_opt = Env.find x env.named_types in    
            match mt1_opt with 
            | None -> mt (* type alias with alias in impl or typedef *)
            (* Some implies type alias *)
            | Some {value=CType {value=TVar y; _}; _} -> 
                aux already_seen y (* keep searching the root *)
            | Some mt1 -> mt1.value (* NB: we use the place of the alias and not the place of the definition here *)
        with Not_found -> logger#error "Unbound type variable %s" (Atom.to_string x); raise (Error.PlacedDeadbranchError (place, "Unbounded type variable, this should have been checked by the cook pass."))
    in
    env, aux Atom.Set.empty x 
| CType ct -> env, CType (snd (pe_ctype env ct))
| SType st -> env, SType (snd(pe_stype env st)) 
| ConstrainedType (mt, cst) -> env, ConstrainedType (
    snd(pe_mtype env mt), 
    snd (peval_applied_constraint env cst)) 
| elmt -> env, elmt (*TODO*)
and pe_mtype env: main_type -> env * main_type = peval_place peval_mtype env

(******************************** Constraints ********************************)
and peval_applied_constraint env : applied_constraint -> env * applied_constraint = function x -> env, x 

(************************************ Expr & Stmt *****************************)
and is_terminal_expr : _expr -> bool = function
| LitExpr _ -> true 
| BlockExpr (List, items) | BlockExpr (Set, items) | BlockExpr (Tuple, items) ->
    List.fold_left (fun flag (item:expr) -> flag && is_terminal_expr item.value) true items
| Block2Expr (Dict, items) -> 
    List.fold_left (fun flag ((key,value):expr*expr) -> flag && is_terminal_expr key.value && is_terminal_expr value.value) true items
| OptionExpr None -> true
| OptionExpr Some e -> is_terminal_expr e.value
| ResultExpr (Some e, None) | ResultExpr (None, Some e) -> is_terminal_expr e.value 
| ResultExpr _ -> raise (Error.DeadbranchError "Result can not be err and ok at the same nor be neither ok nor err")

and peval_unop env place (op: unop) (e: expr) : env * _expr =
match (op, e.value) with
| Not, (LitExpr {value = BoolLit b; _}) -> env, LitExpr {place=e.place; value = BoolLit (not b); }
| UnpackResult, (ResultExpr (ok_opt, err_opt) as re) -> 
    if is_terminal_expr re then
        env, (match (ok_opt, err_opt) with 
        | Some e, None | None, Some e -> e.value (* TODO here we should return e.place *)
        | _ -> raise (Error.PlacedDeadbranchError (place, "Result can not be err and ok at the same neither not an err nor not an ok"))
        )
    else
        env, UnopExpr (op, e)
| _ -> env, UnopExpr (op, e) 
and peval_binop env place (e1: expr) (op: binop) (e2: expr) : env * _expr =
match (e1.value, op, e2.value) with
(* && shortcut*)
| (LitExpr {value = BoolLit false; _}), And, _ | _, And, LitExpr {value = BoolLit false; _} -> env, LitExpr {place=Error.forge_place "peval_binop" 0 0; value=BoolLit false}
| (LitExpr {value = BoolLit true; _}), And, e | e, And, LitExpr {value = BoolLit true; _} -> env, e  
(* || shortcut *)
| (LitExpr {value = BoolLit true; _}), And, _ | _, And, LitExpr {value = BoolLit true; _} -> env, LitExpr {place=Error.forge_place "peval_binop" 0 0; value=BoolLit true}
| (LitExpr {value = BoolLit false; _}), And, e | e, And, LitExpr {value = BoolLit false; _} -> env, e  
(* Integer computation *)
| (LitExpr {value = IntLit i1; _}), Plus, (LitExpr {value = IntLit i2; _}) -> env, LitExpr {place=Error.forge_place "peval_binop" 0 0; value=IntLit (i1+i2)}
| (LitExpr {value = IntLit i1; _}), Minus, (LitExpr {value = IntLit i2; _}) -> env, LitExpr {place=Error.forge_place "peval_binop" 0 0; value=IntLit (i1-i2)}
| (LitExpr {value = IntLit i1; _}), Mult, (LitExpr {value = IntLit i2; _}) -> env, LitExpr {place=Error.forge_place "peval_binop" 0 0; value=IntLit (i1*i2)}
| (LitExpr {value = IntLit i1; _}), Divide, (LitExpr {value = IntLit i2; _}) -> 
    if i2 = 0 then
        Error.error place "division by zero, partial evaluation can not continue"
    else
    env, LitExpr {place=Error.forge_place "peval_binop" 0 0; value=IntLit (i1/i2)}
(* Float computation *)
| (LitExpr {value = FloatLit f1; _}), Plus, (LitExpr {value = FloatLit f2; _}) -> env, LitExpr {place=Error.forge_place "peval_binop" 0 0; value=FloatLit (f1+.f2)}
| (LitExpr {value = FloatLit f1; _}), Minus, (LitExpr {value = FloatLit f2; _}) -> env, LitExpr {place=Error.forge_place "peval_binop" 0 0; value=FloatLit (f1-.f2)}
| (LitExpr {value = FloatLit f1; _}), Mult, (LitExpr {value = FloatLit f2; _}) -> env, LitExpr {place=Error.forge_place "peval_binop" 0 0; value=FloatLit (f1*.f2)}
| (LitExpr {value = FloatLit f1; _}), Divide, (LitExpr {value = FloatLit f2; _}) -> 
    if f2 = 0. then
        Error.error place "division by zero, partial evaluation can not continue"
    else
    env, LitExpr {place=Error.forge_place "peval_binop" 0 0; value=FloatLit (f1/.f2)}
(* Equality *)
| (LitExpr {value = l1; _}), Equal, (LitExpr {value = l2; _}) -> 
    env, LitExpr {place=Error.forge_place "peval_binop" 0 0; value= BoolLit (l1 = l2) }
(* Comparison *)
| (LitExpr {value = l1; _}), GreaterThan, (LitExpr {value = l2; _}) ->
    env, LitExpr {place=Error.forge_place "peval_binop" 0 0; value= BoolLit(
    match (l1, l2) with
    | EmptyLit, EmptyLit -> false 
    | IntLit x, IntLit y -> x > y 
    | FloatLit x, FloatLit y -> x > y 
    | StringLit x, StringLit y -> x > y
    | _ -> Error.error place "greater than is not implemented fot this type of literals"
    )}
| (LitExpr {value = l1; _}), GreaterThanEqual, (LitExpr {value = l2; _}) ->
    env, LitExpr {place=Error.forge_place "peval_binop" 0 0; value= BoolLit(
    match (l1, l2) with
    | EmptyLit, EmptyLit -> true 
    | IntLit x, IntLit y -> x >= y 
    | FloatLit x, FloatLit y -> x >= y 
    | StringLit x, StringLit y -> x >= y
    | _ -> Error.error place "greater than equal is not implemented fot this type of literals"
    )}
| (LitExpr {value = l1; _}), LessThan, (LitExpr {value = l2; _}) ->
    env, LitExpr {place=Error.forge_place "peval_binop" 0 0; value= BoolLit(
    match (l1, l2) with
    | EmptyLit, EmptyLit -> false 
    | IntLit x, IntLit y -> x < y 
    | FloatLit x, FloatLit y -> x < y 
    | StringLit x, StringLit y -> x < y
    | _ -> Error.error place "less than is not implemented fot this type of literals"
    )}
| (LitExpr {value = l1; _}), LessThanEqual, (LitExpr {value = l2; _}) ->
    env, LitExpr {place=Error.forge_place "peval_binop" 0 0; value= BoolLit(
    match (l1, l2) with
    | EmptyLit, EmptyLit -> true 
    | IntLit x, IntLit y -> x <= y 
    | FloatLit x, FloatLit y -> x <= y 
    | StringLit x, StringLit y -> x <= y
    | _ -> Error.error place "less than equal is not implemented fot this type of literals"
    )}
(* TODO deal with list, dict and so on for equality *)
| (LitExpr {value = l1; _}), In, BlockExpr (Block, _) -> Error.error place "right-hand side of in must be an iterable"
| (LitExpr {value = l1; _}), In, BlockExpr (List, items) | (LitExpr {value = l1; _}), In, BlockExpr (Set, items) -> 
    if is_terminal_expr e2.value then
        env, LitExpr {place=Error.forge_place "peval_binop" 0 0; value= BoolLit (
            List.find_opt (function (item:expr) -> match item.value with |LitExpr {value = l2; _} -> l2 = l1 | _ -> false ) items <> None 
        )}
    else
        env, BinopExpr (e1, op, e2)
| (LitExpr {value = l1; _}), In, BlockExpr (Tuple, _) -> Error.error place "right-hand side of in must be an iterable"
| (LitExpr {value = l1; _}), In, Block2Expr (Dict, items) ->
    if is_terminal_expr e2.value then
        env, LitExpr {place=Error.forge_place "peval_binop" 0 0; value= BoolLit (
            List.find_opt (function ((key, _):expr*expr) -> match key.value with |LitExpr {value = l2; _} -> l2 = l1 | _ -> false ) items <> None 
        )}
    else
        env, BinopExpr (e1, op, e2)
| _ -> env, BinopExpr (e1, op, e2)

and peval_call env place (fct: expr) (args: expr list) : env * _expr =
match fct.value, args with
| VarExpr name, [] when Atom.hint(name) = "bridge" ->
    env, LitExpr {
        place;    
        value = Bridge {
            id  = Atom.fresh "bridge";
            protocol = { place = Error.forge_place "partial_eval/peval_call/default_protocol" 0 0; value = STEnd}; (* Should be update afterward by using type annotation *)
        } 
    }
| _ -> env, CallExpr (fct, args)

and peval_expr env place : _expr -> env * _expr = function 
| AccessExpr (e1, e2) -> 
    let _, e1 = pe_expr env e1 in
    let _, e2 = pe_expr env e2 in
    env, AccessExpr (e1, e2)
| BinopExpr (e1, op, e2) -> 
    let _, e1 = pe_expr env e1 in
    let _, e2 = pe_expr env e2 in
    peval_binop env place e1 op e2
| BlockExpr (block_type, items) ->
    env, BlockExpr (block_type, 
        List.map (function (item:expr) -> snd (pe_expr env item)) items 
    )
| Block2Expr (block_type, items) ->
    env, Block2Expr (block_type, 
        List.map (function (item1,item2:expr*expr) -> 
            snd (pe_expr env item1), 
            snd (pe_expr env item2)
        ) items 
    )
| BoxCExpr _ -> failwith "box is not supported by partial eval"
| CallExpr (fct, args) -> 
    let _, fct = pe_expr env fct in
    let args = List.map (function arg -> snd (pe_expr env arg)) args in
    peval_call env place fct args
| LambdaExpr (x, stmt) ->
    let _, stmt = pe_stmt env stmt in
    env, LambdaExpr (x, stmt)
| LitExpr lit -> env, LitExpr lit
| OptionExpr e_opt -> env, OptionExpr (Option.map (function e -> snd(pe_expr env e)) e_opt)
| ResultExpr (ok_opt, err_opt) -> 
    env, ResultExpr (
        (Option.map (function e -> snd(pe_expr env e)) ok_opt),
        (Option.map (function e -> snd(pe_expr env e)) err_opt)
    )
| Spawn {c; args; at} -> 
    (* TODO peval c *)
    let args = List.map (function arg -> snd (pe_expr env arg)) args in
    let at = Option.map (function at -> snd(pe_expr env at)) at in
    env, Spawn {c; args; at}
| This -> env, This
| UnopExpr (op, e) -> 
    let _, e = pe_expr env e in
    peval_unop env place op e
| VarExpr x -> begin
    try 
        let t_e = Env.find x env.terminal_expr_assignements in
        env, t_e.value (*TODO should be t_e.place @ place @ partial_eval_var place *)
    with | Not_found -> env, VarExpr x
end
| x -> env, x 
and pe_expr env: expr -> env * expr = peval_place peval_expr env

and peval_stmt env place : _stmt -> env * _stmt = function 
| EmptyStmt -> env, EmptyStmt
| AssignExpr (x, e) -> 
    let e = snd(pe_expr env e) in
    if is_terminal_expr e.value then
        let new_env = bind_terminal_expr env x e in 
        new_env, EmptyStmt 
    else
        env, AssignExpr (x, e)
| AssignThisExpr (x, e) -> env, AssignExpr (x,  snd(pe_expr env e ))
| BlockStmt stmts -> 
    let stmts = List.map (function stmt -> snd ((pe_stmt env) stmt)) stmts in

    (* Cleansing: removing empty stmt *)
    let stmts = List.filter (function | {AstUtils.value=EmptyStmt;_} -> false | _-> true) stmts in

    (* Stop at exit *)
    let _, stmts, _ = List.fold_left (fun (seen_exit, acc_before, acc_after) -> 
            if seen_exit then function stmt -> (seen_exit, acc_before, stmt::acc_after)
            else function   | {AstUtils.value=ExitStmt i;_} as stmt -> (true, acc_before, stmt::acc_after)
                            | stmt -> (seen_exit, stmt::acc_before, acc_after) 
        ) (false, [],[]) stmts in
    let stmts = List.rev stmts in

    env, BlockStmt stmts 
| BreakStmt -> env, BreakStmt
| CommentsStmt c -> env, CommentsStmt c 
| ContinueStmt -> env, ContinueStmt
| ExitStmt i -> env, ExitStmt i
| ExpressionStmt e -> env, ExpressionStmt (snd(pe_expr env e))
| ForStmt (x, e, stmt) -> env, ForStmt (x, snd (pe_expr env e), snd (pe_stmt env stmt)) (* TODO *)
| IfStmt (e, stmt, stmt_opt) -> begin 
    let e = snd (pe_expr env e) in
    let stmt = snd (pe_stmt env stmt) in
    let stmt_opt = Option.map (function elmt -> snd (pe_stmt env elmt)) stmt_opt in 
    match e.value with
    | LitExpr {value=BoolLit true; _} -> env, stmt.value (* TODO should be stmt.place *)
    | LitExpr {value=BoolLit false; _} -> env, Option.value (Option.map (fun (x:stmt) -> x.value) stmt_opt) ~default:EmptyStmt (*TODO should be stmt.place *)
    | _ -> env, IfStmt (e, stmt, stmt_opt)
end
| LetExpr ({value=CType {value= TBridge t_b; _}; _ } as let_left, let_x, e_b) -> begin 
    let _, let_left = pe_mtype env let_left in
    let _, e_b = pe_expr env e_b in

    match let_left.value with
    | CType {value= TBridge t_b; _} -> begin
        match e_b.value with
        | LitExpr {value=Bridge bridge; _} -> 
            let protocol = match t_b.protocol.value with
            | SType st -> st
            | _ -> Error.error t_b.protocol.place "Third argument of Bridge<_,_,_> must be (partially-evaluated> to a session type"
            in
            let bridge = LitExpr {place = e_b.place@let_left.place; value = Bridge {bridge with protocol = protocol}} in

            env, LetExpr (
                let_left,
                let_x, 
                {place = e_b.place; value = bridge} 
            )
        | _ -> Error.error place "The right-handside of a Bridge<_,_,_> must be partially evaluated to a bridge literal" 
    end
    | _-> 
        if is_terminal_expr e_b.value then
            let new_env = bind_terminal_expr env let_x e_b in
            new_env, LetExpr (let_left, let_x, e_b) (* TODO removing a let needs us to know if their is an assigned somewhere *)
        else env, LetExpr( let_left, let_x, e_b)
end
| MatchStmt (e, entries) -> (* TODO *) 
    env, MatchStmt (
        snd(pe_expr env e), 
        List.map ( function (e,stmt) -> snd(pe_expr env e), snd(pe_stmt env stmt)) entries
    )
| ExpressionStmt e -> 
    let new_env, new_e = pe_expr env e in
    new_env, (ExpressionStmt new_e)
| x -> env, x
and pe_stmt env: stmt -> env * stmt = peval_place peval_stmt env


(************************************ Component *****************************)
and peval_contract env place contract =
    let pre_binders = List.map (
        function  (mt, x, e) -> 
            snd (pe_mtype env mt),
            x,
            snd (pe_expr env e )
        ) contract.pre_binders in
    let ensures = Option.map (function e -> snd (pe_expr env e )) contract.ensures in
    let returns = Option.map (function e -> snd (pe_expr env e )) contract.returns in

    let clean_predicate = function
    | Some {AstUtils.value=LitExpr {value=BoolLit true; _}; _} -> None
    | Some {AstUtils.value=LitExpr {value=BoolLit false; _}; place} -> Error.error place "ensures expresion has been evaluated to false" 
    | Some {value=LitExpr _; place} -> Error.error place "ensures expr has been evaluated to a non boolean literal"
    | pred_opt -> pred_opt
    in

    env, {contract with
        pre_binders;
        ensures = clean_predicate ensures;
        returns = clean_predicate returns 
    }
and pe_contract env: contract -> env * contract = peval_place peval_contract env

and peval_param env place (mt, x) = 
    env, (snd(pe_mtype env mt), x)
and pe_param env: param -> env * param = peval_place peval_param env

and peval_method env place = function
| CustomMethod m -> 
    let contract_opt = Option.map (function c -> snd(pe_contract env c)) m.contract_opt in
    (* Cleansing: elminates empty contract *)
    let contract_opt = match contract_opt with
    | Some c when c.value.ensures = None && c.value.returns = None -> None
    | c_opt -> c_opt 
    in

    env, CustomMethod {m with
            ret_type = snd(pe_mtype env m.ret_type);
            args = List.map (function param -> snd(pe_param env param)) m.args;
            body = List.map (snd <-> pe_stmt env) m.body;
            contract_opt = contract_opt
    } 
| OnStartup m -> env, OnStartup (snd (pe_method env m)) 
| OnDestroy m -> env, OnDestroy (snd (pe_method env m))
and pe_method env: method0 -> env * method0 = peval_place peval_method env

and peval_port env place port = 
    let expecting_st = snd(pe_mtype env port.expecting_st) in 
    
    begin
        match expecting_st.value with
        | SType {value=STEnd; _} -> Error.error place "a port can not expect the end of a protocol, no message will be send"
        | SType _ -> ()
        | _ -> Error.error place "port expecting value must be a session type"
    end;

    env, { port with
        input =  snd(pe_expr env port.input);
        expecting_st;
        callback = snd(pe_expr env port.callback)
    }
and pe_port env: port -> env * port = peval_place peval_port env

and peval_state env place = function 
| StateDcl s -> env, StateDcl {s with 
    type0 = snd(pe_mtype env s.type0);
    body = Option.map (function e -> snd(pe_expr env e)) s.body
} 
| StateAlias _ -> failwith "partial-evaluation does not support yet StateAlias" 
and pe_state env: state -> env * state = peval_place peval_state env

and peval_component_item env place : _component_item -> env * _component_item = function 
| Contract c -> raise (Error.PlacedDeadbranchError (place, "contract should be paired with method before partial_evaluation, therefore no contract should remains as a component_item"))
| Include cexpr -> env, Include (snd(pe_component_expr env cexpr))
| Method m -> env, Method (snd(pe_method env m))
| Port p -> env, Port (snd(pe_port env p))
| State s -> env, State (snd(pe_state env s))
| Term t -> env, Term (snd(pe_term env t))

and pe_component_item env: component_item -> env * component_item = peval_place peval_component_item env

and peval_component_dcl env place : _component_dcl -> env * _component_dcl = function  
| ComponentAssign {name; args; value} -> env, ComponentAssign {
    name;
    args = List.map (function param -> snd(pe_param env param)) args;
    value = snd(pe_component_expr env value) 
} 
| ComponentStructure cdcl ->
    (* Collect contracts *)
    let collect_contracts env (x:component_item) = 
        match x.value with
        | Contract c -> Env.add c.value.method_name c env 
        | _ -> env 
    in
    let contracts : IR.contract Env.t = List.fold_left collect_contracts Env.empty cdcl.body in (* method_name -> contract *)

    (* Remove contracts from body and pair method with contracts *)
    let rec get_method_name (m: method0) = 
        match m.value with
        | CustomMethod m -> m.name
        | OnDestroy m | OnStartup m -> get_method_name m
    in
    let body = List.filter_map (function (item:component_item) ->
        match item.value with 
        | Contract _ -> None 
        | Method m -> begin
            let rec aux (m: method0) = 
                match m.value with
                | CustomMethod _m -> begin
                        let contract : contract = (Env.find _m.name contracts) in
                        { AstUtils.place; value = (CustomMethod { _m with contract_opt = Some contract }) }
                end 
                | OnDestroy m -> { AstUtils.place; value = OnDestroy (aux m) }
                | OnStartup m -> { AstUtils.place; value = OnStartup (aux m) }
            in
            try
                Some { AstUtils.place; value = Method (aux m) }
            with Not_found ->
                Some item 
        end
        | x -> Some item 
        ) cdcl.body in
    
    let new_env, citems = List.fold_left_map pe_component_item env body in 
    new_env, ComponentStructure {cdcl with body = citems }

and pe_component_dcl env: component_dcl -> env * component_dcl = peval_place peval_component_dcl env

(********************** Manipulating component structure *********************)
and peval_component_expr env place = function (* TODO peval for this*)
| VarCExpr x -> env, VarCExpr x
| AppCExpr (cexpr1, cexpr2) -> env, AppCExpr (snd (pe_component_expr env cexpr1), snd (pe_component_expr env cexpr2)) 
| UnboxCExpr e -> env, UnboxCExpr (snd(pe_expr env e)) 
| AnyExpr e -> env, AnyExpr (snd(pe_expr env e)) 
and pe_component_expr env: component_expr -> env * component_expr = peval_place peval_component_expr env

(************************************ Program *****************************)
and peval_term env place : _term -> env * _term = function
| Comments c -> env, Comments c
| Stmt stmt -> map_snd (fun x -> Stmt x) (pe_stmt env stmt)
| Component comp -> map_snd (fun x -> Component x) (pe_component_dcl env comp)
| Typealias (x, mt_opt) -> begin
    match mt_opt with
    | None -> 
        let new_env = bind_named_types env x None in
        new_env, Typealias (x, None)
    | Some mt -> 
        let _, mt = pe_mtype env mt in 
        let new_env = bind_named_types env x (Some mt) in
        new_env, EmptyTerm (*Typealias (x, Some mt)*)
end
| Typedef {value= ClassicalDef (x, args, ()) as tdef; place} | Typedef {value= EventDef (x, args, ()) as tdef; place} -> 
    let new_env = bind_named_types env x None in
    let args = List.map (function mt -> snd(pe_mtype env mt)) args in

    new_env, Typedef ({ place; value = 
    match tdef with 
        | ClassicalDef _ -> ClassicalDef (x, args, ())
        | EventDef _ -> EventDef (x, args, ())
    })
and pe_term env: term -> env * term = peval_place peval_term env

and pe_terms env terms : env * IR.term list =
    let env, program = List.fold_left_map pe_term (fresh_env ()) terms in
    env, List.filter (function |{AstUtils.value=EmptyTerm; _} -> false | _-> true) program

and peval_program (terms: IR.program) : IR.program = 
    (*  Hydrate env, namely:
        -  collect the contract
        And remove contracts from component_item + add contract inside method0 structure *)
    let env, program = pe_terms (fresh_env ()) terms in
    program

