open Core
open IR
open Easy_logging
open Utils
open AstUtils
open IRMisc
 

let logger = Logging.make_logger "_1_ compspec.PartialEval" Debug [];;

(*
 TODO add 
 - Data-flow analysis - https://en.wikipedia.org/wiki/Data-flow_analysis
 - Static single assignment form : https://en.wikipedia.org/wiki/Static_single_assignment_form#Converting_out_of_SSA_form

*)


(***************************************************************)
let stinline_selector = function
    | SType {place; value=STInline _ } -> true 
    | _ -> false 
let stinline_collector parent_opt env {place; value} = 
match value with
| SType {place; value=STInline x } -> 
    let parent = match parent_opt with | None -> "Toplevel" | Some p -> Atom.to_string p in
    Error.error place "STInline remains in IR after parial evaluation : %s. Parent = %s" (Atom.to_string x) parent
| _ -> []

let check_program program : unit= 
    (* Check: no more STInline *)
    ignore (collect_type_program Atom.Set.empty stinline_selector stinline_collector program);
    ()


(***************************************************************)




(* Environment *)
module Env = Atom.VMap 

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

let print_env env =
    let print_keys env = Env.iter (fun x _ -> Printf.printf "%s;" (Atom.to_string x)) env in

    print_newline ();
    print_string "Env = {";

    List.iter (
        function (name, l) -> print_string ("\t"^name^"\n\t\t"); print_keys (l env);print_newline (); 
    ) [("named_types", named_types)];
    List.iter (
        function (name, l) -> print_string ("\t"^name^"\n\t\t"); print_keys (l env);print_newline (); 
    ) [("terminal_expr_assignements", terminal_expr_assignements)];
    
    print_string "}";
    print_newline ()

let rec peval_composed_type env place : _composed_type -> env * _composed_type = function
| TActivationRef mt -> env, TActivationRef ((snd <-> pe_mtype env) mt)
| (TArrow (mt1, mt2) as ct) | (TUnion (mt1, mt2) as ct)-> 
    let _, mt1 = pe_mtype env mt1 in
    let _, mt2 = pe_mtype env mt2 in
    env, (match ct with 
        | TArrow _ -> TArrow (mt1, mt2)
        | TUnion _ -> TUnion (mt1, mt2)) 
| TVar x ->  env, TVar x (*TVar is process by the pe_main_type because it should be able to be evaluated to any type*)

| TFlatType ft -> env, TFlatType ft 
| TDict (mt1, mt2) -> 
    let _, mt1 = pe_mtype env mt1 in
    let _, mt2 = pe_mtype env mt2 in
    env, TDict (mt1, mt2)
| TArray mt ->
    let _, mt = pe_mtype env mt in 
    env, TArray mt 
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
| TBridge { in_type; out_type; protocol} -> begin
    logger#debug "TBridge detected at : %s" (Error.show place);
    let protocol = (snd <-> pe_mtype env) protocol in
    match protocol.value with 
    | SType st -> 
        env, TBridge {
            in_type = (snd <-> pe_mtype env) in_type;
            out_type = (snd <-> pe_mtype env) out_type;
            protocol = protocol  
        }
    | _ -> Error.error place "Third argument of Bridge<_,_,_> must be (partially-evaluated> to a session type\n%s" (show_main_type protocol)
end
| TVPlace mt -> env, TVPlace ((snd <-> pe_mtype env) mt)
| TForall (x, mt) -> env, TForall (x, snd (pe_mtype env mt))
(* no inner_env with x for pe_eval mt because x is not binded to any concrete type at this point *)
| TPolyVar x -> env, TPolyVar x
| TInport mt ->
    env, TInport ((snd <-> pe_mtype env) mt)
| TOutport -> env, TOutport 
and pe_ctype env: composed_type -> env * composed_type = map2_place (peval_composed_type env)


and peval_stype env place : _session_type -> env * _session_type = 
    let aux_entry (x, st, cst_opt) = 
        let _, st' = pe_stype env st in
        let cst_opt' = Option.map (fun elmt -> snd (peval_applied_constraint env elmt)) cst_opt in
        (x, st', cst_opt')
    in function  
    | STEnd -> env, STEnd
    | STWildcard -> env, STWildcard
    | STBottom -> env, STBottom
    | STInline x -> begin
        try
            let mt = Env.find x env.named_types in
            match mt with 
            | Some {value=SType st; _} -> 
                let st = (snd <-> pe_stype env) st in

                (* FIXME is this debug code ?? i don't remember ... *)
                ignore (collect_type_mtype None Atom.Set.empty stinline_selector stinline_collector {value=SType st; place});
                
                env, st.value 
            (* FIXME do we want to have the place of the inline (current behviour) or the place of the typealias ??*)
            | _ -> Error.error place "STInline parameter can not be resolved to a session types."
        with Not_found -> raise (Error.PlacedDeadbranchError (place, (Printf.sprintf "Unbounded inline variable [%s], this should have been checked by the cook pass." (Atom.to_string x))))
    end
    | STDual st ->
        (* Dual elimination *)
        let _, st' = pe_stype env st in
        env, (dual st').value
    | STBranch entries -> 
        env, STBranch (List.map aux_entry entries) 
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
    | STVar x -> 
        env, STVar x
    | STPolyVar x -> env, STPolyVar x
and pe_stype env: session_type -> env * session_type = map2_place (peval_stype env)

and peval_cmtype env place = function 
| CompTUid x -> env, CompTUid x (* can not inlined since rec type are allowed for component *) 
| TStruct (name, sign) -> 
    env, TStruct (name, (Atom.VMap.map (snd <-> pe_mtype env) sign))
| TPolyCVar x -> env, TPolyCVar x
and pe_cmtype env = map2_place (peval_cmtype env)

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
        with Not_found -> logger#error "Unbound type variable %s" (Atom.to_string x); raise (Error.PlacedDeadbranchError (place, Printf.sprintf "Unbounded type variable [%s], this should have been checked by the cook pass." (Atom.to_string x)))
    in
    env, aux Atom.Set.empty x 
| CType ct -> env, CType (snd (pe_ctype env ct))
| SType st -> 
    let st = SType (snd(pe_stype env st)) in 
    env, st
| ConstrainedType (mt, cst) -> env, ConstrainedType (
    snd(pe_mtype env mt), 
    snd (peval_applied_constraint env cst)) 
| CompType cmt -> env, CompType (snd (pe_cmtype env cmt))
| EmptyMainType -> env, EmptyMainType
and pe_mtype env: main_type -> env * main_type = map2_place (peval_mtype env)

(******************************** Constraints ********************************)
and peval_applied_constraint env : applied_constraint -> env * applied_constraint = function x -> env, x 

(************************************ Expr & Stmt *****************************)
and is_terminal_expr (e, mt): bool = 
    match e with
    | LitExpr _ -> true 
    | BlockExpr (List, items) | BlockExpr (Set, items) | BlockExpr (Tuple, items) ->
        List.fold_left (fun flag (item:expr) -> flag && is_terminal_expr item.value) true items
    | Block2Expr (Dict, items) -> 
        List.fold_left (fun flag ((key,value):expr*expr) -> flag && is_terminal_expr key.value && is_terminal_expr value.value) true items
    | OptionExpr None -> true
    | OptionExpr Some e -> is_terminal_expr e.value
    | ResultExpr (Some e, None) | ResultExpr (None, Some e) -> is_terminal_expr e.value 
    | ResultExpr _ -> raise (Error.DeadbranchError "Result can not be err and ok at the same nor be neither ok nor err")
    | _ -> false 

and peval_unop env place (op: unop) (e: expr) : env * (_expr * main_type) =
match (op, e.value) with
| Not, (LitExpr {value = BoolLit b; _}, mt) -> env, (LitExpr {place=e.place; value = BoolLit (not b); }, mt)
| UnpackOrPropagateResult, (ResultExpr (ok_opt, err_opt) as re, mt) -> 
    if is_terminal_expr (re, mt) then
        env, ((match (ok_opt, err_opt) with 
        | Some e, None | None, Some e -> fst e.value (* TODO here we should return e.place *)
        | _ -> raise (Error.PlacedDeadbranchError (place, "Result can not be err and ok at the same neither not an err nor not an ok"))
        ), mt)
    else
        env, (UnopExpr (op, e), mt)
| _, (_,mt) -> env, (UnopExpr (op, e),mt)
and peval_binop env place (e1: expr) (op: binop) (e2: expr) (mt_binop:main_type) : env * (_expr * main_type) =
    let fplace = (Error.forge_place "Plg=Peval" 0 0) in
    let auto_fplace smth = {AstUtils.place = place@fplace; value=smth} in

    match (e1.value, op, e2.value) with
    (* && shortcut*)
    | (LitExpr {value = BoolLit false; _}, _), And, _ | _, And, (LitExpr {value = BoolLit false; _},_) -> env, (LitExpr {place=Error.forge_place "peval_binop" 0 0; value=BoolLit false}, mt_binop)
    | (LitExpr {value = BoolLit true; _}, _), And, e | e, And, (LitExpr {value = BoolLit true; _}, _) -> env, e  
    (* || shortcut *)
    | (LitExpr {value = BoolLit true; _}, _), And, _ | _, And, (LitExpr {value = BoolLit true; _}, _) -> env, (LitExpr {place=Error.forge_place "peval_binop" 0 0; value=BoolLit true}, mt_binop)
    | (LitExpr {value = BoolLit false; _}, _), And, e | e, And, (LitExpr {value = BoolLit false; _}, _) -> env, e  
    (* Integer computation *)
    | (LitExpr {value = IntLit i1; _}, _), Plus, (LitExpr {value = IntLit i2; _}, _) -> env, (LitExpr {place=Error.forge_place "peval_binop" 0 0; value=IntLit (i1+i2)}, mt_binop)
    | (LitExpr {value = IntLit i1; _}, _), Minus, (LitExpr {value = IntLit i2; _}, _) -> env, (LitExpr {place=Error.forge_place "peval_binop" 0 0; value=IntLit (i1-i2)}, mt_binop)
    | (LitExpr {value = IntLit i1; _}, _), Mult, (LitExpr {value = IntLit i2; _}, _) -> env, (LitExpr {place=Error.forge_place "peval_binop" 0 0; value=IntLit (i1*i2)}, mt_binop)
    | (LitExpr {value = IntLit i1; _}, _), Divide, (LitExpr {value = IntLit i2; _}, _) -> 
        if i2 = 0 then
            Error.error place "division by zero, partial evaluation can not continue"
        else
        env, (LitExpr {place=Error.forge_place "peval_binop" 0 0; value=IntLit (i1/i2)}, mt_binop)
    (* Float computation *)
    | (LitExpr {value = FloatLit f1; _}, _), Plus, (LitExpr {value = FloatLit f2; _}, _) -> env, (LitExpr {place=Error.forge_place "peval_binop" 0 0; value=FloatLit (f1+.f2)}, mt_binop)
    | (LitExpr {value = FloatLit f1; _}, _), Minus, (LitExpr {value = FloatLit f2; _}, _) -> env, (LitExpr {place=Error.forge_place "peval_binop" 0 0; value=FloatLit (f1-.f2)}, mt_binop)
    | (LitExpr {value = FloatLit f1; _}, _), Mult, (LitExpr {value = FloatLit f2; _}, _) -> env, (LitExpr {place=Error.forge_place "peval_binop" 0 0; value=FloatLit (f1*.f2)}, mt_binop)
    | (LitExpr {value = FloatLit f1; _}, _), Divide, (LitExpr {value = FloatLit f2; _}, _) -> 
        if f2 = 0. then
            Error.error place "division by zero, partial evaluation can not continue"
        else
        env, (LitExpr {place=Error.forge_place "peval_binop" 0 0; value=FloatLit (f1/.f2)}, mt_binop)
    (* Equality *)
    | (LitExpr {value = l1; _}, _), Equal, (LitExpr {value = l2; _}, _) -> 
        env, (LitExpr {place=Error.forge_place "peval_binop" 0 0; value= BoolLit (l1 = l2) }, mt_binop)
    (* Comparison *)
    | (LitExpr {value = l1; _}, _), GreaterThan, (LitExpr {value = l2; _}, _) ->
        env, (LitExpr {place=Error.forge_place "peval_binop" 0 0; value= BoolLit(
        match (l1, l2) with
        | IntLit x, IntLit y -> x > y 
        | FloatLit x, FloatLit y -> x > y 
        | StringLit x, StringLit y -> x > y
        | _ -> Error.error place "greater than is not implemented fot this type of literals"
        )}, mt_binop)
    | (LitExpr {value = l1; _}, _), GreaterThanEqual, (LitExpr {value = l2; _}, _) ->
        env, (LitExpr {place=Error.forge_place "peval_binop" 0 0; value= BoolLit(
        match (l1, l2) with
        | IntLit x, IntLit y -> x >= y 
        | FloatLit x, FloatLit y -> x >= y 
        | StringLit x, StringLit y -> x >= y
        | _ -> Error.error place "greater than equal is not implemented fot this type of literals"
        )}, mt_binop)
    | (LitExpr {value = l1; _}, _), LessThan, (LitExpr {value = l2; _}, _) ->
        env, (LitExpr {place=Error.forge_place "peval_binop" 0 0; value= BoolLit(
        match (l1, l2) with
        | IntLit x, IntLit y -> x < y 
        | FloatLit x, FloatLit y -> x < y 
        | StringLit x, StringLit y -> x < y
        | _ -> Error.error place "less than is not implemented fot this type of literals"
        )}, mt_binop)
    | (LitExpr {value = l1; _}, _), LessThanEqual, (LitExpr {value = l2; _}, _) ->
        env, (LitExpr {place=Error.forge_place "peval_binop" 0 0; value= BoolLit(
        match (l1, l2) with
        | IntLit x, IntLit y -> x <= y 
        | FloatLit x, FloatLit y -> x <= y 
        | StringLit x, StringLit y -> x <= y
        | _ -> Error.error place "less than equal is not implemented fot this type of literals"
        )}, mt_binop)
    (* TODO deal with list, dict and so on for equality *)
    | (LitExpr {value = l1; _}, _), In, (BlockExpr (Block, _), _) -> Error.error place "right-hand side of in must be an iterable"
    | (LitExpr {value = l1; _}, _), In, (BlockExpr (List, items), _) | (LitExpr {value = l1; _}, _), In, (BlockExpr (Set, items), _) -> 
        if is_terminal_expr e2.value then
            env, (LitExpr {place=Error.forge_place "peval_binop" 0 0; value= BoolLit (
                List.find_opt (function (item:expr) -> match item.value with |(LitExpr {value = l2; _}, _) -> l2 = l1 | _ -> false ) items <> None 
            )}, mt_binop) 
        else
            env, (BinopExpr (e1, op, e2), mt_binop)
    | (LitExpr {value = l1; _}, _), In, (BlockExpr (Tuple, _), _) -> Error.error place "right-hand side of in must be an iterable"
    | (LitExpr {value = l1; _}, _), In, (Block2Expr (Dict, items), _) ->
        if is_terminal_expr e2.value then
            env, (LitExpr {place=Error.forge_place "peval_binop" 0 0; value= BoolLit (
                List.find_opt (function ((key, _):expr*expr) -> match key.value with |(LitExpr {value = l2; _}, _) -> l2 = l1 | _ -> false ) items <> None 
            )}, mt_binop)
        else
            env, (BinopExpr (e1, op, e2), mt_binop)

    | _ -> env, (BinopExpr (e1, op, e2), mt_binop)

and peval_call env place (fct: expr) (args: expr list) (mt_ret:main_type): env * (_expr * main_type) =
match fct.value, args with
| (VarExpr name, _), [{value=(VarExpr protocol_name, _);}] when Atom.hint(name) = "bridge" ->
    env, (BridgeCall {
        protocol_name
    }, mt_ret)
| (VarExpr name, _), _ when Atom.hint(name) = "bridge" ->
    Error.error place "bridge should have exactly one argument - the protocol"
| _ -> env, (CallExpr (fct, args), mt_ret)

and peval_expr env place (e, mt) :  env * (_expr * main_type) = 
    let env, e = 
        match e with
        | AccessExpr (e1, e2) -> 
            let _, e1 = pe_expr env e1 in
            let _, e2 = pe_expr env e2 in
            env, AccessExpr (e1, e2)
        | BinopExpr (e1, op, e2) -> 
            let _, e1 = pe_expr env e1 in
            let _, e2 = pe_expr env e2 in
            let env, (e,_) = peval_binop env place e1 op e2 mt in
            env, e
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
            let env, (e,_) = peval_call env place fct args mt in
            env, e
        | LambdaExpr (params, e) ->
            let params = List.map (function param -> snd(pe_param env param)) params in
            let _, mt = pe_mtype env mt in 
            env, LambdaExpr (params, e)
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
            let env, (e, _) = peval_unop env place op e in
            env, e
        | VarExpr x -> begin
            logger#debug "pe var %s" (Atom.to_string x);
            print_env env;
            try 
                let t_e = Env.find x env.terminal_expr_assignements in
                logger#debug "replace var %s by terminal" (Atom.to_string x);
                env, fst t_e.value (*TODO should be t_e.place @ place @ partial_eval_var place *)
            with | Not_found -> env, VarExpr x
        end
        | x -> env, x 
    in
    env, (e, snd (pe_mtype env mt))
and pe_expr env: expr -> env * expr = map2_place (peval_expr env)

and peval_stmt env place : _stmt -> env * _stmt = 
    let clean_stmts stmts =
        (* Cleansing: removing empty stmt *)
        let stmts = List.filter (function | {AstUtils.value=EmptyStmt;_} -> false | _-> true) stmts in

        (* Stop at exit *)
        let _, stmts, _ = List.fold_left (fun (seen_exit, acc_before, acc_after) -> 
                if seen_exit then function stmt -> (seen_exit, acc_before, stmt::acc_after)
                else function   | {AstUtils.value=ExitStmt i;_} as stmt -> (true, acc_before, stmt::acc_after)
                                | stmt -> (seen_exit, stmt::acc_before, acc_after) 
            ) (false, [],[]) stmts in
        let stmts = List.rev stmts in
        stmts
    in
function 
| EmptyStmt -> env, EmptyStmt
| AssignExpr (x, e) -> 
    let e = snd(pe_expr env e) in
    if is_terminal_expr e.value then
        let new_env = bind_terminal_expr env x e in 
        new_env, EmptyStmt 
    else
        env, AssignExpr (x, e)
| AssignThisExpr (x, e) -> env, AssignExpr (x,  snd(pe_expr env e ))
| BranchStmt {s; label; branches} -> 
    let s = snd(pe_expr env s) in
    let label = snd(pe_expr env label) in

    (* each branch lives in its own isolated env *)
    let branches = List.map (fun {branch_label; branch_s; body} -> 
        {branch_label; branch_s; body = snd(pe_stmt env body)}
    ) branches in

    env, BranchStmt{ s; label; branches }
| BlockStmt stmts -> begin 
    let inner_env, stmts = List.fold_left_map (fun env stmt -> pe_stmt env stmt) env stmts in
    let stmts = clean_stmts stmts in

    match stmts with
    | [] -> env, EmptyStmt
    | [stmt] -> env, stmt.value
    | _ -> env, BlockStmt stmts
end
| BreakStmt -> env, BreakStmt
| CommentsStmt c -> env, CommentsStmt c 
| ContinueStmt -> env, ContinueStmt
| ExitStmt i -> env, ExitStmt i
| ExpressionStmt e -> env, ExpressionStmt (snd(pe_expr env e))
| ForStmt (mt, x, e, stmt) -> begin
    let stmt = snd (pe_stmt env stmt) in
    match stmt.value with
    | EmptyStmt -> env, EmptyStmt
    | BlockStmt [] -> env, EmptyStmt
    | _ -> env, ForStmt (snd (pe_mtype env mt), x, snd (pe_expr env e), stmt)
end
| IfStmt (e, stmt, stmt_opt) -> begin 
    let e = snd (pe_expr env e) in
    let stmt = snd (pe_stmt env stmt) in
    let stmt_opt = Option.map (function elmt -> snd (pe_stmt env elmt)) stmt_opt in 
    match e.value with
    | (LitExpr {value=BoolLit true; _}, _) -> env, stmt.value (* TODO should be stmt.place *)
    | (LitExpr {value=BoolLit false; _}, _) -> env, Option.value (Option.map (fun (x:stmt) -> x.value) stmt_opt) ~default:EmptyStmt (*TODO should be stmt.place *)
    | _ -> env, IfStmt (e, stmt, stmt_opt)
end
| LetStmt (let_mt, let_x, let_e) -> begin 
    let _, let_mt = pe_mtype env let_mt in
    let _, let_e = pe_expr env let_e in

    if is_terminal_expr let_e.value then (
        logger#debug "Is terminal %s" (Atom.to_string let_x);
        let new_env = bind_terminal_expr env let_x let_e in
        new_env, LetStmt (let_mt, let_x, let_e) (* TODO removing a let needs us to know if their is an assigned somewhere *)
    ) else(
        logger#debug "Is not terminal %s" (Atom.to_string let_x);
        env, LetStmt( let_mt, let_x, let_e)
    )
end
| MatchStmt (e, entries) -> (* TODO *) 
    env, MatchStmt (
        snd(pe_expr env e), 
        List.map ( function (e,stmt) -> snd(pe_expr env e), snd(pe_stmt env stmt)) entries
    )
| ExpressionStmt e -> 
    let new_env, new_e = pe_expr env e in
    new_env, (ExpressionStmt new_e)
| ReturnStmt e -> 
    let new_env, new_e = pe_expr env e in
    new_env, (ReturnStmt new_e)
| WithContextStmt (anonymous_mod, cname, e, stmts) ->
    let _, e = pe_expr env e in

    let new_env, stmts = List.fold_left_map (fun env stmt -> pe_stmt env stmt) env stmts in
    let stmts = clean_stmts stmts in

    new_env, (WithContextStmt (anonymous_mod, cname, e, stmts))
and pe_stmt env : stmt -> env * stmt = map2_place (peval_stmt env)


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
    | Some {AstUtils.value=(LitExpr {value=BoolLit true; _}, _); _} -> None
    | Some {AstUtils.value=(LitExpr {value=BoolLit false; _}, _); place} -> Error.error place "ensures expresion has been evaluated to false" 
    | Some {value=(LitExpr _,_); place} -> Error.error place "ensures expr has been evaluated to a non boolean literal"
    | pred_opt -> pred_opt
    in

    env, {contract with
        pre_binders;
        ensures = clean_predicate ensures;
        returns = clean_predicate returns 
    }
and pe_contract env: contract -> env * contract = map2_place (peval_contract env)

and peval_param env place (mt, x) = 
    env, (snd(pe_mtype env mt), x)
and pe_param env: param -> env * param = map2_place (peval_param env)

and peval_method env place m = 
    let contract_opt = Option.map (function c -> snd(pe_contract env c)) m.contract_opt in
    (* Cleansing: elminates empty contract *)
    let contract_opt = match contract_opt with
    | Some c when c.value.ensures = None && c.value.returns = None -> None
    | c_opt -> c_opt 
    in

    let m = {m with
            ret_type = snd(pe_mtype env m.ret_type);
            args = List.map (function param -> snd(pe_param env param)) m.args;
            body = List.map (snd <-> pe_stmt env) m.body;
            contract_opt = contract_opt
    } in

    env, m
and pe_method env: method0 -> env * method0 = map2_place (peval_method env)

and peval_port env place (port, mt_port) = 
    let expecting_st = snd(pe_mtype env port.expecting_st) in 
    
    (* TODO Should be move to type checking *)
    begin
        match expecting_st.value with
        | SType {value=STEnd; _} -> Error.error place "a port can not expect the end of a protocol, no message will be send"
        | SType _ -> ()
        | _ -> Error.error place "inport expecting value must be a session type"
    end;

    env, ({ port with
        expecting_st;
        callback = snd(pe_expr env port.callback)
    }, snd (pe_mtype env mt_port))
and pe_port env: port -> env * port = map2_place (peval_port env)

and peval_outport env place ((outport, mt_outport):_outport*main_type) = 
    env, (
        {name=outport.name}, 
        snd (pe_mtype env mt_outport)
    )
and pe_outport env: outport -> env * outport = map2_place (peval_outport env)

and peval_state env place = function 
| StateDcl s -> env, StateDcl {s with 
    type0 = snd(pe_mtype env s.type0);
    body = Option.map (function e -> snd(pe_expr env e)) s.body
} 
and pe_state env: state -> env * state = map2_place (peval_state env)

and peval_component_item env place : _component_item -> env * _component_item = function 
| Contract c -> raise (Error.PlacedDeadbranchError (place, "contract should be paired with method before partial_evaluation, therefore no contract should remains as a component_item"))
| Include cexpr -> env, Include (snd(pe_component_expr env cexpr))
| Method m -> env, Method (snd(pe_method env m))
|Inport p -> env,Inport (snd(pe_port env p))
| Outport p -> env, Outport (snd(pe_outport env p))
| State s -> 
    let s = snd(pe_state env s)in
    env, State (snd(pe_state env s))
| Term t -> env, Term (snd(pe_term env t))

and pe_component_item env: component_item -> env * component_item = map2_place (peval_component_item env)

and peval_component_dcl env place : _component_dcl -> env * _component_dcl = function  
| ComponentAssign {name; value} -> env, ComponentAssign {
    name;
    value = snd(pe_component_expr env value) 
} 
| ComponentStructure cdcl ->
    let new_env, citems = List.fold_left_map pe_component_item env cdcl.body in 
    new_env, ComponentStructure {cdcl with body = citems }

and pe_component_dcl env: component_dcl -> env * component_dcl = map2_place (peval_component_dcl env)

(********************** Manipulating component structure *********************)
and peval_component_expr env place (ce, mt_ce) = (* TODO peval for this*)
    let env, ce = match ce with
        | VarCExpr x -> env, VarCExpr x
        | AppCExpr (cexpr1, args) -> 
            let _, args = List.split (List.map (function arg -> pe_component_expr env arg) args) in
            env, AppCExpr (snd (pe_component_expr env cexpr1), args) 
        | UnboxCExpr e -> env, UnboxCExpr (snd(pe_expr env e)) 
        | AnyExpr e -> env, AnyExpr (snd(pe_expr env e)) 
    in env, (ce, snd (pe_mtype env mt_ce))
and pe_component_expr env: component_expr -> env * component_expr = map2_place (peval_component_expr env)

(************************************ Program *****************************)
and peval_term env place : _term -> env * _term = function
| Comments c -> env, Comments c
| Stmt stmt -> map_snd (fun x -> Stmt x) (pe_stmt env stmt) 
| Component comp -> map_snd (fun x -> Component x) (pe_component_dcl env comp)
| Function f -> env, Function {
    place = f.place;
    value = { f.value with
        ret_type = snd(pe_mtype env f.value.ret_type);
        args = List.map (function param -> snd(pe_param env param)) f.value.args;
        body = List.map (snd <-> pe_stmt env) f.value.body;
    }
}
| Typealias (x, mt_opt) -> begin
    match mt_opt with
    | None -> 
        let new_env = bind_named_types env x None in
        new_env, Typealias (x, None)
    | Some mt -> 
        let _, mt = pe_mtype env mt in 
        let new_env = bind_named_types env x (Some mt) in
        new_env, EmptyTerm
end
| Typedef {value= ProtocolDef (x, mt); place}-> begin 
    logger#debug "bind type %s" (Atom.to_string x);
    let _, mt = pe_mtype env mt in
    let new_env = bind_named_types env x (Some mt) in
    logger#debug "bind type %s" (Atom.hint x);

    new_env, Typedef ({ place; value = 
    ProtocolDef (x, mt) })
end
| Typedef {value= VPlaceDef x; place} -> begin
    logger#debug "bind type %s" (Atom.to_string x);
    let new_env = bind_named_types env x None in
    logger#debug "bind type %s" (Atom.hint x);

    new_env, Typedef ({ place; value = 
    VPlaceDef x})
end
| Typedef {value= ClassicalDef (x, args, ()) as tdef; place} | Typedef {value= EventDef (x, args, ()) as tdef; place} ->begin 
    logger#debug "bind type %s" (Atom.to_string x);
    let args = List.map (function mt -> snd(pe_mtype env mt)) args in
    let new_env = bind_named_types env x None in
    logger#debug "bind type %s" (Atom.hint x);
    print_env new_env;

    new_env, Typedef ({ place; value = 
    match tdef with 
        | ClassicalDef _ -> ClassicalDef (x, args, ())
        | EventDef _ -> EventDef (x, args, ())
    })
end
| Derive {name; cargs; targs; eargs} ->
    let _, cargs = List.split (List.map (pe_component_expr env) cargs) in
    let _, targs = List.split (List.map (pe_mtype env) targs) in
    let _, eargs = List.split (List.map (pe_expr env) eargs) in
    env, Derive {name; cargs; targs; eargs}
and pe_term env: term -> env * term = map2_place (peval_term env)

and pe_terms env terms : env * IR.term list =
    let env, program = List.fold_left_map pe_term env terms in
    env, List.filter (function |{AstUtils.value=EmptyTerm; _} -> false | _-> true) program


let peval_program (terms: IR.program) : IR.program = 
    let env, program = pe_terms (fresh_env ()) terms in
    check_program program;
    program

(**********************************************************)
let name = "PartialEval"
let displayed_pass_shortdescription = "IR has been partially evaluated"
let displayed_ast_name = "pevaled IR"
let show_ast = true
let global_at_most_once_apply = false

let precondition program = program
let postcondition program = program
let apply_program = peval_program

