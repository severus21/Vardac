open Utils
open Error
open Easy_logging
open Fieldslib

module S = IR
module T = IRD
let logger = Logging.make_logger "_1_ compspec.frontend" Debug [];;

(* atom -> debruijn *)
module Env = Atom.VMap

type env = DeBruijn.debruijn Env.t
let fresh_env () = Env.empty

let rec a2d_full_place a2d_value env ({ AstUtils.place ; AstUtils.value}: 'a AstUtils.placed) = 
    let env, value = a2d_value env place value in
    env, {AstUtils.place; AstUtils.value}

let rec a2d_place a2d_value env ({ AstUtils.place ; AstUtils.value}: 'a AstUtils.placed) = 
    let value = a2d_value env place value in
    {AstUtils.place; AstUtils.value}


let rec a2d_var_expr env x =
    if Atom.is_builtin x then DeBruijn.DeBuiltin x
    else Env.find x env
and a2d_var_type env x = x
and a2d_var_component env x = x

(************************************ Types **********************************)

(* notation debruin uniquement pour les expressions le reste reste en atom*)
and _a2d_full_composed_type env ct = function 
| S.TActivationInfo mt -> env, T.TActivationInfo (a2d_main_type env mt)
| S.TArrow (mt1, mt2) -> env, T.TArrow (
    a2d_main_type env mt1,
    a2d_main_type env mt2
)
| S.TVar x -> env, T.TVar (a2d_var_type env x)
| S.TFlatType ft -> env, T.TFlatType ft
| S.TArray mt -> env, T.TArray (a2d_main_type env mt)
| S.TDict (mt1, mt2) -> env, T.TDict (
    a2d_main_type env mt1,
    a2d_main_type env mt2
)
| S.TList mt -> env, T.TList (a2d_main_type env mt)
| S.TOption mt -> env, T.TOption (a2d_main_type env mt)
| S.TResult (mt1, mt2) -> env, T.TResult (
    a2d_main_type env mt1,
    a2d_main_type env mt2
)
| S.TSet mt -> env, T.TSet (a2d_main_type env mt)
| S.TTuple mts -> env, T.TTuple (List.map (a2d_main_type env) mts)
| S.TVPlace mt -> env, T.TVPlace (a2d_main_type env mt)
| S.TUnion (mt1, mt2) -> env, T.TUnion (
    a2d_main_type env mt1,
    a2d_main_type env mt2
)
| S.TBridge b -> env, T.TBridge {
    in_type = a2d_main_type env b.in_type;
    out_type = a2d_main_type env b.out_type;
    protocol = a2d_main_type env b.protocol;
}
| S.TRaw bt -> env, T.TRaw bt
and a2d_full_composed_type env = a2d_full_place _a2d_full_composed_type env
and a2d_composed_type env ct = snd (a2d_full_composed_type env ct) 
and _a2d_session_type env place = function
| S.STEnd -> env, T.STEnd
| S.STVar x -> env, T.STVar x
| S.STSend (mt, st) -> 
    let mt = a2d_main_type env mt in
    let st = a2d_session_type env st in
    env, T.STSend (mt, st)
| S.STRecv (mt, st) -> 
    let mt = a2d_main_type env mt in
    let st = a2d_session_type env st in
    env, T.STRecv (mt, st)
| S.STRec (x, st) -> 
    let st = a2d_session_type env st in
    env, T.STRec (x, st)
| S.STInline x -> env, T.STInline x 
and a2d_full_session_type env = a2d_full_place _a2d_session_type env 
and a2d_session_type env st : T.session_type = snd (a2d_full_session_type env st) 

and _a2d_component_type env place = function
| S.CompTUid x -> env, T.CompTUid x
and a2d_full_component_type env = a2d_full_place _a2d_component_type env 
and a2d_component_type env ct = snd (a2d_full_component_type env ct)

and _a2d_main_type env place = function
| S.CType ct -> env, T.CType (a2d_composed_type env ct)
| S.SType st -> env, T.SType (a2d_session_type env st)
| S.CompType ct -> env, T.CompType (a2d_component_type env ct)
| S.ConstrainedType (mt, guard) -> env, T.ConstrainedType (
    a2d_main_type env mt, 
    a2d_applied_constraint env guard)
and a2d_full_main_type env = a2d_full_place _a2d_main_type env 
and a2d_main_type env mt = snd (a2d_full_main_type env mt)

(******************************** Constraints ********************************)

and _a2d_constraint_header env place = function
| S.UseGlobal (mt, x) -> 
    env, T.UseGlobal (
        a2d_main_type env mt,
        a2d_var_expr env x
    )
| S.UseMetadata (mt, x) -> 
    env, T.UseMetadata (
        a2d_main_type env mt,
        a2d_var_expr env x
    )
| S.SetTimer x -> env, T.SetTimer (a2d_var_expr env x)
| S.SetFireTimer (x, i) -> env, T.SetFireTimer (a2d_var_expr env x, i)
and a2d_full_constraint_header env = a2d_full_place _a2d_constraint_header env
and a2d_constraint_header env h = snd (a2d_full_constraint_header env h)

and a2d_full_applied_constraint env (headers, guard_opt) = 
env, (List.map (a2d_constraint_header env) headers, Option.map (a2d_expr env) guard_opt)
and a2d_applied_constraint env (ac:S.applied_constraint)  = snd (a2d_full_applied_constraint env ac)

(************************************ (V) - Place ****************************)

and a2d_full_vplace env (vp:S.vplace) = 
env, {
    T.name = a2d_var_component env vp.name;
    nbr_instances = a2d_expr env vp.nbr_instances;
    features = vp.features;
    children = List.map (a2d_vplace env) vp.children;
}
and a2d_vplace env vp = snd (a2d_full_vplace env vp)

(************************************* Literals ******************************)

and _a2d_literal env place = function
| S.VoidLit -> T.VoidLit
| S.BoolLit b -> T.BoolLit b
| S.FloatLit f -> T.FloatLit f
| S.IntLit i -> T.IntLit i
| S.LabelLit l -> failwith "a2d_literal label"
| S.StringLit s -> T.StringLit s
| S.VPlace vp -> T.VPlace (a2d_vplace env vp)
| S.Bridge b -> T.Bridge {
    id = a2d_var_component env b.id;
    protocol_name = a2d_var_component env b.protocol_name;
}
and a2d_literal env lit = a2d_literal env lit

and _a2d_expr env place = function
| S.VarExpr x -> T.VarExpr (a2d_var_expr env x) 
| S.AccessExpr (e1, e2) -> T.AccessExpr (
    a2d_expr env e1,
    a2d_expr env e2
)
| S.BinopExpr (e1, op, e2) -> T.BinopExpr (
    a2d_expr env e1,
    op,
    a2d_expr env e2
)
| S.LambdaExpr (x, mt, stmt) -> failwith "TODO a2d_expr binder"
| S.LitExpr l -> T.LitExpr (a2d_literal env l)
| S.UnopExpr (op, e) -> T.UnopExpr (op, a2d_expr env e)
| S.CallExpr (e, es) -> T.CallExpr(
    a2d_expr env e,
    List.map (a2d_expr env) es
)
| S.NewExpr (e, es) -> T.NewExpr(
    a2d_expr env e,
    List.map (a2d_expr env) es
)
| S.This -> T.This 
| S.Spawn spawn -> T.Spawn {
    c = a2d_component_expr env spawn.c;
    args = List.map (a2d_expr env) spawn.args;
    at = Option.map (a2d_expr env) spawn.at;
} 
| S.BoxCExpr ce -> T.BoxCExpr (a2d_component_expr env ce)
| S.OptionExpr e_opt -> T.OptionExpr (Option.map(a2d_expr env) e_opt)
| S.ResultExpr (e1_opt, e2_opt) -> T.ResultExpr (Option.map(a2d_expr env) e1_opt, Option.map(a2d_expr env) e2_opt)
| S.BlockExpr (b, es) -> T.BlockExpr (
    b,
    List.map (a2d_expr env) es
)
| S.Block2Expr (b, es) -> T.Block2Expr (
    b,
    List.map (function (e1, e2) -> a2d_expr env e1, a2d_expr env e2) es
)
and a2d_expr env = a2d_place _a2d_expr env 

and _a2d_stmt env place : S._stmt -> env * T._stmt = function
| S.EmptyStmt -> env, T.EmptyStmt
| S.AssignExpr (x, e) -> env, T.AssignExpr (
    a2d_var_expr env x,
    a2d_expr env e
)
| S.AssignThisExpr (x, e) -> env, T.AssignThisExpr (
    a2d_var_component env x,
    a2d_expr env e
)
| S.LetExpr _ -> failwith "binders"
| S.CommentsStmt c -> env, T.CommentsStmt c

| S.BreakStmt -> env, T.BreakStmt
| S.ContinueStmt -> env, T.ContinueStmt
| S.ExitStmt i -> env, T.ExitStmt i
| S.ForStmt (mt, x, e, stmt) -> env, T.ForStmt (
    a2d_main_type env mt,
    a2d_var_expr env x,
    a2d_expr env e,
    a2d_stmt env stmt
)
| S.IfStmt (e, stmt1, stmt2_opt) -> env, T.IfStmt (
    a2d_expr env e,
    a2d_stmt env stmt1,
    Option.map (a2d_stmt env) stmt2_opt
)
| S.MatchStmt (e, branches) -> env, T.MatchStmt (
    a2d_expr env e,
    List.map (function (e, stmt) -> 
        a2d_expr env e, a2d_stmt env stmt
    ) branches
)
| S.ReturnStmt e -> env, T.ReturnStmt (a2d_expr env e)
| S.ExpressionStmt e -> env, T.ExpressionStmt (a2d_expr env e)
| S.BlockStmt stmts -> 
    let env, stmts = List.fold_left_map a2d_full_stmt env stmts in
    env, T.BlockStmt stmts
and a2d_full_stmt env : S.stmt -> env * T.stmt =  a2d_full_place _a2d_stmt env 
and a2d_stmt env p = snd (a2d_full_stmt env p)

and _a2d_param env place (mt, x) = env, ( a2d_main_type env mt, a2d_var_expr env x)
and a2d_full_param env : S.param -> env * T.param = a2d_full_place _a2d_param env 
and a2d_param env (p:S.param) : T.param = snd (a2d_full_param env p)

and _a2d_port env place (p:S._port) = env, {
    T.name = a2d_var_component env p.name;
    input = a2d_expr env p.input;
    expecting_st = a2d_main_type env p.expecting_st;
    callback = a2d_expr env p.callback;
} 
and a2d_full_port env = a2d_full_place _a2d_port env
and a2d_port env p = snd (a2d_full_port env p)


and _a2d_contract env place (p:S._contract) = failwith "prebinders a2d_contract" (*env, {
    T.method_name = a2d_var_component env p.method_name;
    pre_binders = List.map (function (mt, x, e) -> 
        a2d_main_type env mt
        a2d_var_expr env x 
        a2d_expr env e
    p.pre_binders;
    ensures = a2d_main_type env p.expecting_st;
    returns = a2d_expr env p.callback;
} *)
and a2d_full_contract env = a2d_full_place _a2d_contract env
and a2d_contract env p = snd (a2d_full_contract env p)

and _a2d_method env place = function
| S.CustomMethod m -> T.CustomMethod {
    T.name = a2d_var_component env m.name;
    ghost = m.ghost;
    ret_type = a2d_main_type env m.ret_type;
    args =  List.map (a2d_param env) m.args;
    body = snd (List.fold_left_map a2d_full_stmt env m.body);
    contract_opt = Option.map (a2d_contract env) m.contract_opt;
} 
| S.OnStartup m -> T.OnStartup (a2d_method env m)
| S.OnDestroy m -> T.OnDestroy (a2d_method env m)
and a2d_method env = a2d_place _a2d_method env

and _a2d_state env place = function 
| S.StateDcl s -> env, T.StateDcl {
    name = a2d_var_component env s.name;
    ghost = s.ghost;
    type0 = a2d_main_type env s.type0;
    body = Option.map (a2d_expr env) s.body;
} 
and a2d_full_state env = a2d_full_place _a2d_state env
and a2d_state env p = snd (a2d_full_state env p)

and _a2d_component_item env place = function 
| S.Contract s -> env, T.Contract (a2d_contract env s)
| S.Include ce -> env, T.Include (a2d_component_expr env ce)
| S.Method m -> env, T.Method (a2d_method env m)
| S.Port p -> env, T.Port (a2d_port env p)
| S.State s -> env, T.State (a2d_state env s)
| S.Term t -> env, T.Term (a2d_term env t)
and a2d_full_component_item env = a2d_full_place _a2d_component_item env
and a2d_component_item env p = snd (a2d_full_component_item env p)


and _a2d_component_dcl env place = function 
| S.ComponentStructure cdcl -> env, T.ComponentStructure {
    target_name = cdcl.target_name;
    name = a2d_var_component env cdcl.name;
    args = List.map (a2d_param env) cdcl.args;
    body = List.map (a2d_component_item env) cdcl.body
} 
| S.ComponentAssign cdcl -> env, T.ComponentAssign {
    name = a2d_var_component env cdcl.name;
    args = List.map (a2d_param env) cdcl.args;
    value = a2d_component_expr env cdcl.value;
} 
and a2d_full_component_dcl env = a2d_full_place _a2d_component_dcl env
and a2d_component_dcl env p = snd (a2d_full_component_dcl env p)



(********************** Manipulating component structure *********************)
and _a2d_component_expr env place = function
| S.VarCExpr x -> env, T.VarCExpr (a2d_var_component env x)
| S.AppCExpr (ce1, ce2) -> env, T.AppCExpr (
    a2d_component_expr env ce1,
    a2d_component_expr env ce2
)
| S.UnboxCExpr e -> env, T.UnboxCExpr (a2d_expr env e)
| S.AnyExpr e -> env, T.AnyExpr (a2d_expr env e)
and a2d_full_component_expr env = a2d_full_place _a2d_component_expr env
and a2d_component_expr env p = snd (a2d_full_component_expr env p)

(************************************ Program *****************************)

and _a2d_function_dcl env place (fdcl:S._function_dcl) : T._function_dcl = {
    T.name = a2d_var_expr env fdcl.name;
    ret_type = a2d_main_type env fdcl.ret_type;
    args = List.map (a2d_param env) fdcl.args;
    body = snd (List.fold_left_map a2d_full_stmt env fdcl.body);
} 
and a2d_function_dcl env = a2d_place _a2d_function_dcl env

and _a2d_typedef env place = function 
| S.ClassicalDef (x, mts, body) -> env, T.ClassicalDef (
    a2d_var_type env x,
    List.map (a2d_main_type env) mts,
    body
) 
| S.EventDef (x, mts, body) -> env, T.EventDef (
    a2d_var_type env x,
    List.map (a2d_main_type env) mts,
    body
)  
| S.ProtocolDef (x, mt) -> env, T.ProtocolDef (
    a2d_var_type env x,
    a2d_main_type env mt
)  
and a2d_full_typedef env = a2d_full_place _a2d_typedef env
and a2d_typedef env p = snd (a2d_full_typedef env p)

and _a2d_term env place = function 
| S.EmptyTerm -> env, T.EmptyTerm
| S.Comments c -> env, T.Comments c
| S.Stmt stmt -> 
    let env, stmt = a2d_full_stmt env stmt in
    env, T.Stmt stmt
| S.Component c -> env, T.Component (a2d_component_dcl env c)
| S.Function f -> env, T.Function (a2d_function_dcl env f)
| S.Typealias (x, body) -> env, T.Typealias (
    a2d_var_type env x,
    Option.map (a2d_main_type env) body
)
| S.Typedef tdef -> env, T.Typedef (a2d_typedef env tdef)
and a2d_full_term env = a2d_full_place _a2d_term env
and a2d_term env p = snd (a2d_full_term env p)

and a2d_program env program = 
    snd (List.fold_left_map a2d_full_term env program)