open Utils
open Error
open Easy_logging
open Fieldslib

module S = IRD
module T = IR
let logger = Logging.make_logger "_1_ compspec.frontend" Debug [];;

let rec a2d_place a2d_value ({ AstUtils.place ; AstUtils.value}: 'a AstUtils.placed) = 
    let value = a2d_value place value in
    {AstUtils.place; AstUtils.value}


let rec a2d_var_expr = function
| DeBruijn.DeBruijn (_, a) -> a
| DeBruijn.DeBuiltin a -> a
and a2d_var_type x = x
and a2d_var_component x = x

(************************************ Types **********************************)

(* notation debruin uniquement pour les expressions le reste reste en atom*)
and _a2d_composed_type ct = function 
| S.TActivationInfo mt -> T.TActivationInfo (a2d_main_type mt)
| S.TArrow (mt1, mt2) -> T.TArrow (
    a2d_main_type mt1,
    a2d_main_type mt2
)
| S.TVar x -> T.TVar (a2d_var_type x)
| S.TFlatType ft -> T.TFlatType ft
| S.TArray mt -> T.TArray (a2d_main_type mt)
| S.TDict (mt1, mt2) -> T.TDict (
    a2d_main_type mt1,
    a2d_main_type mt2
)
| S.TList mt -> T.TList (a2d_main_type mt)
| S.TOption mt -> T.TOption (a2d_main_type mt)
| S.TResult (mt1, mt2) -> T.TResult (
    a2d_main_type mt1,
    a2d_main_type mt2
)
| S.TSet mt -> T.TSet (a2d_main_type mt)
| S.TTuple mts -> T.TTuple (List.map a2d_main_type mts)
| S.TVPlace mt -> T.TVPlace (a2d_main_type mt)
| S.TUnion (mt1, mt2) -> T.TUnion (
    a2d_main_type mt1,
    a2d_main_type mt2
)
| S.TBridge b -> T.TBridge {
    in_type = a2d_main_type b.in_type;
    out_type = a2d_main_type b.out_type;
    protocol = a2d_main_type b.protocol;
}
| S.TRaw bt -> T.TRaw bt
and a2d_composed_type ct = a2d_place _a2d_composed_type ct

and _a2d_session_type place = function
| S.STEnd -> T.STEnd
| S.STVar x -> T.STVar x
| S.STSend (mt, st) -> 
    let mt = a2d_main_type mt in
    let st = a2d_session_type st in
    T.STSend (mt, st)
| S.STRecv (mt, st) -> 
    let mt = a2d_main_type mt in
    let st = a2d_session_type st in
    T.STRecv (mt, st)
| S.STRec (x, st) -> 
    let st = a2d_session_type st in
    T.STRec (x, st)
| S.STInline x -> T.STInline x 
and a2d_session_type st = a2d_place _a2d_session_type st

and _a2d_component_type place = function
| S.CompTUid x -> T.CompTUid x
and a2d_component_type ct = a2d_place _a2d_component_type ct 

and _a2d_main_type place = function
| S.CType ct -> T.CType (a2d_composed_type ct)
| S.SType st -> T.SType (a2d_session_type st)
| S.CompType ct -> T.CompType (a2d_component_type ct)
| S.ConstrainedType (mt, guard) -> T.ConstrainedType (
    a2d_main_type mt, 
    a2d_applied_constraint guard)
and a2d_main_type mt = a2d_place _a2d_main_type mt 

(******************************** Constraints ********************************)

and _a2d_constraint_header place = function
| S.UseGlobal (mt, x) -> 
    T.UseGlobal (
        a2d_main_type mt,
        a2d_var_expr x
    )
| S.UseMetadata (mt, x) -> 
    T.UseMetadata (
        a2d_main_type mt,
        a2d_var_expr x
    )
| S.SetTimer x -> T.SetTimer (a2d_var_expr x)
| S.SetFireTimer (x, i) -> T.SetFireTimer (a2d_var_expr x, i)
and a2d_constraint_header h= a2d_place _a2d_constraint_header h

and a2d_applied_constraint (headers, guard_opt) = 
    List.map a2d_constraint_header headers, Option.map a2d_expr guard_opt

(************************************ (V) - Place ****************************)

and a2d_vplace (vp:S.vplace) = 
 {
    T.name = a2d_var_component vp.name;
    nbr_instances = a2d_expr vp.nbr_instances;
    features = vp.features;
    children = List.map a2d_vplace vp.children;
}

(************************************* Literals ******************************)

and _a2d_literal place = function
| S.VoidLit -> T.VoidLit
| S.BoolLit b -> T.BoolLit b
| S.FloatLit f -> T.FloatLit f
| S.IntLit i -> T.IntLit i
| S.LabelLit l -> failwith "a2d_literal label"
| S.StringLit s -> T.StringLit s
| S.VPlace vp -> T.VPlace (a2d_vplace vp)
| S.Bridge b -> T.Bridge {
    id = a2d_var_component b.id;
    protocol_name = a2d_var_component b.protocol_name;
}
and a2d_literal lit = a2d_literal lit

and _a2d_expr place = function
| S.VarExpr x -> T.VarExpr (a2d_var_expr x) 
| S.AccessExpr (e1, e2) -> T.AccessExpr (
    a2d_expr e1,
    a2d_expr e2
)
| S.BinopExpr (e1, op, e2) -> T.BinopExpr (
    a2d_expr e1,
    op,
    a2d_expr e2
)
| S.LambdaExpr (x, mt, stmt) -> failwith "TODO a2d_expr binder"
| S.LitExpr l -> T.LitExpr (a2d_literal l)
| S.UnopExpr (op, e) -> T.UnopExpr (op, a2d_expr e)
| S.CallExpr (e, es) -> T.CallExpr(
    a2d_expr e,
    List.map a2d_expr es
)
| S.NewExpr (e, es) -> T.NewExpr(
    a2d_expr e,
    List.map a2d_expr es
)
| S.This -> T.This 
| S.Spawn spawn -> T.Spawn {
    c = a2d_component_expr spawn.c;
    args = List.map a2d_expr spawn.args;
    at = Option.map a2d_expr spawn.at;
} 
| S.BoxCExpr ce -> T.BoxCExpr (a2d_component_expr ce)
| S.OptionExpr e_opt -> T.OptionExpr (Option.map a2d_expr e_opt)
| S.ResultExpr (e1_opt, e2_opt) -> T.ResultExpr (Option.map a2d_expr e1_opt, Option.map a2d_expr e2_opt)
| S.BlockExpr (b, es) -> T.BlockExpr (
    b,
    List.map a2d_expr es
)
| S.Block2Expr (b, es) -> T.Block2Expr (
    b,
    List.map (function (e1, e2) -> a2d_expr e1, a2d_expr e2) es
)
and a2d_expr e = a2d_place _a2d_expr e

and _a2d_stmt place : S._stmt -> T._stmt = function
| S.EmptyStmt -> T.EmptyStmt
| S.AssignExpr (x, e) -> T.AssignExpr (
    a2d_var_expr x,
    a2d_expr e
)
| S.AssignThisExpr (x, e) -> T.AssignThisExpr (
    a2d_var_component x,
    a2d_expr e
)
| S.LetExpr _ -> failwith "binders"
| S.CommentsStmt c -> T.CommentsStmt c

| S.BreakStmt -> T.BreakStmt
| S.ContinueStmt -> T.ContinueStmt
| S.ExitStmt i -> T.ExitStmt i
| S.ForStmt (mt, x, e, stmt) -> T.ForStmt (
    a2d_main_type mt,
    a2d_var_expr x,
    a2d_expr e,
    a2d_stmt stmt
)
| S.IfStmt (e, stmt1, stmt2_opt) -> T.IfStmt (
    a2d_expr e,
    a2d_stmt stmt1,
    Option.map a2d_stmt stmt2_opt
)
| S.MatchStmt (e, branches) -> T.MatchStmt (
    a2d_expr e,
    List.map (function (e, stmt) -> 
        a2d_expr e, a2d_stmt stmt
    ) branches
)
| S.ReturnStmt e -> T.ReturnStmt (a2d_expr e)
| S.ExpressionStmt e -> T.ExpressionStmt (a2d_expr e)
| S.BlockStmt stmts -> 
    let stmts = List.map a2d_stmt stmts in
    T.BlockStmt stmts
and a2d_stmt stmt =  a2d_place _a2d_stmt stmt

and _a2d_param place (mt, x) = ( a2d_main_type mt, a2d_var_expr x)
and a2d_param arg = a2d_place _a2d_param arg

and _a2d_port place (p:S._port) = {
    T.name = a2d_var_component p.name;
    input = a2d_expr p.input;
    expecting_st = a2d_main_type p.expecting_st;
    callback = a2d_expr p.callback;
} 
and a2d_port p = a2d_place _a2d_port p


and _a2d_contract place (p:S._contract) = {
    T.method_name = a2d_var_component p.method_name;
    pre_binders = List.map (function (mt, x, e) -> 
        a2d_main_type mt,
        a2d_var_expr x,
        a2d_expr e
    ) p.pre_binders;
    ensures = Option.map a2d_expr p.ensures;
    returns = Option.map a2d_expr p.returns;
} 
and a2d_contract c = a2d_place _a2d_contract c

and _a2d_method place = function
| S.CustomMethod m -> T.CustomMethod {
    T.name = a2d_var_component m.name;
    ghost = m.ghost;
    ret_type = a2d_main_type m.ret_type;
    args =  List.map a2d_param m.args;
    body = List.map a2d_stmt m.body;
    contract_opt = Option.map a2d_contract m.contract_opt;
} 
| S.OnStartup m -> T.OnStartup (a2d_method m)
| S.OnDestroy m -> T.OnDestroy (a2d_method m)
and a2d_method m = a2d_place _a2d_method m

and _a2d_state place = function 
| S.StateDcl s -> T.StateDcl {
    name = a2d_var_component s.name;
    ghost = s.ghost;
    type0 = a2d_main_type s.type0;
    body = Option.map a2d_expr s.body;
} 
and a2d_state s = a2d_place _a2d_state s

and _a2d_component_item place = function 
| S.Contract s -> T.Contract (a2d_contract s)
| S.Include ce -> T.Include (a2d_component_expr ce)
| S.Method m -> T.Method (a2d_method m)
| S.Port p -> T.Port (a2d_port p)
| S.State s -> T.State (a2d_state s)
| S.Term t -> T.Term (a2d_term t)
and a2d_component_item citem= a2d_place _a2d_component_item citem


and _a2d_component_dcl place = function 
| S.ComponentStructure cdcl -> T.ComponentStructure {
    target_name = cdcl.target_name;
    name = a2d_var_component cdcl.name;
    args = List.map a2d_param cdcl.args;
    body = List.map a2d_component_item cdcl.body
} 
| S.ComponentAssign cdcl -> T.ComponentAssign {
    name = a2d_var_component cdcl.name;
    args = List.map a2d_param cdcl.args;
    value = a2d_component_expr cdcl.value;
} 
and a2d_component_dcl cdcl = a2d_place _a2d_component_dcl cdcl



(********************** Manipulating component structure *********************)
and _a2d_component_expr place = function
| S.VarCExpr x -> T.VarCExpr (a2d_var_component x)
| S.AppCExpr (ce1, ce2) -> T.AppCExpr (
    a2d_component_expr ce1,
    a2d_component_expr ce2
)
| S.UnboxCExpr e -> T.UnboxCExpr (a2d_expr e)
| S.AnyExpr e -> T.AnyExpr (a2d_expr e)
and a2d_component_expr ce = a2d_place _a2d_component_expr ce

(************************************ Program *****************************)

and _a2d_function_dcl place (fdcl:S._function_dcl) : T._function_dcl = {
    T.name = a2d_var_expr fdcl.name;
    ret_type = a2d_main_type fdcl.ret_type;
    args = List.map a2d_param fdcl.args;
    body = List.map a2d_stmt fdcl.body;
} 
and a2d_function_dcl fdcl = a2d_place _a2d_function_dcl fdcl

and _a2d_typedef place = function 
| S.ClassicalDef (x, mts, body) -> T.ClassicalDef (
    a2d_var_type x,
    List.map a2d_main_type mts,
    body
) 
| S.EventDef (x, mts, body) -> T.EventDef (
    a2d_var_type x,
    List.map a2d_main_type mts,
    body
)  
| S.ProtocolDef (x, mt) -> T.ProtocolDef (
    a2d_var_type x,
    a2d_main_type mt
)  
and a2d_typedef tdef = a2d_place _a2d_typedef tdef

and _a2d_term place = function 
| S.EmptyTerm -> T.EmptyTerm
| S.Comments c -> T.Comments c
| S.Stmt stmt -> 
    let stmt = a2d_stmt stmt in
    T.Stmt stmt
| S.Component c -> T.Component (a2d_component_dcl c)
| S.Function f -> T.Function (a2d_function_dcl f)
| S.Typealias (x, body) -> T.Typealias (
    a2d_var_type x,
    Option.map a2d_main_type body
)
| S.Typedef tdef -> T.Typedef (a2d_typedef tdef)
and a2d_term t = a2d_place _a2d_term t

and a2d_program program = 
    List.map a2d_term program