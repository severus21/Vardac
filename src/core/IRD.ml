(* De Bruijn *)

module IRCParams : (AstUtils.IRParams with type Variable.t = DeBruijn.t) = struct
    module Variable = DeBruijn.DebruijnVariable
end 

module IRC  : (IR_common.TIRC with module Variable = DeBruijn.DebruijnVariable )= IR_common.Make(DeBruijn.DebruijnVariable)

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
    module Variable = DeBruijn
    type target_name = ir_target_name 
    and _state_dcl_body = ir_state_dcl_body
    and _custom_method0_body = ir_custom_method0_body
    and _custom_function_body = ir_custom_function_body
    and _typealias_body = ir_typealias_body
    and _typedef_body = ir_typedef_body
    [@@deriving show { with_path = false }]
end

include IR_common
module IRD = IR_template.Make(IRC)(Params) 
include IRD


open Utils
open Error
open Easy_logging
open Fieldslib

let logger = Logging.make_logger "_1_ compspec.frontend" Debug [];;

(* d-place shift of a term t above cutoff c - Pierce Types and Programming Languages p.79*)
type shift = {c:int; d: int} 
type subst = {j:int; s: expr} (*[d -> e] substitution of a variable j by expression e*) 
type shift_subst = shift option * subst option

let rec shsu_place shsu_value ssargs ({ AstUtils.place ; AstUtils.value}: 'a AstUtils.placed) = 
    let value = shsu_value ssargs place value in
    {AstUtils.place; AstUtils.value}

let rec shsu_var_expr ({c;d}, subst_opt) = function
| DeBruijn.DeBruijn (i_a, a) -> begin 
    DeBruijn.DeBruijn (
        ( 
            match subst_opt with
            | Some _ -> raise (Error.DeadbranchError "substitution should have been applied before calling shsu_var_expr, since substituing return an expr and not and expr_variable")
            | None ->
                if i_a < c then i_a else i_a + d
        ), a
    )
end
| DeBruijn.DeBuiltin _ as x -> x
and shsu_var_type ssargs x = x
and shsu_var_component ssargs x = x

(************************************ Types **********************************)

(* notation debruin uniquement pour les expressions le reste reste en atom*)
and shsu_composed_type ct = ct

and _shsu_session_type ssargs place = function
| STEnd -> STEnd
| STVar x -> STVar x
| STSend (mt, st) -> 
    let mt = shsu_main_type ssargs mt in
    let st = shsu_session_type ssargs st in
    STSend (mt, st)
| STRecv (mt, st) -> 
    let mt = shsu_main_type ssargs mt in
    let st = shsu_session_type ssargs st in
    STRecv (mt, st)
| STRec (x, st) -> 
    let st = shsu_session_type ssargs st in
    STRec (x, st)
| STInline x -> STInline x 
and shsu_session_type ssargs st = shsu_place _shsu_session_type ssargs st

and _shsu_component_type ssargs place = function
| CompTUid x -> CompTUid x
and shsu_component_type ssargs ct = shsu_place _shsu_component_type ssargs ct 

and _shsu_main_type ssargs place = function
| CType ct -> CType (shsu_composed_type ssargs ct)
| SType st -> SType (shsu_session_type ssargs st)
| CompType ct -> CompType (shsu_component_type ssargs ct)
| ConstrainedType (mt, guard) -> ConstrainedType (
    shsu_main_type ssargs mt, 
    shsu_applied_constraint ssargs guard)
and shsu_main_type ssargs mt = shsu_place _shsu_main_type ssargs mt 

(******************************** Constraints ********************************)

and _shsu_constraint_header ssargs place = failwith "shift_subst constraint header semantics not defined" 
(*| UseGlobal (mt, x) -> 
    UseGlobal (
        shsu_main_type ssargs mt,
        shsu_var_expr ssargs x
    )
| UseMetadata (mt, x) -> 
    UseMetadata (
        shsu_main_type ssargs mt,
        shsu_var_expr ssargs x
    )
| SetTimer x -> SetTimer (shsu_var_expr ssargs x)
| SetFireTimer (x, i) -> SetFireTimer (shsu_var_expr ssargs x, i)
*)
and shsu_constraint_header ssargs h = shsu_place _shsu_constraint_header ssargs h

and shsu_applied_constraint ssargs (headers, guard_opt) = 
    List.map (shsu_constraint_header ssargs) headers, Option.map (shsu_expr ssargs) guard_opt

(************************************ (V) - Place ****************************)

and shsu_vplace ssargs (vp:vplace) = vp

(************************************* Literals ******************************)

(* No variable inside *)
and shsu_literal ssargs lit = lit

and _shsu_expr (({c;d}, subst_opt) as ssargs) place = function
| VarExpr DeBruijn.DeBuiltin _ as e -> e
| VarExpr (DeBruijn.DeBruijn (i_a, _) as x) -> begin
    match subst_opt with
    | None -> VarExpr (shsu_var_expr ssargs x) 
    | Some {j; s} -> 
        if i_a = j then s.value else VarExpr x (* TODO we loose the place of s*)
end
| AccessExpr (e1, e2) -> AccessExpr (
    shsu_expr ssargs e1,
    shsu_expr ssargs e2
)
| BinopExpr (e1, op, e2) -> BinopExpr (
    shsu_expr ssargs e1,
    op,
    shsu_expr ssargs e2
)
| LambdaExpr (x, mt, stmt) -> LambdaExpr
    (
        x, (* not used - except for DeBruijnAtom*)
        shsu_main_type ssargs mt, 
        shsu_stmt ({c = c+1; d}, Option.map (function {j; s} -> {j = j+1; s}) subst_opt) stmt
    )
| LitExpr l -> LitExpr (shsu_literal ssargs l)
| UnopExpr (op, e) -> UnopExpr (op, shsu_expr ssargs e)
| CallExpr (e, es) -> CallExpr(
    shsu_expr ssargs e,
    List.map shsu_expr ssargs es
)
| NewExpr (e, es) -> NewExpr(
    shsu_expr ssargs e,
    List.map shsu_expr ssargs es
)
| This -> This 
| Spawn spawn -> Spawn {
    c = shsu_component_expr ssargs spawn.c;
    args = List.map (shsu_expr ssargs) spawn.args;
    at = Option.map (shsu_expr ssargs) spawn.at;
} 
| BoxCExpr ce -> BoxCExpr (shsu_component_expr ssargs ce)
| OptionExpr e_opt -> OptionExpr (Option.map (shsu_expr ssargs) e_opt)
| ResultExpr (e1_opt, e2_opt) -> ResultExpr (Option.map (shsu_expr ssargs) e1_opt, Option.map (shsu_expr ssargs) e2_opt)
| BlockExpr (b, es) -> BlockExpr (
    b,
    List.map (shsu_expr ssargs) es
)
| Block2Expr (b, es) -> Block2Expr (
    b,
    List.map (function (e1, e2) -> shsu_expr ssargs e1, shsu_expr ssargs e2) es
)
and shsu_expr ssargs e = shsu_place ssargs _shsu_expr ssargs e

and _shsu_stmt (({c;d}, subst_opt) as ssargs) place = function
| EmptyStmt -> ssargs, EmptyStmt
| AssignExpr (x, e) -> ssargs, AssignExpr (
    shsu_var_expr ssargs x,
    shsu_expr ssargs e
)
| AssignThisExpr (x, e) -> ssargs, AssignThisExpr (
    shsu_var_component ssargs x,
    shsu_expr ssargs e
)
| LetExpr (x, mt, e) -> 
    ({c = c+1; d}, Option.map (function {j; s} -> {j = j+1; s}) subst_opt) ,LetExpr( (* Just like the Lambda rule *)
    x, (* not used, only in DeBruijnAtom *)
    shsu_main_type ssargs mt,
    shsu_expr ssargs e
) 
| CommentsStmt c -> ssargs, CommentsStmt c

| BreakStmt -> ssargs, BreakStmt
| ContinueStmt -> ssargs ContinueStmt
| ExitStmt i -> ssargs, ExitStmt i
| ForStmt (mt, x, e, stmt) -> failwith "binders for"(*ForStmt (
    shsu_main_type ssargs mt,
    shsu_var_expr ssargs x,
    shsu_expr ssargs e,
    shsu_stmt ssargs stmt
)*)
| IfStmt (e, stmt1, stmt2_opt) -> ssargs, IfStmt (
    shsu_expr ssargs e,
    shsu_stmt ssargs stmt1,
    Option.map shsu_stmt ssargs stmt2_opt
)
| MatchStmt (e, branches) -> ssargs, MatchStmt (
    shsu_expr ssargs e,
    List.map (function (e, stmt) -> 
        shsu_expr ssargs e, shsu_stmt ssargs stmt
    ) branches
)
| ReturnStmt e -> ssargs, ReturnStmt (shsu_expr ssargs e)
| ExpressionStmt e -> ssargs, ExpressionStmt (shsu_expr ssargs e)
| BlockStmt stmts -> 
    let ssargs, stmts = shsu_full_stmts ssargs stmts in
    ssargs, BlockStmt stmts
and shsu_full_stmt ssargs stmt =  shsu_place _shsu_stmt ssargs stmt
and shsu_stmt ssargs stmt = snd (shsu_full_stmt ssargs stmt)
and shsu_full_stmts ssargs stmts = List.fold_left_map shsu_ful_stmt ssargs stmts
and shsu_stmts ssargs stmts =  snd (shsu_full_stmts ssargs stmts)

(* Function is a binder but can not cross easily the boundaries of components ; 
therefore we use an hybrid representation where function are named
    Component structure is named
    Types are not type-checked
*)