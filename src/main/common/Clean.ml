(**
    Cleansing
        ExpressionStmt EmptyExpr => EmptyStmt
        ExpressionStmt VarExpr _ => EmptyStmt
            because VarExpr has no side effects and ExpressionStmt is not an expr (can not be reduce to a value)
            e.g. function inlining can trigger the insertion of such garbage stmt (call() => ...; ret_var;)
        BlockStmt [stmt] => stmt

    Post condition 
        No more EmptyExpr (otherwise they are not in ExpressionStmt => error)
*)

open Core
open AstUtils
open IR
open Easy_logging
 
module Make () = struct 
    let logger = Core.Utils.make_log_of "Clean" 



    let clean_program program = 
        let stmt_selector = function
            | ExpressionStmt {value=EmptyExpr, _} -> true
            | ExpressionStmt {value=VarExpr _, _} -> true
            | BlockStmt [stmt] -> true
            | _ -> false
        in

        let stmt_rewriter parent_opt place = function
            | ExpressionStmt {value=EmptyExpr, _} -> [EmptyStmt] 
            | ExpressionStmt {value=VarExpr _, _} -> [EmptyStmt] 
            | BlockStmt [stmt] -> [stmt.value]
            | _ -> raise (Error.DeadbranchError "selector prevents accessing this branch")
        in

        rewrite_stmt_program true stmt_selector stmt_rewriter program    

    (*****************************************************)
    let name = "Clean"
    let displayed_pass_shortdescription = "IR has been cleaned"
    let displayed_ast_name = "cleaned IR"
    let show_ast = true
    let global_at_most_once_apply = false

    let precondition program = program

    let postcondition program = 
        let selector = function
            | EmptyExpr -> true 
            | _ -> false
        in

        let collector parent_opt env = function
            | {place; value=EmptyExpr, _} ->  raise (Error.PlacedDeadbranchError (place, "EmptyExpr reamins in IR after cleansing, i.e. it is used otherwise than inside ExpressionStmt EmptyExpr"))
            | _ -> raise (Error.DeadbranchError "selector prevents accessing this branch")
        in

        ignore(collect_expr_program Atom.Set.empty selector collector program);

        program
    let apply_program = clean_program
end