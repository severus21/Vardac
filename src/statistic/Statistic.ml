(* TODO https://sci-hub.mksa.top/10.1016/j.jss.2017.03.044 *)
open Frontend.Ast
open Core.AstUtils
open Easy_logging

let logger = Logging.make_logger "_1_ compspec" Debug [];;

type stats = {
    ssc: int ref (* Source statement count *)
    (* 
        PPterm + Derive + ghost not a statementonly static
        method + component not a statement just a container
    *)
}

let global_stats = {ssc= ref 0}

let rec _analyze_stmt place = 
    logger#debug "analyze stmt";
function
| EmptyStmt | CommentsStmt _ -> () 
| AssignExpr _ | AssignThisExpr _ | LetExpr _ | BreakStmt | ContinueStmt | ExitStmt _ | ReturnStmt _ | ExpressionStmt _ -> incr global_stats.ssc
| ForStmt (_,_,_,stmt) ->
    incr global_stats.ssc;
    analyze_stmt stmt
| IfStmt (_, stmt1, stmt2_opt) ->
    incr global_stats.ssc;
    analyze_stmt stmt1;
    ignore (Option.map analyze_stmt stmt2_opt)
| MatchStmt (_, branches) -> 
    incr global_stats.ssc;
    List.iter (function (_, stmt) -> analyze_stmt stmt) branches
| GhostStmt _ -> failwith "What to do with ghost ???"

and analyze_stmt (stmt:stmt) = map0_place _analyze_stmt stmt

and _analyze_component_item place = function
| Contract {value=c} ->
    if c.pre_binders <> [] then incr global_stats.ssc;
    if c.ensures <> None then incr global_stats.ssc;
    if c.returns <> None then incr global_stats.ssc;
    if c.invariant <> None then incr global_stats.ssc;
| State _ -> incr global_stats.ssc
| Method m -> List.iter analyze_stmt m.value.abstract_impl
| Port _ -> incr global_stats.ssc
| Term t -> analyze_term t
| Include _ -> incr global_stats.ssc
and analyze_component_item (citem:component_item) = map0_place _analyze_component_item citem

and _analyze_component_dcl place = function
| ComponentAssign _ ->  incr global_stats.ssc 
| ComponentStructure cdcl -> List.iter analyze_component_item cdcl.body 
and analyze_component_dcl cdcl = map0_place _analyze_component_dcl cdcl

and _analyze_term place = function
| Comments _ -> ()
| PPTerm _ -> ()
| Stmt stmt -> analyze_stmt stmt
| Component c -> analyze_component_dcl c
| Function fdcl -> List.iter analyze_stmt fdcl.value.abstract_impl 
| Typealias _ | Typedef _ -> incr global_stats.ssc
| Derive derive -> () 
and analyze_term t = map0_place _analyze_term t

let analyze_program program =
    List.iter analyze_term program;

    Format.fprintf Format.std_formatter
    "Statistics on AST\n %a\n\n"
    (function out -> List.iter (function(x,y) -> Format.fprintf out "- %s: %d" x y))
    [ "Source statement count", !(global_stats.ssc)]