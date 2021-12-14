open Core
open AstUtils
open IR
open Easy_logging

let logger = Logging.make_logger ("_1_ compspec.Intercept") Debug [];;

let interception_selector = function 
    | InterceptedActivationInfo _ -> true
    | _ -> false

let functor_selector = function
    | VarCExpr x -> Atom.is_builtin x && Atom.hint x = "MakeInterceptor"
    | _ -> false

let failure_collector msg parent_opt place = 
    let parent = match parent_opt with | None -> "Toplevel" | Some p -> Atom.to_string p in
    Error.error place "%s. Parent = %s" msg parent
let failure_collector_e msg parent_opt env e = failure_collector msg parent_opt e.place 
let failure_collector_ce msg parent_opt place ce = failure_collector msg parent_opt place 


let postcondition program =
    (* Check: no more InterceptedActivationInfo__ *)
    ignore (collect_expr_program Atom.Set.empty interception_selector (failure_collector_e "InterceptedActivationInfo remains in IR") program);

    (* Check: no more MakeInterceptor*)
    ignore (collect_cexpr_program functor_selector (failure_collector_ce "MakeInterceptor remains in IR") program);
    
    program

let rewrite_program program = 
    program
    |> ContextElimination.ctxelim_program
    |> function x-> logger#sinfo "interception ctx has been eliminated from IR";x
    |> dump "interception-ctx-eliminated IR" show_program
    |> failwith "TODO intercept.rewrite" 
    |> postcondition
    