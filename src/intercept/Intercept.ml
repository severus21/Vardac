open Utils
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

let precondition program = program

let postcondition program =
    (* Check: no InterceptedActivationInfo__ *)
    ignore (collect_expr_program Atom.Set.empty interception_selector (failure_collector_e "InterceptedActivationInfo remains in IR") program);

    (* Check: no MakeInterceptor*)
    ignore (collect_cexpr_program functor_selector (failure_collector_ce "MakeInterceptor remains in IR") program);
    
    program


module ContextElimination = Core.CompilationPass.Make(ContextElimination)
module InterceptionElimination = Core.CompilationPass.Make(InterceptionElimination)

let rewrite_program program = 
    program
    |> ContextElimination.apply
    |> function x-> logger#sinfo "interception ctx has been eliminated from IR";x
    |> dump "interception-ctx-eliminated IR" show_program
    |> InterceptionElimination.apply
    |> function x-> logger#sinfo "interception logic has been eliminated from IR";x
    |> dump "interception-eliminated IR" show_program
    
let apply_program = rewrite_program