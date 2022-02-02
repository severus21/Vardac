open InterceptUtils
open Core
open AstUtils
open IR
open Easy_logging

let logger = Logging.make_logger ("_1_ compspec.Intercept") Debug [];;

module ContextElimination0 = ContextElimination.Make()
module ContextElimination = Core.IRCompilationPass.Make(ContextElimination0)

let rewrite_program program = 
    let program = ContextElimination.apply program in

    let module InterceptionElimination = InterceptionElimination.Make(struct let interceptors_info = ContextElimination0.interceptors_info end) in
    let module InterceptionElimination = Core.IRCompilationPass.Make(InterceptionElimination) in
    InterceptionElimination.apply program
    
(*********************************************************)

let displayed_pass_shortdescription = "Interception in IR compiled away"
let displayed_ast_name = "interception-less IR"
let show_ast = true

let interception_selector = function 
    | InterceptedActivationRef _ -> true
    | _ -> false

let functor_selector = function
    | VarCExpr x -> Atom.is_builtin x && Atom.hint x = "MakeInterceptor"
    | _ -> false

let precondition program = program

let postcondition program =
    (* Check: no InterceptedActivationRef__ *)
    ignore (collect_expr_program Atom.Set.empty interception_selector (failure_collector_e "InterceptedActivationRef remains in IR") program);

    (* Check: no MakeInterceptor*)
    ignore (collect_cexpr_program functor_selector (failure_collector_ce "Intercept: MakeInterceptor remains in IR") program);
    
    program

let apply_program = rewrite_program