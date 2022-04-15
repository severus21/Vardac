open Core
open AstUtils
open IR
open Easy_logging

let logger = Logging.make_logger ("_1_ compspec.CommSimpl") Debug [];;

module BranchElimination = Core.IRCompilationPass.Make(BranchElimination.Make())
module RecvElimination = Core.IRCompilationPass.Make(RecvElimination.Make())


let bind_selector = function
    | CallExpr ({value=VarExpr x,_}, _) when (Atom.is_builtin x) && (Atom.hint x = "bind") -> true
    | _ -> false

(*
    "bind" -> "bind_in" or "bind_out" to ease encode.ml
*)
let speciliaze_bind =
    let rewriter place = function 
    | CallExpr ({place=p_bind;value=_,mt}, [port; bridge]) ->
        let name = 
            match (snd port.value).value with
            | CType {value=TInport _} -> "bind_in"
            | CType {value=TOutport} -> "bind_out"
        in

        CallExpr (
            {place=p_bind; value=VarExpr (Atom.builtin name), mt},
            [ port; bridge]
        )
    in

    rewrite_expr_program bind_selector rewriter

let rewrite_program program=  
    program
    |> BranchElimination.apply
    |> RecvElimination.apply
    |> speciliaze_bind
    
(*********************************************************)
let name = "Commsimpl"
let displayed_pass_shortdescription = "Communication has been simplified"
let displayed_ast_name = "simple-comm IR"
let show_ast = true
let global_at_most_once_apply = false

let precondition program = program

let postcondition program = 
    (* Ensure that all "bind" call have been specialized according to the bridge kind *)
    collect_expr_program Atom.Set.empty bind_selector (fun _ -> 
        raise (Error.DeadbranchError "bind remains after commsimpl")
    );
    program

let apply_program = rewrite_program