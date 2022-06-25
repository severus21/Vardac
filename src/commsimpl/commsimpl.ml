open Core
open AstUtils
open IR
open Easy_logging
 

let logger = Logging.make_logger ("_1_ vardac.CommSimpl") Debug [];;

module BranchElimination = Core.IRCompilationPass.Make(BranchElimination.Make())
module RecvElimination = Core.IRCompilationPass.Make(RecvElimination.Make())


let selector = function
    | CallExpr ({value=VarExpr x,_}, _) when (Atom.is_builtin x) && (Atom.hint x = "bind") -> true
    | CallExpr ({value=VarExpr x,_}, _) when (Atom.is_builtin x) && (Atom.hint x = "bridgeof") -> true
    | _ -> false

(*
    "bind" -> "bind_in" or "bind_out" to ease encode.ml
    "bridgeof" -> "bridgeof_in" or "bridgeof_out" to ease encode.ml
*)
let speciliaze_call =
    let rewriter place = function 
        | CallExpr ({place=p_bind;value=VarExpr x, mt}, [port; bridge])  when (Atom.is_builtin x) && (Atom.hint x = "bind") ->
            let name = 
                match (snd port.value).value with
                | CType {value=TInport _} -> "bind_in"
                | CType {value=TOutport _} -> "bind_out"
            in

            CallExpr (
                {place=p_bind; value=VarExpr (Atom.builtin name), mt},
                [ port; bridge]
            )
        | CallExpr ({place=p_bridgeof;value=VarExpr x, mt}, [port]) when (Atom.is_builtin x) && (Atom.hint x = "bridgeof") ->
            let name = 
                match (snd port.value).value with
                | CType {value=TInport _} -> "bridgeof_in"
                | CType {value=TOutport _} -> "bridgeof_out"
            in

            CallExpr (
                {place=p_bridgeof; value=VarExpr (Atom.builtin name), mt},
                [ port ]
            )
    in

    rewrite_expr_program selector rewriter

let rewrite_program program=  
    program
    |> BranchElimination.apply
    |> RecvElimination.apply
    |> speciliaze_call
    
(*********************************************************)
let name = "Commsimpl"
let displayed_pass_shortdescription = "Communication has been simplified"
let displayed_ast_name = "simple-comm IR"
let show_ast = true
let global_at_most_once_apply = false

let precondition program = program

let postcondition program = 
    (* Ensure that all "bind" call have been specialized according to the bridge kind *)
    ignore (collect_expr_program Atom.Set.empty selector (fun _ -> 
        raise (Error.DeadbranchError "bind/bridgeof remains after commsimpl")
    ) program);
    program

let apply_program = rewrite_program