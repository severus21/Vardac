(* 
    Check that there is no discrepency between config and architecture
    * That there is no usage of a placement reflexivity primitives if enable_global_placement_reflexivity is unset
    * That there is no usage of a bridge reflexivity primitives if enable_global_bridge_reflexivity is unset
*)

open Core
open AstUtils
open IR
open Easy_logging

let logger = Core.Utils.make_log_of "Sanitiser" 

let name = "Sanitiser"
let displayed_pass_shortdescription = "IR has been sanitise"
let displayed_ast_name = "sanitised IR"
let show_ast = true
let global_at_most_once_apply = false

let precondition program = 
    let selector = function
        | VarExpr x -> Atom.is_builtin x 
        | _ -> false
    in

    let collector parent_opt env = function
        | {place; value=VarExpr x, _} -> begin
            match Atom.hint x with 
            | "register_activation_at" | "placeof" | "places" | "select_places" when Bool.not (Config.enable_global_placement_reflexivity()) ->
                Error.perror place "%s can not be used with placement_reflexivity is globally disabled" (Atom.to_string x)
            | "leftactivations" | "rightactivations" | "leftregister" | "rightregister" when Bool.not (Config.enable_global_bridge_reflexivity()) ->
                Error.perror place "%s can not be used with bridge_reflexivity is globally disabled" (Atom.to_string x)
            | _ -> [] 
        end
        | _ -> raise (Error.DeadbranchError "selector prevents accessing this branch")
    in

    ignore(collect_expr_program Atom.Set.empty selector collector program);

    program

let postcondition program = 
    program
let apply_program = Fun.id 