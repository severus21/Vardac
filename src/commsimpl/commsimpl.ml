open Core
open AstUtils
open IR
open Easy_logging

let logger = Logging.make_logger ("_1_ compspec.CommSimpl") Debug [];;

module BranchElimination = Core.IRCompilationPass.Make(BranchElimination.Make())
module RecvElimination = Core.IRCompilationPass.Make(RecvElimination.Make())
let rewrite_program program=  
    program
    |> BranchElimination.apply
    |> RecvElimination.apply
    
(*********************************************************)

let displayed_pass_shortdescription = "Communication has been simplified"
let displayed_ast_name = "simple-comm IR"
let show_ast = true

let precondition program = program

let postcondition program = program

let apply_program = rewrite_program