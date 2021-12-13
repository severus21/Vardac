open Core
open AstUtils
open IR
open Easy_logging

let logger = Logging.make_logger ("_1_ compspec.Intercept") Debug [];;

let rewrite_program program = 
    program
    |> ContextElimination.ctxelim_program
    |> function x-> logger#sinfo "interception ctx has been eliminated from IR";x
    |> dump "interception-ctx-eliminated IR" show_program
    |> failwith "TODO intercept.rewrite" 
    