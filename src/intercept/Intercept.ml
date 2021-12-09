open Core
open AstUtils
open IR
open Easy_logging

let logger = Logging.make_logger ("_1_ compspec.Intercept") Debug [];;

let rewrite_program program = 
    program