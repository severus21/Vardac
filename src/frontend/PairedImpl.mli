(* This module translates [IR] into [IRR]. IR-with-Implementations *)

(* cook the different pieces (ast, place ast) into the IR 
    - ensuring that every name in the Ast_impl is properly bound to the IR part
    - ensuring that every type, state and method in the IR has an abstract implementation or a blackbox one
    - paired blackbox implementation with IR entities 
*)

open Core

val paired_program: IR.program -> Ast_impl.program -> IRI.program
