(* This function implements a translation of the intermediate language [IR]
   to [tAkkaAst]. *)

open Core

val finish_program: IR.program -> Ast.program
