(* This module translates [Ast] into [IR]. *)

(* cook the different pieces (ast, place ast) into the IR 
    - ensuring that every name is properly bound (otherwise, an
   error is reported) and switching from a representation of names as strings to a representation of names as atoms
    - bind contract to method 
    - incorporate vplace into ast
    - detect unbounded variable
    - ensures some safety properties
      * metadata can only be used for session type constraints 
   - typedef -> creation of type constructor inside expression derived using String.lowercase_ascii
   - at most one constructor/destructor per component

*)

open Core

val cook_expression: Ast.expr -> IR.expr
val cook_program: IR.vplaces -> Ast.program -> IR.program
