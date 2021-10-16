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
   - session type
      - check that the two followings patterns do not occured (not supported by the implem)
         - (recrusive st) (timeout 10 st)
         - st (timeout 10 (recusive st))
   - check that method and subcomponent name are unique in a given component
      TODO do the same for states and ports (bridge + session should be unique)
   - event creation or type creation are rewritten as NewExpr (semantics needed for instance we translating Java)
      only for CallExpr (VarExpr, ....)
      prevent constructor aliasing except for protocoldef (FIXME)

*)

open Core

type gamma_t = (IR.expr_variable, IR.main_type) Hashtbl.t
val print_gamma : gamma_t -> unit

val cook_expression: Ast.expr -> IR.expr
val cook_program: IR.vplaces -> Ast.program -> gamma_t * IR.program
