(*
    Clean Java AST:
    - rewrite toplevel, inside class, let stmt to field declarations  
*)
val clean_program : Ast.program -> Ast.program 