(*
    Clean Java AST:
    - deduplicate annotations and decorators  
    - method with Void return type without return stmt is rewritten to void return type.
*)
val clean_program : Ast.program -> Ast.program 