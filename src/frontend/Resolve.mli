(**  resolve preprocessed commands
    - use: resolve + cycle detection
    - target: include
  
    ghost:
    - remove ghost annotations or remove ghost item 
*)

val resolve_program: Ast.program -> Ast.program
include AstCompilationPass.Pass
