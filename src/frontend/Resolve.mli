(* resolve preprocessed commands : i.e include*)

(* TODO[low] create a ResolveAst to use type system to hunt processing. 
*)

val resolve_program: Ast.program -> Ast.program
include AstCompilationPass.Pass
