(* 
    - remove MakeInterceptor
*)

val rewrite_program : Core.IR.program -> Core.IR.program
include Core.IRCompilationPass.Pass