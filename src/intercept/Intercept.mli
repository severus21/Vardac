(* 
    TODO rewrite doc
    - remove InterceptedActivation 
    - remove MakeInterceptor function
*)

val rewrite_program : Core.IR.program -> Core.IR.program
include Core.IRCompilationPass.Pass