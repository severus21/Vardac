(* 
    - simplify communication to only send/receive + session
    - get ride of branch / then receive
*) 

val rewrite_program : Core.IR.program -> Core.IR.program
include Core.IRCompilationPass.Pass