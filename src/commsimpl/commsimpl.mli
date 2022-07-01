(** 
    - simplify communication to only send/receive + session
    - get ride of branch / then receive
*) 

module Make : functor () -> sig
    val rewrite_program : Core.IR.program -> Core.IR.program
    include Core.IRCompilationPass.Pass
end