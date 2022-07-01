(* 
    - remove MakeInterceptor
*)
module Make : functor () -> sig
    val rewrite_program : Core.IR.program -> Core.IR.program
    include Core.IRCompilationPass.Pass
end