open Core.IR

module Make : functor () -> sig
    val ctxelim_program : program -> program
    include Core.IRCompilationPass.Pass
end