open Core
open IR
open TypingUtils

module Make : functor () -> sig
    (**
        @eturn program if ok (unmodified) - 
        @raise error otherwise
    *)
    val tcheck_program : program -> program 
    include IRCompilationPass.Pass
end