open Core
open IR
open TypingUtils

(**
    @eturn program if ok (unmodified) - 
    @raise error otherwise
*)
val tcheck_program : program -> program 
include IRCompilationPass.Pass