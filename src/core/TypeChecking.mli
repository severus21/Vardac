open IR
open TypingUtils

(*
    Return program if ok (unmodified) - 
    Raise error otherwise
*)
val tcheck_program : program -> program 