open IR
open TypingUtils

(* _e, EmptyMainType -> _e, typeof_expr e *)
val tannot_expr : context -> expr -> expr
val tannot_program : program -> program
(* TODO same for CEXPR *)