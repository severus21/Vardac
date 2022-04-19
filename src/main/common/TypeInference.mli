open Core
open IR
open TypingUtils

module Make : functor () -> sig
    (* _e, EmptyMainType -> _e, typeof_expr e *)
    val tannot_expr : Atom.atom option -> expr -> expr
    val tannot_program : program -> program
    (* TODO same for CEXPR *)

    include IRCompilationPass.Pass
end