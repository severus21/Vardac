(** This module translates [Ast_impl] to [Ast_impl]
    - it removes CurrentDefaultTarget and add target information to all component impl
*)
module Make : functor () -> sig
    val cook_program: Ast_impl.program -> Impl.program

    include Ast2ImplCompilationPass.Pass
end
