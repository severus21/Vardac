(**
    Clean Java AST:
    - deduplicate annotations and decorators  
    - check that each method ends with a return stmt
        - method with Void return type without return stmt is rewritten to void return type.
        - method with Either<..., Void> return type without return stmt is rewritten to Either.right(void) return type.
    - rewrite lambda expr call
        (x -> y)(z) -> ((Function<T1, T2>)x->y).apply(z)
        t(z) such that t:Arrow  -> t.apply(z)
*)
module Make : functor (Arg : sig val filename : string val toplevel_functions : Core.Atom.Set.t end) -> sig 
    include AstCompilationPass.Pass
end