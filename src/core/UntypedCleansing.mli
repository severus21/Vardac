(* functor to hide inner state *)
module Make : functor() -> sig
    include IRCompilationPass.Pass
end