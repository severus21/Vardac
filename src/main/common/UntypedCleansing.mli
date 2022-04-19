(* functor to hide inner state *)
module Make : functor() -> sig
    include Core.IRCompilationPass.Pass
end