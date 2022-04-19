(**
    To be run before TypeInference/...
    - remove type aliasing
        - inline all session types (i.e get ride of session types aliasing + resolve STInline)
    - session type 
        - dual elimination
*)

module Make : functor() -> sig
    include Core.IRCompilationPass.Pass
end