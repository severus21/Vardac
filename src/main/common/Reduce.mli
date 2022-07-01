(**  
    Transformation befor type inference/checking/reconstruction
        - binds contract and methods 
*)
open Core

module Make : functor () -> sig 
    val reduce_program: IR.program -> IR.program
    include IRCompilationPass.Pass
end