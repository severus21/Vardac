(*  
    Transformation befor type inference/checking/reconstruction
        - binds contract and methods 
*)
open Core

val reduce_program: IR.program -> IR.program
include IRCompilationPass.Pass