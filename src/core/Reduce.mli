(*  
    Transformation befor type inference/checking/reconstruction
        - binds contract and methods 
*)

val reduce_program: IR.program -> IR.program
include CompilationPass.Pass