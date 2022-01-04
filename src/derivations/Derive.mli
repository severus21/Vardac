(* 
    Handle derivation - rewrite the architectures
*)

val derive_program : Core.IR.program -> Core.IR.program
include Core.CompilationPass.Pass
