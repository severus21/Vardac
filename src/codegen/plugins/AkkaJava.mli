val name: string

(* The runtime used. *)
module Rt = Akka 
(* The target programming language. *)
module Lg = Java 

val finish_program : Rt.Ast.program -> Lg.Ast.program 
val finish_ir_program : Core.IR.program -> Lg.Ast.program 
val output_program : string list -> Core.IR.program -> unit

val init_build : string list -> unit