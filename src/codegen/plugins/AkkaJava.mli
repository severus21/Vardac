val name: string

(* The runtime used. *)
module Rt = Akka 
(* The target programming language. *)
module Lg = Java 

val finish_program : Rt.Ast.program -> Lg.Ast.program 
val finish_ir_program : Plugin.S.program -> Lg.Ast.program 
val output_program : Fpath.t -> Plugin.S.program -> unit

val init_build : Fpath.t -> unit