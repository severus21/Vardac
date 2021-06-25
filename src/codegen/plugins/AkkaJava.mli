val name: string

(* The runtime used. *)
module Rt = Akka 
(* The target programming language. *)
module Lg = Java 

(* file * program in file *)
val finish_program : Rt.Ast.program -> (Fpath.t * Lg.Ast.program) Seq.t  
val finish_ir_program : Plugin.S.program ->  (Fpath.t * Lg.Ast.program) Seq.t 
val output_program : Fpath.t -> Plugin.S.program -> unit


val custom_template_rules : unit -> (Fpath.t * (string * Jingoo.Jg_types.tvalue) list * Fpath.t) list
val custom_external_rules : unit -> (Fpath.t * Fpath.t) list
val jingoo_env : unit -> (string * Jingoo.Jg_types.tvalue) list