val name: string

(* The runtime used. *)
module Rt = Akka 
(* The target programming language. *)
module Lg = Java 

(* file * program in file *)
val finish_program : Core.Target.target -> Rt.Ast.program -> (Fpath.t * Lg.Ast.program) list  
val finish_ir_program : Core.Target.target -> Plugin.S.program ->  (Fpath.t * Lg.Ast.program) list 
val output_program : Core.Target.target -> Fpath.t -> Plugin.S.program -> unit


val custom_template_rules : Core.Target.target -> (Fpath.t * (string * Jingoo.Jg_types.tvalue) list * Fpath.t) list
val custom_external_rules : unit -> (Fpath.t * Fpath.t) list
val jingoo_env : Core.Target.target -> (string * Jingoo.Jg_types.tvalue) list