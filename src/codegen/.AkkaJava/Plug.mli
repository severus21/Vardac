val name: string
val logger: Easy_logging.Logging.logger

(* The runtime used. *)
module Rt = Akka 
(* The target programming language. *)
module Lg = Java 

val templates_location : string
val externals_location : string

(* file * program in file *)
module MakeRt2Lg : functor (Arg:sig
    val target: Core.Target.target 
    val cstate: Rt.Finish.collected_state
end) -> sig
    val cstate : Rt.Finish.collected_state ref
    val finish_program : Rt.Ast.program -> ((string * Fpath.t) * Lg.Ast.program) list  
end

type plgstate = Rt.Finish.collected_state
val plgstate: plgstate ref

val finish_ir_program : Core.Target.target -> Registration.Plugin.S.program ->  ((string * Fpath.t) * Lg.Ast.program) list 
val output_program : Core.Target.target -> Fpath.t -> Registration.Plugin.S.program -> unit


val custom_template_rules : Core.Target.target -> (Fpath.t * (string * Jingoo.Jg_types.tvalue) list * Fpath.t) list
val custom_external_rules : unit -> (Fpath.t * Fpath.t) list
val jingoo_env : Core.Target.target -> Rt.Finish.collected_state -> Core.IR.vplace list -> (string * Jingoo.Jg_types.tvalue) list