open Core
module S = IRI

(** Runtime plugin interface *)
module type S_Ast = sig
    type program
end;;

module type Rt_plg = sig
    (*val name: string ifwe need the name we will have to load it from module Desc.name*)
    module IR : S_Ast
    module Ast : S_Ast

    module Finish: sig 
        type collected_state
        module Make : functor () -> sig
            val cstate : collected_state ref
            val finish_program : S.program -> Ast.program
        end
    end
end
module type Lg_plg = sig
    (*val name: string*)
    module Ast : S_Ast

    module Output: sig
        val output_program : string -> Fpath.t -> Ast.program -> unit 
    end
end

module type Cg_plg = sig
    val name: string
    val logger: Easy_logging.Logging.logger

    module Rt : Rt_plg
    module Lg : Lg_plg 

    val templates_location : string
    val externals_location : string

    (* TODO places could be just field of Plug in order not to have to proapgate it and to recompute it 
    
    Same for target

    with a Plug.Make(target)(places)
    
    *)
    module MakeRt2Lg : functor (Arg : sig
        val target:Core.Target.target 
        val cstate:Rt.Finish.collected_state
    end) -> sig 
        val cstate : Rt.Finish.collected_state ref
        val finish_program : Rt.Ast.program -> ((string * Fpath.t) * Lg.Ast.program) list 
    end

    type plgstate
    val plgstate: plgstate ref
    
    val finish_ir_program : Core.Target.target -> S.program -> ((string * Fpath.t) * Lg.Ast.program) list 
    val output_program : Core.Target.target -> Fpath.t -> S.program -> unit

    val custom_template_rules : Core.Target.target -> (Fpath.t * (string * Jingoo.Jg_types.tvalue) list * Fpath.t) list
    val custom_external_rules : unit -> (Fpath.t * Fpath.t) list
    val jingoo_env : Core.Target.target -> plgstate -> Core.IR.vplace list -> (string * Jingoo.Jg_types.tvalue) list
end

(*let todo : (((module Cg_plg) -> unit) -> unit) Queue.t = Queue.create ()*)
let todo : (unit -> unit) Queue.t = Queue.create ()