open Core
open Core.Utils
open S_AST


(* The source calculus. *)
module S = IRI


module type Rt_plg = sig
    (*val name: string ifwe need the name we will have to load it from module Desc.name*)
    module Ast : S_Ast
    
    module Prepare: IRICompilationPass.Pass

    module Interfaces: Interface_plugin.Make(Ast).InterfaceFactory.SigInterfaces

    module Finish: sig 
        type collected_state
        module Make : functor (Arg:sig val target:Target.target end) -> sig
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

(* Configuration - independent from AST *)
module type CgArgSig = sig
    (* Header general for each file of the target *)
    val headers: string 
    (* Building dependencies *)
    val dependencies: string 
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
    val iplgstate: Interface_plugin.istate ref
    
    val finish_ir_program : Core.Target.target -> Fpath.t -> Fpath.t -> S.program -> ((string * Fpath.t) * Lg.Ast.program) list 
    val output_program : Core.Target.target -> Fpath.t -> Fpath.t -> S.program -> unit

    val custom_template_rules : unit -> (Fpath.t * (string * Jingoo.Jg_types.tvalue) list * Fpath.t) list
    val custom_external_rules : unit -> (Fpath.t * Fpath.t) list
    val auto_jingoo_env : plgstate -> Interface_plugin.istate -> Core.IR.vplace list -> (string * Jingoo.Jg_types.tvalue) list
end

module type Plug = sig
    include Cg_plg

    val init_build_dir : Core.Target.target -> Fpath.t -> Fpath.t -> unit
    val resolve_templates : Core.IR.vplace list -> Fpath.t -> Fpath.t -> unit
end

module type CCg_plg = sig
    val name: string
    module Make : functor (Arg:CgArgSig) -> Cg_plg
end

module Make (Plg: Cg_plg) = struct 
    (* Generic template handling *)
    include Plg

    (*
        lg4dc/
        - externals
            - auto
            - custom
        - templates 
            - auto
            - custom

        difference between custom and auto -> auto location is trivial computed and custom -> programmer provide specific location rules (not yet specific env for template)

        project_name/ (optional)
        - externals 
        ...
        - templates
        ...
        
        precedence rules
        - project_name [externals/templates] overrides lg4dc
        - custom overides auto in project_name and lg4dc (independently)

    *)

    (******************************** External & Templates **********************************)
    let init_build_dir target (project_dir:Fpath.t) (build_dir: Fpath.t) : unit = 
        let module ExternalsHelper = ExternalsHelper.Make(struct 
            let logger = logger
            let name = name
            let build_dir = build_dir
            let externals_location = externals_location 
        end) in
        ExternalsHelper.process_externals None (custom_external_rules ()); (* Plugin wide *)
        ExternalsHelper.process_externals (Some project_dir) (custom_external_rules ()) (* Project specific *)

    let resolve_templates places (project_dir:Fpath.t) (build_dir: Fpath.t) : unit = 
        let module TemplatesHelper = TemplatesHelper.Make(struct 
            let logger = logger
            let name = name
            let build_dir = build_dir
            let templates_location = templates_location 
            let custom_template_rules = custom_template_rules () 
        end) in
        let jingoo_models = auto_jingoo_env !plgstate !iplgstate places in
        TemplatesHelper.process_templates None jingoo_models; (* Plugin wide *)
        TemplatesHelper.process_templates (Some project_dir) jingoo_models (* Project specific *)
end
