open Core
open Core.Utils
open S_AST

(** Runtime plugin interface *)
module type SigArg = sig
    val build_dir : Fpath.t 
    val target : Target.target
end;;

(* State transmitted from the interface plugin to the codegen plg *)
type istate = {
    jingoo_models: Jingoo.Jg_types.kwargs;
}

let empty_istate () = {
    jingoo_models = [];
}

module Make(Ast:S_Ast) = struct 
    module CompilationPass = Core.CompilationPass.Make2(IRI)(Ast)(struct type acc = Target.target * IRI.program end)

    module type Interface__plg = sig 
        val name : string
        val istate : istate ref

        include CompilationPass.Pass

        val templates_location : string
        val externals_location : string
        val custom_template_rules : unit -> (Fpath.t * (string * Jingoo.Jg_types.tvalue) list * Fpath.t) list
        val custom_external_rules : unit -> (Fpath.t * Fpath.t) list
        val auto_jingoo_env :  unit -> (string * Jingoo.Jg_types.tvalue) list
    end

    module type Interface_plg0 = sig
        val name : string
        val version : string

        val display_info : unit -> unit

        module Make (Arg:SigArg) : Interface__plg
    end

    module type Interface_plg = sig 
        include Interface__plg
        val update_build_dir : Fpath.t -> Fpath.t -> unit
        val resolve_templates : Fpath.t -> Fpath.t -> unit
    end
    
    module MMake (IPlg0:Interface__plg) : Interface_plg = struct
        include IPlg0
        let update_build_dir (project_dir:Fpath.t) (build_dir: Fpath.t) : unit = 
            let module ExternalsHelper = ExternalsHelper.Make(struct 
                let logger = logger
                let name = name
                let build_dir = build_dir
                let externals_location = externals_location 
            end) in
            ExternalsHelper.process_externals None (custom_external_rules ()) (* Plugin interface wide *)
            (* TODO *)
            (*ExternalsHelper.process_externals (Some project_dir) (custom_external_rules ()) (* Project specific *)*)

        let resolve_templates (project_dir:Fpath.t) (build_dir: Fpath.t) : unit = 
            let module TemplatesHelper = TemplatesHelper.Make(struct 
                let logger = logger
                let name = name
                let build_dir = build_dir
                let templates_location = templates_location 
                let custom_template_rules = custom_template_rules () 
            end) in
            let jingoo_models = auto_jingoo_env () in
            TemplatesHelper.process_templates None jingoo_models; (* Plugin wide *)
            (*TemplatesHelper.process_templates (Some project_dir) jingoo_models (* Project specific *)*)
    end

    module InterfaceFactory = struct
        module type SigInterfaceIndex = sig 
            val logger : Easy_logging.Logging.logger
            val plugins : (string, (module Interface_plg0)) Hashtbl.t 
        end

        module type SigInterfaces = sig 
            val display_available_plugins : unit -> unit
            val load_plugin : string -> (module Interface_plg0)
        end

        module Make (Arg: SigInterfaceIndex) = struct 
            include Arg

            let display_available_plugins () = 
                let n = Hashtbl.length plugins in
                Printf.fprintf stdout "\t\t%d interface plugin%s %s available:\n" n (if n>1 then "s" else "") (if n>1 then "are" else "is");

                let display_plug (key, value) = 
                    let module Plug = (val value:Interface_plg0) in    
                    if Core.Config.debug () then
                        Printf.fprintf stdout "\t\t- %s at key %s\n" Plug.name key
                    else begin
                        Printf.fprintf stdout "\t\t- %s [version=%s]\n" Plug.name Plug.version;
                        Plug.display_info ()
                    end
                in
                Seq.iter display_plug (Hashtbl.to_seq plugins)

            let load_plugin name : (module Interface_plg0) =
                let key = name in
                try
                    let plg = Hashtbl.find plugins key in
                    let module P = (val plg : Interface_plg0) in

                    (module P:Interface_plg0) 
                with Not_found -> logger#error "Codegeneration plugin %s not found" key; raise Not_found 

            let load_and_make name build_dir target : (module Interface_plg) *  (module CompilationPass.Pass2) = 
                let module Plg = (val load_plugin name) in
                let module Plg1 : Interface__plg = Plg.Make(struct let build_dir = build_dir let target = target end) in
                let module Plg2 : Interface_plg = MMake(Plg1) in
                (module Plg2:Interface_plg), (module CompilationPass.Make(Plg2):CompilationPass.Pass2)
        end
    end
end
