open Core
open AstUtils
open Plg

open Easy_logging

let logger = Logging.make_logger "_1_ compspec.codegen" Debug [];;

let display_available_plugins = Factory.display_available_plugins 

let codegen_program project_dir build_dir places (target2dependencies, target2headers) ((target, program):Core.Target.target * Core.IRI.program) : unit =
    let build_dir = Fpath.add_seg build_dir target.value.name in
    Utils.refresh_or_create_dir build_dir;

    (* Loading the codegeneration plugin *)
    let aux error_name = function
    | None -> ""
    | Some xs ->
        List.fold_left (fun a b -> a^b) ""
        (List.flatten
            (List.map (
                function (header:Core.IRI.blackbox_term) -> 
                    List.map (function 
                        | Core.IRI.Text str -> 
                            Jingoo.Jg_template.from_string str  ~models:[]
                        | Core.IRI.Varda e -> Error.error e.place "Varda expression is not supported inside headers"
                    ) header.value.body
                ) xs
        ))
    in

    let dependencies = aux "dependencies" (Hashtbl.find_opt target2dependencies target.value.name) in
    let headers = aux "headers" (Hashtbl.find_opt target2headers target.value.name) in

    let plug = Factory.load_plugin (dependencies, headers) target.value.codegen.runtime_plg target.value.codegen.language_plg in 
    let module Plug = (val plug:Plugin.Plug) in    

    (* Generating the code *)
    logger#info "Init build directory \"%s\" with plugin external files"  (Fpath.to_string build_dir);
    Plug.init_build_dir target project_dir build_dir;
    logger#info "Building ...";
    let res = Plug.output_program target project_dir build_dir program in 
    logger#info "Resolve templates (maydepend of the build collected state)";
    Plug.resolve_templates places target project_dir build_dir;
    res 

let codegen project_dir (build_dir:Fpath.t) places targets ((target2dependencies, target2headers), program) = 
    program
    (* Split the AST according to targets *)
    |> Split.split_program targets
    (* Do the codegeneration *) 
    |> Seq.iter (codegen_program project_dir build_dir places (target2dependencies, target2headers))


let  make_component2target () = Split.make_component2target ()