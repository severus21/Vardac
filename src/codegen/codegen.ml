open Core
open AstUtils

open Easy_logging

let logger = Logging.make_logger "_1_ compspec.codegen" Debug [];;

let display_available_plugins = Factory.display_available_plugins 

let codegen_program project_dir build_dir places ((target, program):Target.target * IRI.program) : unit =
    let build_dir = Fpath.add_seg build_dir target.value.name in
    Utils.refresh_or_create_build_dir build_dir;

    let plug = (Factory.load_plugin target.value.codegen.runtime_plg target.value.codegen.language_plg) in 
    let module Plug = (val plug:Plugins.Plugin.Plug) in    

    logger#info "Init build directory \"%s\" with plugin external files"  (Fpath.to_string build_dir);
    Plug.init_build_dir target project_dir build_dir;
    logger#info "Building ...";
    let res = Plug.output_program target build_dir program in 
    logger#info "Resolve templates (maydepend of the build collected state)";
    Plug.resolve_templates places target project_dir build_dir;
    res 

let codegen project_dir (build_dir:Fpath.t) places targets program = 
    (* build a { target -> plugin } dictionary *)
    let build_plug = function
        | {place; value={Target.name; codegen}} as target -> 
            target, Factory.load_plugin codegen.runtime_plg codegen.language_plg
    in
    let plugs = Hashtbl.of_seq (List.to_seq (List.map build_plug targets) ) in
    logger#warning "%s" (Target.show_targets targets);
    logger#info "Plugins loaded: %d" (Hashtbl.length plugs);

    program
    (* Split the AST according to targets *)
    |> Split.split_program targets
    (* Do the codegeneration *) 
    |> Seq.iter (codegen_program project_dir build_dir places)