open Core
open AstUtils

open Easy_logging

let logger = Logging.make_logger "_1_ compspec.codegen" Debug [];;

let display_available_plugins = Factory.display_available_plugins 

let process_target (filename:string) =
  filename
  |> ParseTarget.parse_targets
  |> function x-> logger#sinfo "Target file has been parsed"; x
  |> dump "RawTarget" RawTarget.show_targets
  |> CookTarget.cook_targets   
  |> function x-> logger#sinfo "Targets has been cooked"; x
  |> dump "Target" Target.show_targets 


let codegen_program build_dir ((target, program):Target.target * IRI.program) : unit =
    let build_dir = Fpath.add_seg build_dir target.value.name in
    Utils.refresh_or_create_build_dir build_dir;

    let plug = (Factory.load_plugin target.value.codegen.runtime_plg target.value.codegen.language_plg) in 
    let module Plug = (val plug:Plugins.Plugin.Cg_plg) in    

    logger#info "Init build directory \"%s\" with plugin templates and external files"  (Fpath.to_string build_dir);
    Plug.init_build build_dir;
    logger#info "Building ...";
    Plug.output_program build_dir program 

let codegen (build_dir:Fpath.t) targets_file program = 
    (* extract targets definitions from file *)
    let targets = process_target targets_file in

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
    |> Seq.iter (codegen_program build_dir)