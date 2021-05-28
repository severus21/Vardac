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

let codegen build_dir targets_file program = 
    (* extract targets definitions from file *)
    let targets = process_target targets_file in

    (* build a { target -> plugin } dictionary *)
    let build_plug = function
        | {place; value=Target.Target {name; codegen}} as target -> 
            target, Factory.load_plugin codegen.runtime_plg codegen.language_plg
    in
    let plugs = Hashtbl.of_seq (List.to_seq (List.map build_plug targets) ) in

    logger#info "Plugins loaded: %d" (Hashtbl.length plugs);

    (* TODO Process the targets*)

    (* TODO gather rt and lg name from file *)
    
    let plug = (Factory.load_plugin "Akka" "Java") in 
    let module Plug = (val plug:Plugins.Plugin.Cg_plg) in    
    logger#info "Init build directory with plugin templates and external files";
    Plug.init_build build_dir;
    logger#info "Building ...";
    Plug.output_program build_dir program 