open Core
open AstUtils

open Easy_logging

let logger = Logging.make_logger "_1_ compspec.check" Debug [];;

let display_available_plugins = Factory.display_available_plugins 

let check program plug_name : unit =
    let plug = (Factory.load_plugin plug_name) in 
    let module Plug = (val plug:Cplugins.Plugin.Plug) in    

    logger#info "Checking with %s ..." Plug.name;
    Plug.check_ir_program program

let check_program project_dir build_dir program = 
    let checking_strategy : string list = [] in 
    List.iter (check program) checking_strategy 