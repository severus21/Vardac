open Core 
open Plugins

open Easy_logging

let logger = Logging.make_logger "_1_ compspec.codegen" Debug [];;

let cg_plugins : (string, (module Plugin.Cg_plg)) Hashtbl.t = (Hashtbl.create 10)

let register_cg_plugin plug =
    let module M = (val plug : Plugin.Cg_plg) in
    Hashtbl.add cg_plugins M.name plug

(**************************Registering plugins********************************)

(*TODO auto register by scandir / dynamic*)
let _ = register_cg_plugin (module Plugins.AkkaJava: Plugin.Cg_plg)

(*****************************************************************************)

let display_available_plugins () = 
    let n = Hashtbl.length cg_plugins in
    Printf.fprintf stdout "%d plugin%s %s available:\n" n (if n>1 then "s" else "") (if n>1 then "are" else "is");

    let display_plug (key, value) = 
        let module Plug = (val value:Plugins.Plugin.Cg_plg) in    
        if Core.Config.debug () then
            Printf.fprintf stdout "- %s at key %s\n" Plug.name key
        else
            Printf.fprintf stdout "- %s\n" Plug.name
    in
    Seq.iter display_plug (Hashtbl.to_seq cg_plugins)

let load_plugin rt_name lg_name : (module Plugin.Cg_plg) =
    let key = rt_name^"<"^lg_name^">" in
    try
        let cg_plug = Hashtbl.find cg_plugins key in
        cg_plug 
    with Not_found -> logger#error "Codegeneration plugin %s not found" key; raise Not_found 



