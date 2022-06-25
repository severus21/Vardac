open Core 
open Cplugins

open Easy_logging

let logger = Logging.make_logger "_1_ vardac.check" Debug [];;

let plugins : (string, (module Plugin.Plug)) Hashtbl.t = (Hashtbl.create 10)

let register_cg_plugin (plug : (module Plugin.Cg_plg)) =
    let module M = (val plug : Plugin.Cg_plg) in
   
    Hashtbl.add plugins M.name (module Plugin.Make(M))

(************************** Registering plugins ********************************)

(*TODO auto register by scandir / dynamic*)
let _ = () 

(*****************************************************************************)

let display_available_plugins () = 
    let n = Hashtbl.length plugins in
    Printf.fprintf stdout "%d verification plugin%s %s available:\n" n (if n>1 then "s" else "") (if n>1 then "are" else "is");

    let display_plug (key, value) = 
        let module Plug = (val value:Plugin.Plug) in    
        if Core.Config.debug () then
            Printf.fprintf stdout "- %s at key %s\n" Plug.name key
        else
            Printf.fprintf stdout "- %s\n" Plug.name
    in
    Seq.iter display_plug (Hashtbl.to_seq plugins)

let load_plugin plg_name : (module Plugin.Plug) =
    try
        let cg_plug = Hashtbl.find plugins plg_name in
        cg_plug 
    with Not_found -> logger#error "Check plugin %s not found" plg_name; raise Not_found 



