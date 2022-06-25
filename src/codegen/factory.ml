open Core 
open Plugins
open Plg

open Easy_logging

let logger = Logging.make_logger "_1_ vardac.codegen" Debug [];;

let cg_plugins : (string, (module Plugin.CCg_plg)) Hashtbl.t = (Hashtbl.create 10)

let register_cg_plugin (plug : (module Plugin.CCg_plg)) =
    let module M = (val plug : Plugin.CCg_plg) in
   
    Hashtbl.add cg_plugins M.name plug 


(**************************Registering plugins********************************)

(*TODO auto register by scandir / dynamic*)
let _ = register_cg_plugin (module Plugins.AkkaJava: Plugin.CCg_plg)

(*****************************************************************************)

let display_available_plugins () = 
    let n = Hashtbl.length cg_plugins in
    Printf.fprintf stdout "%d codegen plugin%s %s available:\n" n (if n>1 then "s" else "") (if n>1 then "are" else "is");

    let display_plug (key, value) = 
        let module Plug = (val value:Plugin.CCg_plg) in    
        if Core.Config.debug () then
            Printf.fprintf stdout "- %s at key %s\n" Plug.name key
        else
            Printf.fprintf stdout "- %s [version=%s]\n" Plug.name Plug.version;
            Printf.fprintf stdout "\t- language: %s [version=%s]\n" Plug.Lg.name Plug.Lg.version;
            Plug.Lg.display_info();
            Printf.fprintf stdout "\t- runtime: %s [version=%s]\n" Plug.Rt.name Plug.Rt.version;
            Plug.Rt.display_info();
            Plug.Rt.Interfaces.display_available_plugins ();

    in
    Seq.iter display_plug (Hashtbl.to_seq cg_plugins)

let load_plugin (dependencies, headers) rt_name lg_name : (module Plugin.Plug) =
    let key = rt_name^"<"^lg_name^">" in
    try
        let cg_plug = Hashtbl.find cg_plugins key in
        let module P = (val cg_plug: Plugin.CCg_plg) in
        let module PP = P.Make(struct 
            let dependencies = dependencies 
            let headers = headers
        end) in
        let module PPP = Plugin.Make(PP) in

        (module PPP:Plugin.Plug) 
    with Not_found -> logger#error "Codegeneration plugin %s not found" key; raise Not_found 



