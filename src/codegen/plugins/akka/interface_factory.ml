open Core 
open Interface_plugin
open Interface_plugin

open Easy_logging

let logger = Logging.make_logger "_1_ compspec.codegen" Debug [];;

let plugins : (string, (module Interface_plg)) Hashtbl.t = (Hashtbl.create 10)

let register_plugin (plug : (module Interface_plg)) =
    let module M = (val plug : Interface_plg) in
   
    Hashtbl.add plugins M.name plug 


(**************************Registering plugins********************************)

(*TODO auto register by scandir / dynamic*)
let _ = register_plugin (module Interfaces.GRPC: Interface_plg)

(*****************************************************************************)

let display_available_plugins () = 
    let n = Hashtbl.length plugins in
    Printf.fprintf stdout "%d interface plugin%s %s available:\n" n (if n>1 then "s" else "") (if n>1 then "are" else "is");

    let display_plug (key, value) = 
        let module Plug = (val value:Interface_plg) in    
        if Core.Config.debug () then
            Printf.fprintf stdout "- %s at key %s\n" Plug.name key
        else
            Printf.fprintf stdout "- %s\n" Plug.name
    in
    Seq.iter display_plug (Hashtbl.to_seq plugins)

let load_plugin name : (module Interface_plg) =
    let key = name in
    try
        let plg = Hashtbl.find plugins key in
        let module P = (val plg : Interface_plg) in

        (module P:Interface_plg) 
    with Not_found -> logger#error "Codegeneration plugin %s not found" key; raise Not_found 



