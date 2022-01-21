open Core 

open Easy_logging

let logger = Logging.make_logger "_1_ compspec.codegen" Debug [];;

let cg_plugins : (string, (module Plugin.Plug)) Hashtbl.t = (Hashtbl.create 10)

(*****************************************************************************)

let register_cg_plugin (plug : (module Registration.Cg_plg)) =
    let module M = (val plug : Registration.Cg_plg) in
   
    Hashtbl.add cg_plugins M.name (module Plugin.Make(M))


let display_available_plugins () = 
    let n = Hashtbl.length cg_plugins in
    Printf.fprintf stdout "%d codegen plugin%s %s available:\n" n (if n>1 then "s" else "") (if n>1 then "are" else "is");

    let display_plug (key, value) = 
        let module Plug = (val value:Plugin.Plug) in    
        if Core.Config.debug () then
            Printf.fprintf stdout "- %s at key %s\n" Plug.name key
        else
            Printf.fprintf stdout "- %s\n" Plug.name
    in
    Seq.iter display_plug (Hashtbl.to_seq cg_plugins)

let load_plugin rt_name lg_name : (module Plugin.Plug) =
    let key = rt_name^"<"^lg_name^">" in
    try
        let cg_plug = Hashtbl.find cg_plugins key in
        cg_plug 
    with Not_found -> logger#error "Codegeneration plugin %s not found" key; raise Not_found 

(*****************************************************************************)

(* load all the available plugins *)
let () =
  try
    Sites.Plugins.Plugins.load_all ();
    List.iter (logger#info "Detected codegen plug %s") (Sites.Plugins.Plugins.list ())
  with exn ->
    Printf.printf "Error during dynamic linking: %s" (Printexc.to_string exn)

let () = List.iter (function x -> logger#info "Loading plugin %s \n" x) (Sites.Plugins.Plugins.list ())
(* Execute the code registered by the plugins *)
let () = Queue.iter (fun f -> f ()) Registration.todo
(*let () = Queue.iter (fun f -> f register_cg_plugin) Registration.todo*)

let () = failwith "toto"