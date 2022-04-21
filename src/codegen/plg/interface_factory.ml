open Core
open Interface_plugin

module type SigInterfaceIndex = sig 
    val logger : Easy_logging.Logging.logger
    val plugins : (string, (module Interface_plg)) Hashtbl.t 
end

module type SigInterfaces = sig 
    val display_available_plugins : unit -> unit
    val load_plugin : string -> (module Interface_plg)
end

module Make (Arg: SigInterfaceIndex) = struct 
    include Arg

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
end