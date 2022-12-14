open Core 
open Core.Utils
open Plg.Interface_plugin

open Easy_logging

module TMP = Plg.Interface_plugin.Make(Ast)
open TMP

include TMP.InterfaceFactory.Make(struct
    let logger = make_log_of "Akka.Interfaces"

    let plugins : (string, (module Interface_plg0)) Hashtbl.t = (Hashtbl.create 10)

    let register_plugin (plug : (module Interface_plg0)) =
        let module M = (val plug : Interface_plg0) in
    
        Hashtbl.add plugins M.name plug 


    (**************************Registering plugins********************************)

    (*TODO auto register by scandir / dynamic*)
    let _ = register_plugin (module GRPC: Interface_plg0)
end)