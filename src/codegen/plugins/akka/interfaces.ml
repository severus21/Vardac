open Core 
open Plg.Interface_plugin

open Easy_logging

module TMP = Plg.Interface_plugin.Make(Ast)
open TMP

include TMP.InterfaceFactory.Make(struct
    let logger = Logging.make_logger "_1_ compspec.codegen.Akka.Interfaces" Debug [];;

    let plugins : (string, (module Interface_plg)) Hashtbl.t = (Hashtbl.create 10)

    let register_plugin (plug : (module Interface_plg)) =
        let module M = (val plug : Interface_plg) in
    
        Hashtbl.add plugins M.name plug 


    (**************************Registering plugins********************************)

    (*TODO auto register by scandir / dynamic*)
    let _ = register_plugin (module Interfaces_.GRPC: Interface_plg)
end)