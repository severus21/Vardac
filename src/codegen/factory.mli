open Core
open Plugins
open Plugin

(** Used by plugins to register themselves
    - param1 the plug reprensenting the plugin to register *)
val register_cg_plugin : (module Plugin.Cg_plg) -> unit
(* TODO print the list of currently codegen lg rt plugins
(** Returns the list of currently loaded plugins
    @return the list of loaded plugins *)
val get_plugins : unit -> Plugin.plug list
*)

(** Searches for plugins and load them dynamically *)
val load_plugin : string -> string -> (module Plugin.Plug)

val display_available_plugins : unit -> unit
