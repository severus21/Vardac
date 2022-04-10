val codegen : Fpath.t -> Fpath.t -> Core.IR.vplace list -> Core.Target.targets -> ((string, Core.IRI.blackbox_term list) Hashtbl.t * Core.IRI.program) -> unit 

val display_available_plugins : unit -> unit

val make_component2target : unit -> (Core.Atom.atom, string) Hashtbl.t  