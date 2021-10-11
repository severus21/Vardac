(* This function implements a translation of the intermediate language [IR]
   to [tAkkaAst]. *)

open Core

(* Type used to export collected state to subsequent processing *)
type collected_state = {
    event2receptionists : (Atom.t, Atom.t list) Hashtbl.t; 
    collected_components: Atom.Set.t ref
}
val empty_cstate : unit -> collected_state
val print_cstate : collected_state -> unit

val finish_program: IRI.program -> collected_state * Ast.program
