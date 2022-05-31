(* This function implements a translation of the intermediate language [IR]
   to [tAkkaAst]. *)

open Core

(* Type used to export collected state to subsequent processing *)
type collected_state = {
    mutable target : Core.Target.target option;
    event2receptionists : (Atom.t, Atom.t list) Hashtbl.t; 
    external2receptionists : (Atom.t, Atom.t list) Hashtbl.t; 
    collected_components: Atom.Set.t ref;
    guardian_components: Atom.Set.t ref;
}
val empty_cstate : unit -> collected_state
val print_cstate : collected_state -> unit

(* functor to hide inner state *)
module Make : functor(Arg: sig val target:Target.target end) -> sig
    val cstate: collected_state ref
    val finish_program: IRI.program -> Ast.program
    include IRI2AstCompilationPass.Pass
end
