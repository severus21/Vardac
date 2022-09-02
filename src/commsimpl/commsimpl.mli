(** 
    - simplify communication to only send/receive + session
    - get ride of branch / then receive
*) 
open Core
open IR

module Make : functor () -> sig
    val rewrite_program : program -> program
    include IRCompilationPass.Pass
end

module Utils : sig  
    val compute_intermediate_args : stmt list -> Atom.atom option -> (main_type * Atom.atom) list
end