open Core
open Core.Utils

(* The source calculus. *)
module S = IRI

(** Runtime plugin interface *)
module type SigArg = sig
    val build_dir : Fpath.t 
end;;

module type Interface_plg = sig
    val name : string
    module Make (Arg:SigArg) : IRICompilationPass.Pass
end
