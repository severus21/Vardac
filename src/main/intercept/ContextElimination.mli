(** Compile away the interception contexts
    see. wiki/interception for intuition and technical details 
*)

open Core
open IR
open InterceptUtils

module Make : functor () -> sig
    val ctxelim_program : program -> program
    include Core.IRCompilationPass.Pass

    (* Shared state between ctx elim and intercept elim *)
    val interceptors_info : (Atom.atom, interceptor_info) Hashtbl.t
end