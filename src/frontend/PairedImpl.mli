(* This module translates [IR] into [IRR]. IR-with-Implementations *)

(* cook the different pieces (ast, place ast) into the IR 
    - ensuring that every name in the Ast_impl is properly bound to the IR part
    - ensuring that every type, state and method in the IR has an abstract implementation or a blackbox one
    - paired blackbox implementation with IR entities 
    - it also add a target to each component according to the Impl bindings
*)

open Core

(* 
    return headers per target, program
    headers per target are external to AST, i.e., orthogonal with IRI program,
*)

module Make : functor (Arg:sig val sealed_envs : (Atom.t, Cook.env) Hashtbl.t end) -> sig 
    val paired_program: Target.targets -> IR.program -> Impl.program ->  (string, IRI.blackbox_term list) Hashtbl.t * IRI.program
end
