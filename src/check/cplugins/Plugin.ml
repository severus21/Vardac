(** Schema for verification and static analysis plugins *)

open Core
open Core.Utils

(* The source calculus. *)
module S = IR

(** External tool interface *)
module type S_Ast = sig
    type program
end;;

module type Cg_plg = sig
    val name: string
    module Ast : S_Ast
   
    val check_program : Ast.program -> unit 
    val to_ast : S.program -> Ast.program
end

module type Plug = sig
    include Cg_plg

    val check_ir_program : IR.program -> unit
end

module Make (Plg: Cg_plg) = struct 
    include Plg

    let check_ir_program program = 
        program
        |> to_ast
        |> check_program
end
