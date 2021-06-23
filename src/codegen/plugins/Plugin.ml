open Core
open Core.Utils
open Akka
open Java

(* The source calculus. *)
module S = IRI

(** Runtime plugin interface *)
module type S_Ast = sig
    type program
end;;

module type Rt_plg = sig
    (*val name: string ifwe need the name we will have to load it from module Desc.name*)
    module Ast : S_Ast

    module Finish: sig 
        val finish_program : S.program -> Ast.program
    end
end
module type Lg_plg = sig
    (*val name: string*)
    module Ast : S_Ast

    module Output: sig
        val output_program : Fpath.t -> Ast.program -> unit 
    end
end

module type Cg_plg = sig
    val name: string
    module Rt : Rt_plg
    module Lg : Lg_plg
    
    val finish_program : Rt.Ast.program -> Lg.Ast.program
    val finish_ir_program : S.program -> Lg.Ast.program
    val output_program : Fpath.t -> S.program -> unit

    val init_build : Fpath.t -> unit
end
