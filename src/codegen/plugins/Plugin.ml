open Core
open Core.Utils
open Akka
open Java

(** Runtime plugin interface *)
module type S_Ast = sig
    type program
end;;

module type Rt_plg = sig
    (*val name: string ifwe need the name we will have to load it from module Desc.name*)
    module Ast : S_Ast

    module Finish: sig 
        val finish_program : IR.program -> Ast.program
    end
end
module type Lg_plg = sig
    (*val name: string*)
    module Ast : S_Ast

    module Output: sig
        val output_program : string list -> Ast.program -> unit 
    end
end

module type Cg_plg = sig
    val name: string
    module Rt : Rt_plg
    module Lg : Lg_plg
    
    val finish_program : Rt.Ast.program -> Lg.Ast.program
    val finish_ir_program : IR.program -> Lg.Ast.program
    val output_program : string list -> IR.program -> unit

    val init_build : string list -> unit
end
