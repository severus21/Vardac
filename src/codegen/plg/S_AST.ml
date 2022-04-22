(** Runtime plugin interface *)
module type S_Ast = sig
    type program
    type blackbox_term
    val show_program : program -> string 
end