module Make : functor () -> sig
    val output_arg :  Format.formatter -> Ast.decorator list * Ast.jtype * Ast.variable -> unit
    val ojtype :  Format.formatter -> Ast.jtype -> unit
    val output_program : string -> Fpath.t -> Ast.program -> unit 
end 