(*
    describes an IR -> IR transformation
*)
open IR

module type Pass = sig  
    val displayed_ast_name : string
    val displayed_pass_shortdescription : string
    val show_ast : bool

    val precondition : program -> program
    val apply_program : program -> program
    val postcondition : program -> program
end

module Make (Pass: Pass) : sig
    val apply : program -> program
end = struct
    include Pass

    let apply program = 
        program
        |> precondition
        |> apply_program
        |> function x-> logger#sinfo displayed_pass_shortdescription;x
        |> (if show_ast && Config.debug () then AstUtils.dump displayed_ast_name show_program else Fun.id)
        |> postcondition
end