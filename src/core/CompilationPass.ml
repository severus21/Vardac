(*
    describes an IR -> IR transformation
*)
open IR

module type Pass = sig  
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
        |> postcondition
end