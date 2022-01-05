(*
    describes an IR -> IR transformation
*)

open Easy_logging
let logger = Logging.make_logger "_1_" Debug [];;

module type Arg = sig 
    type program
    val show_program : program -> string 
end
module Make(S:Arg)(T:Arg) = struct 
    module type Pass = sig  
        val displayed_ast_name : string
        val displayed_pass_shortdescription : string
        val show_ast : bool

        val precondition : S.program -> S.program
        val apply_program : S.program -> T.program
        val postcondition : T.program -> T.program
    end

    module Make (Pass: Pass) : sig
        val apply : S.program -> T.program
    end = struct
        include Pass

        let apply program = 
            program
            |> precondition
            |> apply_program
            |> function x-> logger#sinfo displayed_pass_shortdescription;x
            |> (if show_ast && Config.debug () then AstUtils.dump displayed_ast_name T.show_program else Fun.id)
            |> postcondition
    end
end

module Make2(S:Arg)(T:Arg)(Acc:sig type acc end) = struct 
    module type Pass = sig  
        val displayed_ast_name : string
        val displayed_pass_shortdescription : string
        val show_ast : bool

        val precondition : S.program -> S.program
        val apply_program : S.program -> (Acc.acc * T.program) list
        val postcondition : T.program -> T.program
    end

    module Make (Pass: Pass) : sig
        val apply : S.program -> (Acc.acc * T.program) list
    end = struct
        include Pass

        let apply program = 
            program
            |> precondition
            |> apply_program
            |> function x-> logger#sinfo displayed_pass_shortdescription;x
            |> List.map (function (acc, program) -> acc, (if show_ast && Config.debug () then AstUtils.dump displayed_ast_name T.show_program else Fun.id) program)
            |> List.map (function (acc, program) -> acc, postcondition program)
    end
end