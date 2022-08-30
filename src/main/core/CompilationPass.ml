(**
    describes an IR -> IR transformation
*)

open Easy_logging

module type Arg = sig 
    type program
    val show_program : program -> string 
    val program_to_yojson : program -> Yojson.Safe.t 
end
module Make(S:Arg)(T:Arg) = struct 
    module type Pass = sig  
        val name : string
        val displayed_ast_name : string
        val displayed_pass_shortdescription : string
        val show_ast : bool

        (***
            If set to "true" this pass should not be applied more than once
            This is a global property for all generated Pass (using this Make) and sharing the same name
        *)
        val global_at_most_once_apply : bool

        val precondition : S.program -> S.program
        val apply_program : S.program -> T.program
        val postcondition : T.program -> T.program
    end
    module type Pass2 = sig 
        val apply : S.program -> T.program
    end

    module Make (Pass: Pass) : Pass2 = struct
        include Pass


        let apply program = 
            if Config.already_applied_pass name && global_at_most_once_apply then
                raise (Error.DeadbranchError (Printf.sprintf "Pass [%s] has been applied twice, this pass requires global_at_most_once_apply" name))
            else begin
                let logger = Logging.make_logger ("vardac") Debug [] in

                let program = 
                    program
                    |> precondition
                    |> apply_program
                    |> function x-> logger#sinfo displayed_pass_shortdescription;x
                    |> (if show_ast && Config.debug () then AstUtils.dump_selected name displayed_ast_name T.show_program T.program_to_yojson else Fun.id)
                    |> postcondition
                in

                Config.register_pass name;
                program
            end
    end
end

module Make2(S:Arg)(T:Arg)(Acc:sig type acc end) = struct 
    module type Pass = sig  
        val name : string
        val displayed_ast_name : string
        val displayed_pass_shortdescription : string
        val show_ast : bool

        val precondition : S.program -> S.program
        val apply_program : S.program -> (Acc.acc * T.program) list
        val postcondition : T.program -> T.program
    end
    
    module type Pass2 = sig 
        val apply : S.program -> (Acc.acc * T.program) list
    end

    module Make (Pass: Pass) : Pass2 = struct
        include Pass

        let apply program = 
            let logger = Logging.make_logger ("vardac") Debug [] in

            program
            |> precondition
            |> apply_program
            |> function x-> logger#sinfo displayed_pass_shortdescription;x
            |> List.map (function (acc, program) -> acc, (
                    if show_ast && Config.debug () then 
                        AstUtils.dump_selected name displayed_ast_name T.show_program T.program_to_yojson 
                    else Fun.id
                ) program)
            |> List.map (function (acc, program) -> acc, postcondition program)
    end
end