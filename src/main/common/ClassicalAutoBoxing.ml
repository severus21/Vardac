open Core
open IR
open Easy_logging
open Utils
open AstUtils
open IRMisc

(*
    int -> long
    * let and state
        long x = IntLit i;
        long x = (long_of_int IntLit i)
    * binop
        TODO
    * call args
        TODO
*)
 


module Make () = struct 
    let logger = Core.Utils.make_log_of "ClassicalAutoBoxing"
    let fplace = (Error.forge_place "EventAutoBoxing" 0 0) 
    let auto_fplace smth = {place = fplace; value=smth}
    include AstUtils2.Mtype.Make(struct let fplace = fplace end)


    let autobox_program program : IR.program = 
        let citem_selector = function
        | State {value={
            type0   = {value=CType {value = TFlatType TLong}};
            body    = Some {value=_, {value=CType{value= TFlatType TInt}}}}} -> true
        | _ -> false
        in
        let citem_rewriter _ place = function
        | State ({value={
            type0   = {value=CType {value = TFlatType TLong}};
            body    = Some ({value=_, {value=CType{value= TFlatType TInt}}} as e)}} as s) ->
            [ State { s with
                value = { s.value with body = Some{   
                    place=fplace; 
                    value=
                        CallExpr(
                            e2var (Atom.builtin "long_of_int"),
                            [ e ]
                        )
                    , mtype_of_ft TLong
                }}
            } ]    
        in

        let stmt_selector = function
        | LetStmt({value=CType{value= TFlatType TLong}}, _, {value=_, {value=CType{value= TFlatType TInt}}}) -> true
        | _ -> false
        in
        let stmt_rewriter parent_opt place = function
        | LetStmt({value=CType{value= TFlatType TLong}}as mt, x, ({place=place_e; value=_, {value=CType{value= TFlatType TInt}}} as e)) -> 
            [ LetStmt(
                mt, x, 
                {   place=fplace; 
                    value=
                    CallExpr(
                        e2var (Atom.builtin "long_of_int"),
                        [ e ]
                    )
                    , mtype_of_ft TLong
                }) ]
        in

        program
        |> rewrite_citem_program citem_selector citem_rewriter 
        |> rewrite_stmt_program true stmt_selector stmt_rewriter 
        
    (**********************************************************)
    let name = "ClassicalAutoBoxing"
    let displayed_pass_shortdescription = "ClassicalAutoBoxing"
    let displayed_ast_name = "classical-auto-boxed IR"
    let show_ast = true
    let global_at_most_once_apply = false 


    let precondition program = program
    let postcondition program = program
    let apply_program = autobox_program
end