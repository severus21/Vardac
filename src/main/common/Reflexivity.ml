(*
    Annotate and transformate AST to add dynamic reflexivity information
    - access list of ports
    - ??
    - TODO maybe add here intermediate_states = { ... }

    Warning:
        Should be one of the last Varda transformation pass, in order not to miss anything
*)

open Core
open AstUtils
open IR
 
open Easy_logging

let logger = Logging.make_logger ("_1_ compspec.Reflexivity") Debug [];;

let fplace = (Error.forge_place "Reflexivity" 0 0) 
let auto_fplace smth = {place = fplace; value=smth}
include AstUtils2.Mtype.Make(struct let fplace = fplace end)

let rewrite_program program =  
    let selector = function
        | Component {value=ComponentStructure cstruct} ->  true
        | _ -> false
    in

    let rewriter place = function
        | Component {place; value=ComponentStructure cstruct} -> 
            let inports = List.filter_map (function | {value=Inport p} -> Some p |_ -> None) cstruct.body in
            let e_inports = List.map (function (p:port)-> 
                e2_e (AccessExpr(
                    e2_e This,
                    e2var (fst p.value).name
                ))
            ) inports in

            [
                Component {
                    place; 
                    value=ComponentStructure {
                        cstruct with
                            body = 
                                ( auto_fplace (State (auto_fplace (StateDcl {
                                    ghost = false;
                                    type0 = mtype_of_ct (TList (mtype_of_ct (TInport (mtype_of_st STBottom))));
                                    name = Atom.builtin "reflexivity_inports";
                                    body = Some (e2_e (BlockExpr(List, e_inports)));
                                })))) :: cstruct.body
                }}
            ]
    in

    rewrite_term_program selector rewriter program
    
(*********************************************************)
let name = "Reflexivity"
let displayed_pass_shortdescription = "Reflexivity capabilities have been added"
let displayed_ast_name = "reflexivity IR"
let show_ast = true 
let global_at_most_once_apply = true 

let precondition program = program

let postcondition program = program

let apply_program = rewrite_program