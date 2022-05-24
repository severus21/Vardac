open Core
open Utils
open AstUtils
open Easy_logging
open Fieldslib
open Misc
open IRI

let plg_name = "Akka"
let logger = Logging.make_logger ("_1_ compspec.plg."^plg_name) Debug [];;


let fplace = (Error.forge_place ("plg."^plg_name^".PrepareIRI") 0 0) 
let auto_fplace smth = {place = fplace; value=smth}

include AstUtils2.Mtype.Make(struct let fplace = fplace end)

(* The source calculus. *)
module S = IRI 
(* The target calculus. *)
module T = IRI 

let selector_unpack_or_propagate = function 
    | UnopExpr (UnpackOrPropagateResult, e) -> true 
    |_ ->false
let elim_unpack_or_propagate program = 
    let rewriter parent_opt mt_op (UnopExpr (UnpackOrPropagateResult, e)) =
        let mt_ok = match (snd e.value).value with
            | CType{value=TResult (mt_ok, _)} -> mt_ok
        in
        (*
            e:Result<T, ...>
            e = rewrite => stmts + e
            ```
            stmts:
                xxx tmp = e; (* TO avoid duplicating expression *)
                if(tmp = Err err) {
                    return Err err;
                }
            ```

            e': Either.get tmp

        *)
        let tmp = Atom.fresh "tmp" in
        let store = auto_fplace (LetStmt(
            snd e.value,
            tmp,
            e
        )) in

        let propagate_or_nothing = 
            auto_fplace (IfStmt(
                e2_e (CallExpr(
                    e2_e (AccessExpr( 
                        (* Update type of e : TRes<ok, err> -> ok*)
                        {place=e.place@fplace; value = (VarExpr tmp, mt_op)},
                        e2var (Atom.builtin "isLeft")
                    )),
                    [ ]
                )),
                auto_fplace (ReturnStmt(
                    (* Built a new result with a wildcard type for ok type*)
                    e2_e (ResultExpr (
                        None,
                        (* Extract the error*)
                        Some ( 
                            e2_e (CallExpr(
                                e2_e (AccessExpr(
                                    e2var tmp,
                                    e2var (Atom.builtin "getLeft")
                                )),
                                [ ]
                            ))
                        )
                    ))
                )),
                None
            ))
        in
        let unpacked_e = 
            (CallExpr(
                e2_e(AccessExpr(
                    e2var tmp,
                    e2var (Atom.builtin "get")
                )),
                [ ]
            ), mt_ok)
        in
        [store; propagate_or_nothing], unpacked_e
    in
    IRUtils.rewrite_exprstmts_program (function _ -> false) selector_unpack_or_propagate rewriter program

(*****************************************************)
let name = "Akka.PrepareIRI"
let displayed_pass_shortdescription = Printf.sprintf "Codegen: Prepare AST" 
let displayed_ast_name = "Prepared IRI for codegen plg"
let show_ast = true
let global_at_most_once_apply = false

let precondition (program:IRI.program) = 
    (* Check that there is no UnpackOrPropagate into a lambda, since Varda language do not have the capacity to express the elimination (without using a stmt and lambda body is an expression) *)
    let propagate_in_lambda_selector = function 
        | LambdaExpr (_, e) -> 
            let _, elts, _ = IRUtils.collect_expr_expr 
                None 
                Atom.Set.empty 
                selector_unpack_or_propagate 
                (fun _ _ e -> raise (Error.PlacedDeadbranchError(e.place, "UnpackOrResult  operation (e?) can not be performed inside a lambda!!"))) 
                e
            in
           elts <> [] 
        |_ ->false
    in

    IRUtils.collect_expr_program Atom.Set.empty propagate_in_lambda_selector (fun _ _ e -> raise (Error.PlacedDeadbranchError(e.place, "UnpackOrPropagateResult"))) program; 
    program

let postcondition program = 
    (* Ensure that they are no UnpackOrPropagateResult anymore *)
    IRUtils.collect_expr_program Atom.Set.empty selector_unpack_or_propagate (fun _ _ e -> raise (Error.PlacedDeadbranchError(e.place, "UnpackOrPropagateResult"))) program;
    program

let apply_program program =
    program
    |> elim_unpack_or_propagate 