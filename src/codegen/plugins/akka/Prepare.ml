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

let selector_unbox_or_propagate = function 
    | UnboxOrPropagateResult _ -> true 
    |_ ->false
let elim_unbox_or_propagate program = 
    let rewriter parent_opt mt_e e =
        let e = auto_fplace (e, mt_e) in
        let mt_ok = match mt_e.value with
            | CType{value=TResult (mt_ok, _)} -> mt_ok
        in
        (*
            e:Result<T, ...>
            e = rewrite => stmts + e
            ```
            stmts:
                if(e = Err err) {
                    return Err err;
                }
            ```

            e': Either.get e

        *)

        let propagate_or_nothing = 
            auto_fplace (IfStmt(
                e2_e (CallExpr(
                    e2_e (AccessExpr( 
                        e,
                        e2var (Atom.builtin "isLeft")
                    )),
                    [ ]
                )),
                auto_fplace (ReturnStmt(
                    e2_e (CallExpr(
                        e2_e (AccessExpr(
                            e,
                            e2var (Atom.builtin "getLeft")
                        )),
                        [ e ]
                    ))
                )),
                None
            ))
        in
        let unboxed_e = 
            (CallExpr(
                e2_e(AccessExpr(
                    e,
                    e2var (Atom.builtin "get")
                )),
                [ e ]
            ), mt_ok)
        in
        [propagate_or_nothing], unboxed_e
    in
    failwith "Akka.Prepare"
    (* rewrite_exprstmts_program (function _ -> false) selector_unbox_or_propagate rewriter program *)

(*****************************************************)
let name = "Akka.PrepareIRI"
let displayed_pass_shortdescription = Printf.sprintf "Codegen: Prepare AST" 
let displayed_ast_name = "Prepared IRI for codegen plg"
let show_ast = true
let global_at_most_once_apply = false

let precondition program = 
    (* Ensure that they are no UnboxOrPropagateResult anymore *)
    failwith "Akka.Prepare";
    (*collect_expr_program Atom.Set.empty selector_unbox_or_propagate (failwith "UnboxOrPropagateResult") program;*)
    program
let postcondition program = program
let apply_program program =
    program
    |> elim_unbox_or_propagate 