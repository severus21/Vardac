open Core
open Utils
open AstUtils
open Easy_logging
open Fieldslib
open Misc
open IRI

module Make () = struct
    let pass_name = "Akka.PrepareIRI"
    let logger = make_log_of pass_name


    let fplace = (Error.forge_place pass_name 0 0) 
    let auto_fplace smth = {place = fplace; value=smth}

    include AstUtils2.Mtype.Make(struct let fplace = fplace end)

    (* The source calculus. *)
    module S = IRI 
    (* The target calculus. *)
    module T = IRI 

    let selector_unpack_or_propagate = function 
        | UnopExpr (UnpackOrPropagateResult, e) -> true 
        |_ -> false
    let elim_unpack_or_propagate program = 
        (* Rewrite LambdaExpr first since the classical can not be used, can not store return *)
        let rewrite_lambda_expr parent_opt place = function 
            | LambdaExpr (params, e) -> 

                let _, elts, _ = IRUtils.collect_expr_expr 
                    None Atom.Set.empty selector_unpack_or_propagate (fun _ _ -> function |{value=UnopExpr (UnpackOrPropagateResult, e),_} -> [e]) e in  

                match elts with
                | [] -> LambdaExpr(params, e)
                | [result] ->
                    let x = Atom.fresh "x" in
                    let body = 
                        e2_e (LambdaExpr( 
                            [auto_fplace (snd result.value,x)],
                            e2_e (TernaryExpr(
                                e2var (Atom.builtin "is_ok"),
                                e2_e(CallExpr(
                                    rewrite_expr_expr parent_opt selector_unpack_or_propagate (fun _ _ _ -> VarExpr x) e,
                                    [
                                        {
                                            place = fplace;
                                            value= (
                                                CallExpr( e2_e (RawExpr "get"), [e2var x]), 
                                                match (snd result.value).value with
                                                | CType{value=TResult(mt_ok,_)} -> mt_ok
                                                | _ -> raise (Error.DeadbranchError "should be a result type")
                                            )
                                        } 
                                    ]
                                )),
                                e2var x
                            ))
                        ))
                    in
                    LambdaExpr(params,
                        e2_e (CallExpr(
                            body,
                            [ result ]
                        ))
                    )
                | _ ->  Error.error "%s" "UnpackOrPropagateResult remains after Akka.prepare"
        in
        let program = rewrite_expr_program (function | LambdaExpr _ -> true |_ -> false) rewrite_lambda_expr program in





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
            let tmp = Atom.fresh "tmp_porpagate" in
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
                                        {place=e.place@fplace; value = (VarExpr tmp, mt_op)},
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
                        {place=e.place@fplace; value = (VarExpr tmp, mt_op)},
                        e2var (Atom.builtin "get")
                    )),
                    [ ]
                ), mt_ok)
            in
            [store; propagate_or_nothing], unpacked_e
        in

        (* FIXME recruse does not works as expected since we must rewrite also the generated stmts *)
        program
        |> IRUtils.rewrite_exprstmts_program ~recurse:true (function _ -> false) selector_unpack_or_propagate rewriter

   
    (* 
        Varda: in class body, [this] denotes the current activation whereas [self] denotes the object
        Java: [self] -> [this] and [this] [this] -> [this]
        Therefore we need to emulate the distinction between [self] and [this]    
    *)    
    let elim_self_this program = 
        let cls = ref Atom.Set.empty in
        let self_parents = Hashtbl.create 16 in

        let select_class = function
            | Class _ -> true
            | _ -> false
        in
        let rewrite_class parent_opt place = function
            | Class cl -> begin
                cls := Atom.Set.add cl.name !cls;

                let a_parent_this = Atom.fresh "parent_this" in
                let self_parent_this = Atom.fresh "parent_this" in
                Hashtbl.add self_parents cl.name self_parent_this;
                let mt_parent_this = match parent_opt with
                    | None -> raise (Error.PlacedDeadbranchError (place, "varda class must be defined inside component"))
                    | Some x -> mtype_of_cvar x
                in


                let select_this = function
                    | This -> true 
                    | _ -> false
                in 
                let rewrite_this _ mt = function 
                    | This -> AccessExpr(
                        e2_e Self,
                        e2var self_parent_this
                    )
                in

                let cl = {cl with 
                    body = auto_fplace(auto_plgannot(CLState(auto_fplace {
                        ghost = false;
                        name = self_parent_this;
                        type0 = mt_parent_this;
                        body = NoInit;
                }))) ::(List.map (rewrite_expr_class_item None select_this rewrite_this) cl.body) } in

                let constructor = get_clconstructor cl in
                let constructor = match constructor with 
                | None -> auto_fplace {
                    annotations  = [];
                    ghost = false;
                    ret_type = mtype_of_ft TVoid;
                    name = Atom.fresh "constructor";
                    args = [auto_fplace(mt_parent_this, a_parent_this)];
                    body = AbstractImpl [
                        auto_fplace (AssignSelfExpr(
                                self_parent_this,
                                e2var a_parent_this
                            ))
                    ];
                    contract_opt = None;
                    on_destroy = false;
                    on_startup = true;
                }
                | Some constructor -> { constructor with 
                    value = {constructor.value with 
                        args = (auto_fplace(mt_parent_this, a_parent_this)) ::constructor.value.args;
                        body = match constructor.value.body with
                        | AbstractImpl body -> 
                            AbstractImpl((
                                auto_fplace (AssignSelfExpr(
                                    self_parent_this,
                                    e2var a_parent_this
                                ))
                            ) :: body);
                        | BBImpl _ -> Error.perror constructor.place "[onstartup/constructor] method can not have abstract implementation!"
                    }
                } in
                [Class (replace_clconstructor cl constructor)]
            end
        in

        let select_instantiation = function 
            | Create {c} -> 
                logger#debug "select_instantiation of [%s] <%b>" (Atom.to_string c) (Atom.Set.mem c !cls);
                Atom.Set.mem c !cls 
            | _ -> false
        in
        let rewrite_instantiation parent_opt place = function 
            | Create {c; args} -> begin
                logger#debug "rewrite [NewExpr] of [%s]" (Atom.to_string c);
                try 
                    let self_parent_this = Hashtbl.find self_parents c in
                    (* FIXME works since cl can not be nested *) 
                    Create {c; args = (e2_e This)::args}
                with Not_found -> raise (Error.DeadbranchError "cl is registered in [cls] but ,not in [self_parents]")
            end
        in

        program 
        |> rewrite_term_program select_class rewrite_class
        |> rewrite_expr_program select_instantiation rewrite_instantiation


    let correct_type_intermediate_futures program = 
        let selector = function
            | CallExpr(
                {value=VarExpr x, _},
                [
                    {value = AccessExpr(
                        {value = This, _},
                        {value = VarExpr y, _}
                    ),_};
                    e2;
                    f
                ]
            ) when Atom.hint x = "add2dict" &&  Atom.hint y = "intermediate_futures" -> true 
            | _ -> false
        in 

        let rewritor parent_opt mt = function
        | CallExpr(e1, [e2; e3; f]) -> 
            CallExpr (e1,
                [ e2; e3;
                    e2_e(AccessExpr(
                        f,
                        e2_e (RawExpr "thenApply(x -> (Object) x)")
                    ))
                ]
            )
        in

        rewrite_expr_program selector rewritor program
    
    
    (*****************************************************)
    let name = "Akka.PrepareIRI"
    let displayed_pass_shortdescription = Printf.sprintf "Codegen: Prepare AST" 
    let displayed_ast_name = "Prepared IRI for codegen plg"
    let show_ast = true
    let global_at_most_once_apply = true 

    let precondition (program:IRI.program) = 
        program

    let postcondition program = 
        (* Ensure that they are no UnpackOrPropagateResult anymore *)
        IRUtils.collect_expr_program Atom.Set.empty selector_unpack_or_propagate (fun _ _ e -> raise (Error.PlacedDeadbranchError(e.place, "UnpackOrPropagateResult remains after Akka.prepare"))) program;
        program

    let apply_program program : program =
        program
        |> elim_unpack_or_propagate 
        |> elim_self_this
        |> correct_type_intermediate_futures
end