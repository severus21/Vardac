(*
    Can not use future.get(timeout) in Akka block the threat and the other async methods ...
*)
open Core
open Utils
open AstUtils
open Easy_logging
open Fieldslib
open Misc
open IRI


module Make () = struct

    let pass_name = "Akka.FutureElim"
    let logger = make_log_of pass_name 

    let fplace = (Error.forge_place pass_name 0 0) 
    let auto_fplace smth = {place = fplace; value=smth}

    include AstUtils2.Mtype.Make(struct let fplace = fplace end)

    (* The source calculus. *)
    module S = IRI 
    (* The target calculus. *)
    module T = IRI 


    let intermediate_futures_tbl = Hashtbl.create 16


    (*
        to_X_form + split
        stmts A
        wait_future()
        stmts B
        for wait_future and for function calling function with wait_future inside ....
    *)

    let selected = ref 0 
    let select_new_future = function
    | LetStmt (_, _, {value=CallExpr ({value=VarExpr x,_}, []), _}) ->
        selected := 2;
        Atom.is_builtin x && Atom.hint x = "future"
    | _ when !selected > 0 -> 
        decr selected;
        true
    | _ -> false

    let rewrite_new_future parent_opt place = function
        (*
            LetStmt(..., f, future());
            add2dict(..., f_id, f);
            return f;

            becomes [ return new_id ]


        *)
    | ExpressionStmt {value=CallExpr (_, [_; id; _]),_} -> 
        [ ReturnStmt (e2_e (ResultExpr(
            Some id, None
            ))) ]
    | _ -> [ EmptyStmt ]



    let warp_complete_future_method parent_opt place = function 
    | Method ({value={body=AbstractImpl body}} as m) -> begin
        let contains_complete_future = ref false in
        let select_complete_future = function
            | ExpressionStmt {value=CallExpr ({value=VarExpr x, _}, _),_} | ReturnStmt {value=CallExpr ({value=VarExpr x, _}, _), _} -> Atom.is_builtin x && Atom.hint x = "complete_future"
            | _ -> false
        in

        let rewrite_complete_future parent_opt place = function
            (*
                complete_future(get2dict(..., id), value)

                becomes ! ResolvedResult(id, value)
            *)
            | ExpressionStmt {value=CallExpr (_, [{value=CallExpr (_, [this_intermediate_futures; id]), _}; value]), _} | ReturnStmt {value=CallExpr (_, [{value=CallExpr (_, [this_intermediate_futures; id]), _}; value]), _} ->
                contains_complete_future := true;
                [ ReturnStmt(
                    e2_e(CallExpr(
                        e2_e(RawExpr "new ResolvedResult"),
                        [id; value]
                    ))
                ) ]
        in

        let body = List.map (rewrite_stmt_stmt false parent_opt select_complete_future rewrite_complete_future) body in
        let body = List.flatten body in

        (* if m  contains complete_fture 
            create an innner m which returns a resolved result 
            and m beomes the wrapper that send it and return the write type
            therefore -> elim future is nearly transparent for contract rewritting
        *)
        if !contains_complete_future then (
            let inner_name = Atom.fresh ((Atom.hint m.value.name)^"_inner") in
            let inner_method = auto_fplace{m.value with 
                    contract_opt = m.value.contract_opt;
                    name = inner_name; 
                    ret_type = mtype_of_raw "ResolvedResult";
                    body = AbstractImpl body;
                }
            in

            let m = {
                place = m.place@fplace;
                value = { m.value with
                    contract_opt = None;
                    body = AbstractImpl [
                        stmt2_e (CallExpr(
                            e2_e( RawExpr "getContext().getSelf().tell"),
                            [
                                e2_e(CallExpr(
                                    e2_e (AccessExpr(e2_e This, e2var inner_name)),
                                    List.map (function {value=_,x} -> e2var x) m.value.args 
                                ))

                            ] 
                        ));
                        auto_fplace (ReturnStmt (e2_e (ResultExpr (Some (e2_lit VoidLit), None))))
                    ] 
                } 
            } in

            [ Method m; Method inner_method]
        ) else (
            [ Method m ]
        )
    end
    | citem -> [citem]


    let select_wait_future = function
    | Method _ -> true 
    | State {value={name}} -> Atom.hint name = "intermediate_futures"
    | _ -> false


    let rec rewrite_wait_future parent_opt place = function
    | State {value=s; place} when Atom.hint s.name = "intermediate_futures" ->
        [ State {place; value={s with 
            type0 = mtype_of_ct (TDict (
                mtype_of_ft TUUID, 
                mtype_of_ct (TArrow(
                    mtype_of_ct (TResult(
                        mtype_of_ft TBottom,
                        mtype_of_var (Atom.builtin "error")
                    )),
                    mtype_of_ct (TResult(
                        mtype_of_ft TVoid,
                        mtype_of_var (Atom.builtin "error")
                    ))
                ))
            ))
        }} ]
    | Method m -> begin
        let parent =
            match parent_opt with
            | Some x -> x
            | None -> raise (Error.DeadbranchError "parent = None in citem rewriter")
        in 
        let rec aux acc_stmts = function
            (*
                ret = wait_future(f()?, timeout)  
                stmts

                becomes

                add2dict(this.futures, s_id, ret -> { stmts })

                where ret is now the continuation_id
            *)
            | [] -> [], AbstractImpl (List.rev acc_stmts)
            | {value=LetStmt(mt_x, let_x, ({value=CallExpr ({value=VarExpr x, _}, [continuation_id; _]), _}))}::stmts when Atom.is_builtin x && Atom.hint x = "wait_future" ->
                logger#debug "wait_future in %s" (Atom.to_string parent);

                let intermediate_futures = 
                    match Hashtbl.find_opt intermediate_futures_tbl  parent with
                    | Some x -> x
                    | None -> raise (Error.DeadbranchError (Printf.sprintf "wait_future in %s::%s, without intermediate_futures" (Atom.to_string parent) (Atom.to_string m.value.name)))
                in

                let intermediate_args = Commsimpl.compute_intermediate_args stmts (Some let_x) in

                (*** Since we introduce intermediate let (even for same variable) we need to attribute fresh identities ***)
                let to_rename = Atom.VMap.of_list (List.map (function (_,x) -> 
                    x, Atom.fresh (Atom.hint x)) intermediate_args) in
                let renaming x = match Atom.VMap.find_opt x to_rename with 
                    | Some y -> y  
                    | None -> x
                in
                (* rename remaining stmts*)
                let stmts = List.map (rename_stmt false renaming) stmts in 
                (* Remove garbage stmt (i.e. x; ) : no side effect and not use as a value *)
                let stmts = List.filter (function | {value=ExpressionStmt{value=VarExpr _,_}} -> false | _ -> true) stmts in

                logger#debug "split: \n\tlet_x: %s\n\t args:%s" (Atom.to_string let_x) (Atom.show_list "," (List.map snd intermediate_args));

                let tmp_let_x = Atom.fresh ((Atom.to_string let_x)^"_") in

                let renamed_intermediate_args = List.map (function (mt, x) -> (mt, renaming x )) intermediate_args in

                let continuation_name = Atom.fresh ("_"^(Atom.to_string parent)^"_future_continuation") in
                let continuation = auto_fplace {
                    annotations = [];
                    ghost = false;
                    ret_type = mtype_of_ct (TResult(
                        mtype_of_ft TVoid,
                        mtype_of_var (Atom.builtin "error")
                    )) ;
                    name = continuation_name;
                    args = List.rev (
                        auto_fplace(mtype_of_ct (TResult(
                            mtype_of_ft TBottom,
                            mtype_of_var (Atom.builtin "error")
                        )), tmp_let_x) 
                        :: List.map auto_fplace renamed_intermediate_args);
                    body = AbstractImpl 
                        (
                        let x = Atom.fresh "x" in
                        auto_fplace (LetStmt (
                                mt_x,
                                let_x,
                                e2_e (CallExpr(
                                    e2_e (AccessExpr (
                                        e2var tmp_let_x,
                                        e2_e (RawExpr "map")
                                    )),
                                    [
                                        e2_e (LambdaExpr (
                                            [ auto_fplace(mtype_of_ft TBottom, x) ],
                                            e2_e (CastExpr (
                                                (match mt_x.value with 
                                                | CType {value=TResult (mt1, _)} -> mt1
                                                | _ -> mt_x
                                                ),
                                                e2var x 
                                            ))
                                        ))
                                    ]
                                ))
                        ))  :: stmts
                         ); 
                    contract_opt = None;
                    on_destroy = false;
                    on_startup = false;
                } in

                (* Rec call - if there is other wait_future *)
                let continuations = rewrite_wait_future parent_opt place (Method continuation) in
                
                let footer_x = Atom.fresh "x" in
                let footer = stmt2_e(
                    CallExpr(
                        e2var (Atom.builtin "add2dict"),
                        [
                            e2_e (AccessExpr( e2_e This, e2var (intermediate_futures)));
                            continuation_id;
                            (* Closure only the result is free *)
                            e2_e(LambdaExpr (
                            [ auto_fplace(mtype_of_ct(TResult (mtype_of_ft TBottom, mtype_of_var (Atom.builtin "error"))), footer_x) ],
                                e2_e(CallExpr(
                                    e2_e(AccessExpr(e2_e This, e2var continuation_name)),
                                    (List.map (function (_,x) -> e2var x) intermediate_args)@ [ e2var footer_x ]
                                ))
                            ))
                        ]
                    )
                ) in

                continuations, AbstractImpl ((List.rev acc_stmts)@[footer])
            | stmt::stmts -> 
                aux (stmt::acc_stmts) stmts
        in

        let continuation_ms, body = 
            match m.value.body with
            | AbstractImpl stmts -> 
                let stmts = List.flatten (List.map (function stmt -> Commsimpl.to_X_form "wait_future" stmt.place stmt.value) stmts) in
                aux [] stmts
            | BBImpl _ -> [], m.value.body
        in
    
        (Method {m with value = {
            m.value with body = body
        }}) :: continuation_ms
    end 
    let rewrite_global_component parent_opt place (cstruct:component_structure) = 
        (* 
            onReceive(
                ResolvedResult.class,
                (id,ret) -> this.futures[id](ret)
            )
        *)
        let rec search_intermediate_futures = function
            | [] -> None 
            | {value={v=State {value={name}}}}::_ when Atom.hint name = "intermediate_futures" -> Some name 
            | _::items -> search_intermediate_futures items
        in

        begin match search_intermediate_futures cstruct.body with
        | Some name -> Hashtbl.add intermediate_futures_tbl cstruct.name name
        | None -> () 
        end;

        
        [ cstruct ]

    let select_ct_future = function
    | CType{value = TFuture _} -> true
    | _ -> false
    
    let rewrite_ct_future = function 
    | _ -> (mtype_of_ft TUUID).value


    let select_call_wait_future = function
    | CallExpr ({value=VarExpr x, _},  _)  -> Atom.is_builtin x && Atom.hint x = "wait_future" 
    | _ -> false 

    let select_lambda_wait_future = function
    | LambdaExpr(_, e) ->
        let _, elts, _ = 
            IRUtils.collect_expr_expr 
                ~exclude_expr:(function |LambdaExpr _-> true |_->false) (* search for minimal lambda containing wait_future, exclude intermediate nested lambdas *)
                None 
                Atom.Set.empty 
                select_call_wait_future
                (fun _ _ _ -> [true]) 
                e 
        in  
        elts <> []
    | _ -> false

    let rewrite_lambda_wait_future parent_opt place = function
    | LambdaExpr(params, e) -> 
        let _, elts, _ = IRUtils.collect_expr_expr 
                    None Atom.Set.empty select_call_wait_future (fun _ _ -> function  | {value=CallExpr (_, [continuation_id; _]), mt_res} -> [continuation_id, mt_res]) e in  

        let n_e = match elts with
            | [] -> LambdaExpr(params, e)
            | [continuation_id, mt_res] -> begin
                let parent =
                    match parent_opt with
                    | Some x -> x
                    | None -> raise (Error.DeadbranchError "parent = None in rewriter")
                in 

                let intermediate_futures = 
                    match Hashtbl.find_opt intermediate_futures_tbl  parent with
                    | Some x -> x
                    | None -> raise (Error.DeadbranchError (Printf.sprintf "wait_future in %s, without intermediate_futures" (Atom.to_string parent)))
                in

                let x = Atom.fresh "y" in
                let continuation_params = [auto_fplace(mt_res, x)] in
                let mt_ret_continuation = snd e.value in
                let continuation = auto_fplace(LambdaExpr(
                    continuation_params,
                    rewrite_expr_expr parent_opt select_call_wait_future (fun _ _ _ -> VarExpr x) e
                ), mtype_of_fun continuation_params mt_ret_continuation) in

                let footer_x = Atom.fresh "x" in
                let footer_params = [ auto_fplace(mtype_of_ct(TResult (mtype_of_ft TBottom, mtype_of_var (Atom.builtin "error"))), footer_x) ] in
                LambdaExpr(
                    params,
                    auto_fplace (CallExpr(
                        e2var (Atom.builtin "add2dict"),
                        [
                            e2_e (AccessExpr( e2_e This, e2var (intermediate_futures)));
                            continuation_id;
                            auto_fplace(LambdaExpr (
                            footer_params,
                                e2_e(CallExpr(
                                    continuation,
                                    [ e2var footer_x ]
                                ))
                            ), mtype_of_fun footer_params mt_ret_continuation)
                        ]
                    ), mtype_of_ft TVoid)

                )
                end
            in

            (* begin sanity check *)
            let _, elts, _ = 
                IRUtils.collect_expr_expr 
                    None 
                    Atom.Set.empty 
                    select_call_wait_future
                    (fun _ _ _ -> [true]) 
                    (e2_e n_e)
            in  
            assert(elts = []);

            n_e
        | _ ->  Error.error "%s" "Vardac does not support more than one wait_future in lambda"


    (*****************************************************)
    let name = pass_name 
    let displayed_pass_shortdescription = Printf.sprintf "Codegen: Eliminate future" 
    let displayed_ast_name = "pre-Akka AST without futures"
    let show_ast = true
    let global_at_most_once_apply = true 

    let precondition (program:IRI.program) = 
        program


    module StringSet = Set.Make(String) 
    let postcondition program = 
        let  fcts_set = StringSet.of_seq (List.to_seq   ["complete_future"; "future"; "wait_future"]) in
        (* Ensure not more future primitives *)
        collect_expr_program Atom.Set.empty 
            (function |VarExpr x -> 
                Atom.is_builtin x && StringSet.mem (Atom.hint x) fcts_set | _ -> false)
            (fun _ _ e -> raise (Error.PlacedDeadbranchError(e.place, "future's primitives remains after FutureElim")))
            program;
        program

    let apply_program program =
        program
        |> rewrite_component_program (function _ -> true) rewrite_global_component (* hydrate shared state *)
        |> rewrite_stmt_program false select_new_future rewrite_new_future (* NB can not use recurse:true with the ref selected trick in side select_new_future *) 
        |> rewrite_citem_program (function _ -> true) warp_complete_future_method
        |> rewrite_expr_program select_lambda_wait_future rewrite_lambda_wait_future
        |> rewrite_citem_program select_wait_future rewrite_wait_future 
        |> rewrite_type_program select_ct_future rewrite_ct_future
end