open Utils
open Core
open IR
open Easy_logging
open Utils
open AstUtils
open IRMisc
 
let receive_selector = function 
    | (CallExpr ({value=(VarExpr x, _)}, [s])as e) when Atom.is_builtin x && Atom.hint x = "receive" -> true
    | _ -> false

let receive_collector msg parent_opt env e = 
    let parent = match parent_opt with | None -> "Toplevel" | Some p -> Atom.to_string p in
    Error.perror e.place "%s. Parent = %s" msg parent

let show_receive_entries = [%derive.show: (Atom.atom * main_type * session_type * expr * expr) list]

(*
    input [x_1; ... ; x_n]
    return [x_1; ..; x_n-1], x_n
*)
(* TODO move it out *)
(*let split_last : 'a list -> ('a list) * 'a = 
    let rec _split_last acc = function
        | [] -> failwith "splitlast"
        | x::[] -> List.rev acc,  x
        | x::xs -> _split_last (x::acc) xs
    in
    _split_last []
*)
(***************************************************)

module type Sig = sig
    include IRCompilationPass.Pass
end

module Make () : Sig = struct
    let logger = make_log_of "Commsimpl.RecvElimination"
    let pass_name = "Commsimpl.RecvElimination"
    let fplace = (Error.forge_place "RecvElimination" 0 0) 
    let auto_fplace smth = {place = fplace; value=smth}
    include AstUtils2.Mtype.Make(struct let fplace = fplace end)

    (***************************************************)
    (*
        Architecture remains unchanged - rewriting architecture is done in an other module (to be written)
        - get ride of session.receive only use ports
            TODO FIXME only scan component method at this point
    *)


    (************** Utils *****************)
    let fresh_next_method main_name main_annotations t_msg_cont =
        {
            annotations =
                List.filter_map 
                    (function
                        | Expose -> None 
                        | MsgInterceptor _ -> None
                        | SessionInterceptor _ -> None
                        | Onboard _ -> None
                    ) 
                    main_annotations;
            ghost = false;
            (* result<void, error> to support error propagation *)
            ret_type = mtype_of_ct (TResult(mtype_of_ft TVoid, Builtin.builtin_mt_error)); 
            name = Atom.fresh ((Atom.hint main_name)^"_intermediate");
            args = (
                match t_msg_cont with
                | None -> []
                | Some (t_msg, st_continuation) ->
                    [
                        auto_fplace (t_msg, Atom.fresh "e");
                        auto_fplace (mtype_of_st st_continuation.value, Atom.fresh "session");
                    ]
            ); 
            body = [];
            contract_opt = None;
            on_destroy = false;
            on_startup = false;
        }

    (*******************************************************)
    

    type method_info = ((expr_variable * main_type * main_type) option * expr option * _method0)




    (* 
        @param s1_opt - receive(s1, ..) in m1, s1_opt = Some(s1) if m1 is the main method 
        @param intermediate_args - args that should propagated from 1 to 2 
        @param (res, t_msg, st_continuation) - tuple<T_msg, st_continuation> res = receive(...); that separate both methods
    *)
    let rewrite_methodint a_registered_sessions (m1 : _method0) (m2 : _method0) s1_opt e_current_intermediate_port intermediate_args (res, t_msg, st_continuation) : state list * _method0 * _method0 =
        let intermediate_args = List.map auto_fplace intermediate_args in
        let ctype_intermediate_args = mtype_of_ct (TTuple (List.map (function arg -> fst arg.value) intermediate_args)) in
        let tuple_intermediate_args = e2_e (BlockExpr (
            Tuple, 
            List.map (function arg -> e2var (snd arg.value)) intermediate_args
        )) in

        (*** Header method 2 ***)
        (* let res = receive ()
            build res variable in 2th method
        *)
        let param_event, param_session = 
            match m2.args with
                | [ {value=_,param_event}; {value=_,param_session} ] -> param_event, param_session
        in 

        let load_recv_result : stmt list = 
            (* Tuple<t_msg, st_continuation> res = tuple(tmp_event, tmp_session) *)
            [
                auto_fplace(LetStmt(
                    mtype_of_ct (TTuple[ t_msg; mtype_of_st st_continuation.value]),
                    res,
                    e2_e (BlockExpr(
                        Tuple,
                        [
                            e2var param_event;
                            e2var param_session
                        ]
                    ))
                ))
            ]
        in 

        let register s = 
            (* this.registered_sessions[s.id] = this.intermediate_port; *)
            stmt2_e(
                CallExpr(
                    e2var (Atom.builtin "add2dict"),
                    [
                        e2_e (AccessExpr( e2_e This, e2var (a_registered_sessions)));
                        e2_e (CallExpr( e2var (Atom.builtin "sessionid"), [s]));
                        e_current_intermediate_port
                    ]
                )
            )
        in

        let unregister s = 
            (* del this.registered_sessions[s.id]; *)
            stmt2_e(
                CallExpr(
                    e2var (Atom.builtin "remove2dict"),
                    [
                        e2_e (AccessExpr( e2_e This, e2var (a_registered_sessions)));
                        e2_e (CallExpr( e2var (Atom.builtin "sessionid"), [s]))
                    ]
                )
            )
        in

        match intermediate_args with
        | [] -> (* Only load recv in the [res] variable *)
            let m2 = { m2 with 
                (* TODO add cleansing when timeout *)
                body = load_recv_result @ m2.body} in

            (*** Add footer m1 - Registering the session in order to be able to do dynamic routing ***)
            let m1 = { m1 with body = m1.body @ [ 
                register (match s1_opt with
                    | Some session -> session (* when we are in the first method of the list*)
                    | None -> e2var param_session (* for all the intermediate (and last) methods *)
                )
            ]} in

            (*** Add header m2 - to unregister the session ***)
            let m2 = {m2 with body = m2.body @ [ unregister (e2var param_session)]} in

            (*** Returns ***)
            [], m1, m2
        | _ -> (* Load args from state + load recv in the [res] variable *)

            (*** Create state to store intermediate variables between m1 and m2 ***)
            let intermediate_state_name = Atom.fresh ((Atom.to_string m1.name)^"__"^(Atom.to_string m2.name)^"_intermediate_state") in

            let intermediate_state_type = 
                mtype_of_ct (TDict(
                    mtype_of_ft TSessionID,
                    ctype_intermediate_args
                )) 
            in

            let intermediate_state = auto_fplace ({
                ghost = false;
                type0 = intermediate_state_type;
                name = intermediate_state_name;
                body =  Some (e2_e (
                    Block2Expr(Dict, [])
                ))
            }) in

            (*** Add footer to m1 to store intermediate in state before returning ***)
            let m1 = { m1 with 
                body = m1.body @ [
                    (* TODO add cleansing when timeout *)
                    auto_fplace (ExpressionStmt ( e2_e (CallExpr(
                        e2var (Atom.builtin "add2dict"),
                        [
                            e2_e (AccessExpr(
                                e2_e This,
                                e2var intermediate_state_name
                            ));
                            e2_e (CallExpr(
                                e2var (Atom.builtin "sessionid"),
                                [ match s1_opt with
                                    | Some session -> session (* when we are in the first method of the list*)
                                    | None -> e2var param_session (* for all the intermediate (and last) methods *) 
                                ]
                            ));
                            tuple_intermediate_args;
                        ]
                    ))))
                ]
            } in

            (*** Add header to m2 to load the intermediate in state and store the result of receive in [res] ***)
            let local_tmp_args = Atom.fresh "local_tmp_args" in
            let m2 = { m2 with 
                (* TODO add cleansing when timeout *)
                body = 
                load_recv_result @
                [
                    auto_fplace (LetStmt (ctype_intermediate_args, local_tmp_args, (e2_e(CallExpr(
                        e2var (Atom.builtin "remove2dict"),
                        [
                            e2_e(AccessExpr(
                                e2_e This,
                                e2var intermediate_state_name
                            ));
                            e2_e (CallExpr(
                                e2var (Atom.builtin "sessionid"),
                                [ e2var param_session ]
                            ));
                        ]
                    )))));
                ] @ (
                    List.mapi (fun i {value=(mt, x)} ->
                        auto_fplace (LetStmt (mt, x, 
                            e2_e( AccessExpr(
                                e2var local_tmp_args,
                                e2var (Atom.builtin (Printf.sprintf "_%d" i))
                            ))
                        )) 
                    ) intermediate_args
                )
                @ m2.body; 
            } in


            (*** Add footer m1 - Registering the session in order to be able to do dynamic routing ***)
            let m1 = { m1 with body = m1.body @ [ 
                register (match s1_opt with
                    | Some session -> session (* when we are in the first method of the list*)
                    | None -> e2var param_session (* for all the intermediate (and last) methods *)
                )
                ]} in

            (*** Add header m2 - to unregister the session ***)
            let m2 = {m2 with body = m2.body @ [ unregister (e2var param_session)]} in

            (*** Returns ***)
            [intermediate_state], m1, m2


    let rec split_body a_registered_sessions a_intermediate_futures  (main_name, main_annotations) acc_stmts (next_method:_method0) : stmt list -> state list * (Atom.atom * main_type * session_type * expr * expr) list * _method0 list =
        let fplace = (Error.forge_place "Core.Rewrite.split_body" 0 0) in
        let auto_fplace smth = {place = fplace; value=smth} in

        function
        | [] -> 
            logger#debug "end split_body";
            let current_method = { next_method with 
                body = next_method.body @ (List.rev acc_stmts);
            } in

            [], [], [ current_method ]
        | {place; value=LetStmt ({value=CType{value=TTuple [t_msg;{value = SType st_continuation}]}}, let_x, {place=place1; value=(CallExpr ({value=(VarExpr x, _)}, [s]),_)})}::stmts  when Atom.is_builtin x && Atom.hint x = "receive" -> 
            logger#debug "receive %s at %s" (Atom.to_string let_x) (Error.show place1);
            (*** Prepare ***)
            let stage_stmts = next_method.body @ (List.rev acc_stmts) in
            let current_method = { next_method with 
            body = stage_stmts; } in


            (*** Shifting***)
            (*
                creating the new_next_method and the current next_method becomes the current method
            *)
            let next_method = fresh_next_method main_name main_annotations (Some (t_msg, st_continuation)) in

            (*** Gathering intells ***)
            let intermediate_args = compute_intermediate_args stmts (Some let_x) in

            logger#debug "split: \n\tlet_x: %s\n\t args:%s" (Atom.to_string let_x) (Atom.show_list "," (List.map snd intermediate_args));

            let receive_id = Atom.fresh ((Atom.to_string current_method.name)^"receive_id") in
            let e_current_intermediate_port = e2_e(
                CallExpr(
                    e2_e(AccessExpr(
                        e2_e This,
                        e2var (Atom.builtin "__get_intermediate_port")
                    )),
                    [
                        s;
                        e2_e( OptionExpr (Some (e2_lit (StringLit (Atom.to_string receive_id)))));
                    ]
            )) in
            
            (*** Creating link between current_method and next_method, before shifting ***)
            let intermediate_states, current_method, next_method = rewrite_methodint a_registered_sessions current_method next_method (Some s) e_current_intermediate_port intermediate_args (let_x, t_msg, st_continuation) in

            (*** Creation of the intermediate ports ***)
            let receive_entries = [(receive_id, t_msg, st_continuation, e2var next_method.name, s)] in


            (*** Since we introduce intermediate let (even for same variable) we need to attribute fresh identities ***)
            let to_rename = Atom.VMap.of_list (List.map (function (_,x) -> 
                x, Atom.fresh (Atom.hint x)) intermediate_args) in
            let renaming x = match Atom.VMap.find_opt x to_rename with 
                | Some y -> y  
                | None -> x
            in
            (* rename remaining stmts*)
            let stmts = List.map (rename_stmt false renaming) stmts in 
            (* renaming let i = nth(res, ...)*)
            let next_method = {next_method with body = List.map (rename_stmt false renaming) next_method.body } in

            (*** Returns and rec call***)
            (*  NB. Initial param_current_method is unused 
                See whitepaper for details
            *)
            let intermediate_methods = [current_method] in 


            let intermediate_states2, receive_entries2, intermediate_methods2 = split_body a_registered_sessions a_intermediate_futures  (main_name, main_annotations) [] next_method stmts in           

            intermediate_states@intermediate_states2,
            receive_entries@receive_entries2,
            intermediate_methods@intermediate_methods2
        
        (* If case - conditional branching is painfull *)
        | ({place; value = IfStmt (e, stmt1, stmt2_opt)} as stmt) :: stmts -> 
            logger#debug "recv-elim IfStmt";
            (* DEBUG Code split_body (main_name, main_annotations) acc_stmts next_method (stmt1::stmts)*)
            let _,flag1,_ = collect_expr_stmt None Atom.Set.empty receive_selector (fun _ _ _ -> [true]) stmt1 in
            let flag1 = flag1 <> [] in

            let flag2 = Option.map (collect_expr_stmt None Atom.Set.empty receive_selector (fun _ _ _ -> [true])) stmt2_opt in
            let flag2 = Option.map (function (_, elts, _) -> elts) flag2 in
            let flag2 = match flag2 with | None -> false | Some flag2 -> flag2 <> [] in


            (*** Returns and rec call***)
            if flag1 || flag2 then
            begin
                logger#debug "Detect receive in If block in %s" (Atom.to_string main_name);

                (* There is at least one receive inside stmt1 or stmt2_opt *)

                (*  next_method is in charge of the whole IfStmt not a branch, 
                    therefore we create two specialised next_method 
                *)
                let t_msg_cont = 
                    match next_method.args with 
                    | [{value=t_msg,_}; {value={value=SType st_continuation},_}] -> Some (t_msg, st_continuation)
                    | _ -> (* IfStmt first stmt of method body *)
                        None
                in

                (*** Prepare ***)
                let stage_stmts = next_method.body @ (List.rev acc_stmts) in
                let current_method = { next_method with 
                body = stage_stmts; } in

                (*** Shifting***)
                (*
                    creating the new_next_method and the current next_method becomes the current method
                *)
                let next_method = fresh_next_method main_name main_annotations t_msg_cont in
                
                let split_branch stmt_branch = 
                    logger#debug "split_branch of \n";

                    (*** 
                        stmts should be add as the continuation of branch

                        can not deduplicate stmts because in each branch it can be bounded differently  
                        one with msg/session + loading from intermediate state and the other a direct call

                        choice 1: duplicate code - easiest (done here)
                        choice 2: one method with freevars as args + two wrapper one per branch
                    ***)
                    
                    (* Duplication implies renaming to preserve binder unicity *)
                    (* TODO create generic duplicate fct *)
                    let duplicate_stmts stmts = 
                        let _, fvars = List.split (List.map (free_vars_stmt Atom.Set.empty) stmts) in
                        let fvars = List.flatten fvars in
                        let _, ftvars = List.split (List.map (free_tvars_stmt ~flag_tcvar:true Atom.Set.empty) stmts) in
                        let ftvars = List.flatten ftvars in
                        let not_to_rename = Atom.Set.of_list ((List.map snd fvars)@ftvars) in
                        let renaming =
                            let state = Hashtbl.create 16 in
                            function x -> 
                                (* Refresh ids only of inner binders i.e. avoid refreshing ids of freevars *)
                                (* N.B. in stmt we do not need to refresh components nor types (not binder for them) *)
                                match Atom.Set.find_opt x not_to_rename with 
                                | Some _ -> x  
                                | None -> begin 
                                    logger#debug "renaming x=%s" (Atom.to_string x);
                                    match Hashtbl.find_opt state x with 
                                    | Some y -> y 
                                    | None -> begin
                                        let y = Atom.fresh (Atom.hint x) in 
                                        Hashtbl.add state x y;
                                        y
                                    end
                                end
                        in
                        List.map (rename_stmt false renaming) stmts
                    in

                    (* Warning. stmt_branch is in a nested scope 
                        but since cook introduce unique name we do not need to preserve it as a separate BlockStmt
                    *)
                    let branch_stmts = stmt_branch :: (duplicate_stmts stmts) in

                    let next_method_branch = fresh_next_method main_name main_annotations t_msg_cont in
                    let intermediate_states_branch, receive_entries_branch, intermediate_methods_branch =
                        let intermediate_states_branch, receive_entries_branch, intermediate_methods_branch = 
                            split_body a_registered_sessions a_intermediate_futures  (main_name, main_annotations) [] next_method_branch branch_stmts in
                        intermediate_states_branch, receive_entries_branch, intermediate_methods_branch
                    in

                    (*** Built stmt added to stage_stmts ***)
                    (* 
                        m_branch_0.body from the begining of the if branch to the first receiv 
                        therefore we inline it as the content of the branch
                    *)
                    let m_branch_0::intermediate_methods_branch = intermediate_methods_branch in

                    auto_fplace (BlockStmt m_branch_0.body), intermediate_states_branch, receive_entries_branch, intermediate_methods_branch
                in

                let stmt1, intermediate_states1, receive_entries1, intermediate_methods1 = 
                    if flag1 then (
                        logger#debug "Detect receive in If block1\n"; 
                        split_branch stmt1
                    ) else stmt1, [], [], []
                in

                let stmt2_opt, intermediate_states2, receive_entries2, intermediate_methods2 = 
                    if flag2 then begin
                        logger#debug "Detect receive in If block2";
                        match stmt2_opt with
                        | None -> None, [], [], []
                        | Some stmt2 -> (match split_branch stmt2 with | stmt2, a, b, c -> Some stmt2, a, b, c)
                    end
                    else stmt2_opt, [], [], []
                in

                let stmt = auto_fplace (IfStmt(e, stmt1, stmt2_opt)) in

                (* Sanity check *)
                (*collect_expr_stmt None Atom.Set.empty receive_selector (fun _ _ _ -> assert false) stmt;*)

                
                (*** Finish parent split execution branch since there is conditional branching *)
                let current_method = { current_method with 
                    body = current_method.body @ [stmt];
                } in

                intermediate_states1@intermediate_states2,
                receive_entries1@receive_entries2,
                current_method::intermediate_methods1@intermediate_methods2
            end
            else
                (* If has no receive inside *)
                split_body a_registered_sessions a_intermediate_futures  (main_name, main_annotations) (stmt::acc_stmts) next_method stmts
        | ({value = BlockStmt stmts1} as stmt)::stmts2 ->
            let flag = List.map (collect_expr_stmt None Atom.Set.empty receive_selector (fun _ _ _ -> [true])) stmts1 in
            let flag = List.flatten (List.map (function (_, elts, _) -> elts) flag) in
            let flag = flag <> [] in



            if flag then
            begin (* receive in stmts1 *)
                logger#debug "receive in block stmt";
                
                (* 
                    continuation of stmts1 is stmts
                    cook alieviate us from taking care of syntaxic scoping since it introduce unique variable name for each binder

                    therefore we reason as if there is not block information
                    TODO do we need to keep blockstmt or inline it in other stmts constructions since nested block are handled by cook -> maybe yes for readability of generated codegen ?????
                    grouping both stmt list avoid us to painfully interconnect their splits
                *)
                let full_stmts = stmts1@stmts2 in
                split_body a_registered_sessions a_intermediate_futures  (main_name, main_annotations) acc_stmts next_method full_stmts
            end
            else split_body a_registered_sessions a_intermediate_futures  (main_name, main_annotations) (stmt::acc_stmts) next_method stmts2

            
        (* TODO for others stmt that can host nested let*)
        | stmt::stmts -> split_body a_registered_sessions a_intermediate_futures  (main_name, main_annotations) (stmt::acc_stmts) next_method stmts



    let methods_with_continuations = Hashtbl.create 16

    let rec rewrite_method0 a_registered_sessions a_intermediate_futures a_intermediate_prebinders place (m:_method0) = 
        let fplace = (Error.forge_place "Core.Rewrite.rewrite_method0" 0 0) in
        let auto_fplace smth = {place = fplace; value=smth} in

        let stmts = List.flatten (List.map (function stmt -> to_X_form "receive" stmt.place stmt.value) m.body) in

        let intermediate_states, receive_entries, intermediate_methods = split_body a_registered_sessions a_intermediate_futures (m.name, m.annotations) [] {m with body = []} stmts in
        let intermediate_methods = List.map auto_fplace intermediate_methods in

        let intermediate_methods = match m.ret_type.value, intermediate_methods with
        | _, [] -> raise (Error.DeadbranchError "Can not be empty")
        | _, [_] -> 
            (* No receive in m *)
            intermediate_methods
        | CType{value=TFlatType TVoid}, ms |   CType{value=TResult ({value=CType{value=TFlatType TVoid}}, _)}, ms  -> 
            (* No values to propagate *)
            ms  
        | _, top_method::methods ->
            (* Will be set to true if there is returns inside intermediate methods*)
            let has_delayed_returns = ref false in


            (* Find return stmts in methods and rewrite then by
                this.intermediate_futures[s_id].complete(return_value);   
            *)
            let return_selector = function | ReturnStmt _ -> true | _ -> false in
            let return_rewritor param_s parent_opt place = function
                | ReturnStmt e -> 
                    has_delayed_returns := true;
                    [
                        ExpressionStmt(
                            e2_e(CallExpr(
                                e2var (Atom.builtin "complete_future"),
                                [
                                    (* The future *)
                                    e2_e(CallExpr(
                                        e2var (Atom.builtin "get2dict"),
                                        [
                                            e2_e(AccessExpr(
                                                e2_e This,
                                                e2var a_intermediate_futures
                                            ));
                                            e2_e (CallExpr( e2var (Atom.builtin "sessionid"), [e2var param_s]));
                                        ]
                                    ));
                                    (* The value*)
                                    e
                                ]
                            ))
                        )
                    ]
            in

            let methods = List.map (function (m:method0) -> 
                let param_s = match m.value.args with
                    | [_;{value=_,param_s}] -> param_s
                    | _ -> raise (Error.DeadbranchError "wrong intermediate method signature")
                in
                { m with value = {
                    m.value with body = List.flatten (
                        List.map (rewrite_stmt_stmt true None return_selector (return_rewritor param_s)) m.value.body) }}     
            ) methods in

            (* Add at the end of top_method
                Future<m.ret_type> f = new CompletableFuture();
                this.intermediate_futures[s_id] = f;
                return f;
            *)
            let top_method = if !has_delayed_returns then (

                let mt_ok, mt_err = match m.ret_type.value with
                    | CType {value=TResult(mt_ok, mt_err)} -> mt_ok, mt_err
                    | _ -> raise (Error.PlacedDeadbranchError (place, "wrong return type"))
                in

                let generate_footer s = 
                    let f = Atom.fresh "ret_future" in

                    [
                        (* create the future *)
                        auto_fplace(LetStmt(
                            mtype_of_ct (TFuture m.ret_type),
                            f,
                            e2_e (CallExpr( e2var (Atom.builtin "future"), []))
                        ));
                        (* register the future *)
                        auto_fplace(ExpressionStmt (
                            e2_e(CallExpr(
                                e2var (Atom.builtin "add2dict"),
                                [
                                    e2_e(AccessExpr(
                                        e2_e This,
                                        e2var a_intermediate_futures
                                    ));
                                    e2_e (CallExpr( e2var (Atom.builtin "sessionid"), [e2var s]));
                                    e2var f
                                ]
                            ))
                        ));
                        (* return the future *)
                        auto_fplace(ReturnStmt (e2_e(ResultExpr(
                            Some (e2var f), None))));
                    
                    ] 
                in
                
                let split_last =
                    let rec aux acc = function
                    | [] -> failwith "empty list"
                    | [y] -> List.rev acc, y
                    | y::ys -> aux (y::acc) ys
                    in
                    aux []
                in

                let rec add_footer_to stmts = 
                    let core_stmts, last_stmt = split_last stmts in
                    core_stmts @ (
                        match last_stmt.value with
                        | ExpressionStmt {value=CallExpr (_, [_;{value=CallExpr (_, [{value=VarExpr s, _}]), _};_]), _} as stmt -> 
                            last_stmt :: (generate_footer s)
                        | IfStmt (e, stmt1, None) -> 
                            [ auto_fplace (IfStmt(e, auto_fplace (BlockStmt (add_footer_to [stmt1])), None)) ]
                        | IfStmt (e, stmt1, Some stmt2) ->
                            let stmt1 = auto_fplace (BlockStmt (add_footer_to [stmt1])) in
                            let stmt2 = auto_fplace (BlockStmt (add_footer_to [stmt2])) in
                            
                            [ auto_fplace (IfStmt(e, stmt1, Some stmt2)) ]
                        | BlockStmt stmts -> [ auto_fplace (BlockStmt (add_footer_to stmts)) ]
                        | e -> 
                            raise (Error.DeadbranchError "see rewrite_methodint")
                    )
                in

                { top_method with value = { top_method.value with 
                    ret_type = mtype_of_ct (TResult(
                        mtype_of_ct (TFuture m.ret_type),
                        mt_err
                    ));
                    body = add_footer_to top_method.value.body 
                }}
            ) else top_method in

            (* Register m.name in order to rewrite all m call such that
                m_call(....) becomes

                Future<m.ret_type> f = m_call(...);
                f().get(timeout_custom, TimeUnit.MILLISECONDS);
            *)
            Hashtbl.add methods_with_continuations m.name m.ret_type;


            (* Handles contract
                Ensures should bind with top_method
                Returns should be bind with last_method
                Pre_binders should be shared using some => TODO
            *)

            match top_method.value.contract_opt with
            | None -> top_method::methods
            | Some contract -> begin

                let last_method::core_methods = List.rev methods in

                let top_method, last_pre_binders, last_pre_binders_renaming = 
                    if (contract.value.pre_binders <> [] && contract.value.returns <> None) then (
                        (* Store pre_binders
                        At the end of the top_method -> after session creation
                        *)
                        let s_top = match receive_entries with
                            | [(_,_,_,_, s)] -> s
                            | _ -> raise (Error.DeadbranchError "at least one receive entry")
                        in
                        let top_store_prebinders = e2_e(CallExpr(
                            e2var (Atom.builtin "add2dict"),
                            [ 
                                e2_e(AccessExpr(
                                    e2_e This,
                                    e2var a_intermediate_prebinders
                                ));
                                e2_e (CallExpr( e2var (Atom.builtin "sessionid"), [s_top]));
                                e2_e (BlockExpr(
                                    List, 
                                    List.map (function (_,_, e) -> 
                                        e2_e (CastExpr(
                                            mtype_of_ft TBottom,
                                            e
                                        ))    
                                    ) contract.value.pre_binders
                                ))
                            ]
                        )) in
                        let top_method = {top_method with 
                            value = { top_method.value with
                                body = insert_in_stmts [stmt2e top_store_prebinders] top_method.value.body
                        }} in

                        let s_last = match last_method.value.args with
                            | [_; {value=(_,s_last)}] -> s_last
                            | _ -> raise (Error.DeadbranchError "wrong args for last_method")
                        in

                        (* Load pre_binders inside last_method contract 
                            and garbage collect at the same time

                            N.B rename binders since their ids are already used inside the top_method contract
                        *)
                        let renaming = 
                            let to_rename = Atom.Set.of_list (List.map (function (_,x,_) -> x) contract.value.pre_binders) in
                            let state = Hashtbl.create 16 in
                        function x -> 
                            match Atom.Set.find_opt x to_rename with
                            | None -> x
                            | Some _ -> begin
                                match Hashtbl.find_opt state x with
                                | Some y -> y
                                | None -> 
                                    let y = Atom.fresh (Atom.hint x) in
                                    Hashtbl.add state x  y;
                                    y
                            end
                        in

                        let last_pre_binders  = 
                            List.mapi (fun i (mt, x, _) ->
                                let e = 
                                    e2_e(CastExpr(
                                        mt,
                                        e2_e(CallExpr(
                                            e2var (Atom.builtin "listget"),
                                            [ 
                                                e2_e(CallExpr(
                                                    e2var (Atom.builtin "remove2dict"),
                                                    [ 
                                                        e2_e(AccessExpr(
                                                            e2_e This,
                                                            e2var a_intermediate_prebinders
                                                        ));
                                                        e2_e (CallExpr( e2var (Atom.builtin "sessionid"), [e2var s_last]));
                                                    ]
                                                ));
                                                e2_lit (IntLit i)
                                            ]
                                        ))
                                    ))
                                in
                                (mt, renaming x, e)
                            ) contract.value.pre_binders in
                        top_method, last_pre_binders, renaming 
                    ) else
                        top_method, [], Fun.id
                in

                let top_contract = match contract.value.ensures with
                | None -> None 
                | Some predicate -> 
                    Some {
                        place = fplace@contract.place;
                        value = {
                            method_name = top_method.value.name;
                            pre_binders = contract.value.pre_binders;
                            ensures = Some predicate; 
                            returns = None;
                        }
                    }
                in
                let last_contract = match contract.value.returns with
                | None -> None
                | Some predicate -> 
                    Some {
                        place = fplace@contract.place;
                        value = {
                            method_name = last_method.value.name;
                            pre_binders = last_pre_binders;
                            ensures = None; 
                            returns = Some (rename_expr true last_pre_binders_renaming predicate);
                        }
                    }
                in

                let top_method = {top_method with value = {top_method.value with contract_opt = top_contract}} in
                let last_method = {last_method with value = {last_method.value with contract_opt = last_contract}} in

                top_method :: (List.rev (last_method::core_methods))
            end
        in

        receive_entries, intermediate_states, intermediate_methods
    and rmethod0 a_registered_sessions a_intermediate_futures a_intermediate_prebinders = map0_place (rewrite_method0 a_registered_sessions a_intermediate_futures a_intermediate_prebinders)

    (* return name of intermediate states * citems *)
    and rewrite_component_item a_registered_sessions a_intermediate_futures a_intermediate_prebinders place = 
    let fplace = (Error.forge_place "Core.Rewrite" 0 0) in
    let auto_place smth = {place = place; value=smth} in
    let auto_fplace smth = {place = fplace; value=smth} in
    function
    | State _ as citem -> 
        ([], []), [auto_place (auto_plgannot citem)]
    | Contract _ as citem -> 
        ([], []), [auto_place (auto_plgannot citem)] 
    | Method m as citem -> 
        let receive_entries, intermediate_states, intermediate_methods = rmethod0 a_registered_sessions a_intermediate_futures a_intermediate_prebinders m in

        (
            List.map (function (s : state) -> s.value.name) intermediate_states,
            receive_entries
        ), (List.map (function s-> auto_fplace (auto_plgannot(State s))) intermediate_states)
            @ (List.map (function m-> auto_fplace (auto_plgannot(Method m))) intermediate_methods)
    | (Inport _ as citem) | (Eport _ as citem) | (Outport _ as citem) -> 
        ([], []), [auto_place (auto_plgannot citem)]
    | Term t -> 
        ([], []), [auto_place (auto_plgannot(Term (rterm t)))]
    | Include _ as citem -> 
        ([], []), [auto_place (auto_plgannot citem)]
    and rcitem a_registered_sessions a_intermediate_futures a_intermediate_prebinders = map0_place (transparent0_plgannot(rewrite_component_item a_registered_sessions a_intermediate_futures a_intermediate_prebinders))

    and rewrite_component_dcl place : _component_dcl -> _component_dcl = 
    let fplace = (Error.forge_place "Core.Rewrite" 0 0) in
    let auto_place smth = {place = place; value=smth} in
    let auto_fplace smth = {place = fplace; value=smth} in
    function
    | ComponentAssign _ as cdcl -> cdcl
    | ComponentStructure cdcl -> 
        (* Define component names *)
        let a_registered_sessions = 
            match 
                List.find_opt (function | {value={v=State {value={name}}}} -> Atom.hint name = "registered_session" | _-> false) cdcl.body 
            with
            | Some {value={v=State {value={name}}}} -> name (* exactly once per component *)
            | None -> Atom.fresh "registered_session" 
        in
        let a_intermediate_futures = 
            match 
                List.find_opt (function | {value={v=State {value={name}}}} -> Atom.hint name = "intermediate_futures" | _-> false) cdcl.body 
            with
            | Some {value={v=State {value={name}}}} -> name (* exactly once per component *)
            | None -> Atom.fresh "intermediate_futures" 
        in
        let a_intermediate_prebinders = 
            match 
                List.find_opt (function | {value={v=State {value={name}}}} -> Atom.hint name = "intermediate_prebinders" | _-> false) cdcl.body 
            with
            | Some {value={v=State {value={name}}}} -> name (* exactly once per component *)
            | None -> Atom.fresh "intermediate_prebinders" 
        in



        let body = cdcl.body in

        (* Elimination of sync receiv *)
        let tmp, body = List.split(List.map (rcitem a_registered_sessions a_intermediate_futures a_intermediate_prebinders) body) in
        let intermediate_state_names, receive_entries = List.split tmp in
        let intermediate_state_names = List.flatten(intermediate_state_names) in
        let body = List.flatten body in
        let receive_entries  = List.flatten receive_entries in

        logger#debug "receive_entries -\n %s" (show_receive_entries receive_entries);
               
        (*  Creation of intermediate ports 
            For each port and receive (t_msg,st_msg, intermediate_callback) generates an intermediate ports 
                if ?t_msg st_msg is included in port.expecting_st   
                such that intermediate_ports \in port.children 

            Warning: needed since ports can be binded dynamically.
        *)
        let ports = List.filter_map (function {value={v=Inport p }} -> Some p | _ -> None) body in
        let body_wo_ports = List.filter (function {value={v=Inport _ }} -> false | _ -> true) body in
        let outports = List.filter_map (function {value={v=Outport p}} -> Some p | _ -> None) body in
        let body_wo_ports_outports = List.filter (function {value={v=Outport _} } -> false | _ -> true) body_wo_ports in

        (* main_port_name -> list of newly created children *)
        let children = Hashtbl.create 32 in

        (*  Take a receive in input and returns the list of ports for handling this receive 
            @param ports - list of input ports of the component
            @return - list of intermediate_ports for this receive + ports with updated children
        *)
        let generate_intermediate_ports ports (receive_id, t_msg, st_continuation, callback, _) = 
            logger#debug "generate_intermediate_port for %s <%s> and ports %d and outports %d " (Atom.to_string cdcl.name) (Atom.to_string receive_id) (List.length ports) (List.length outports);
            (* Remove ports that are already binded to receive *)
            let ports = List.filter (function p -> Bool.not (fst p.value)._is_intermediate) ports in

            let generate_intermediate_port_for_port portname =
                let intermediate_port_name = Atom.fresh ((Atom.to_string receive_id)^"_intermediate_port") in
                logger#debug "\t - generate %s" (Atom.to_string intermediate_port_name);

                (* register to children *)
                Hashtbl.add children portname (intermediate_port_name::
                    match Hashtbl.find_opt children portname with
                    | None -> []
                    | Some l -> l
                );

                (*** Creation of the intermediate port ***)
                [ auto_fplace ({
                        name = intermediate_port_name;
                        callback = e2_e (AccessExpr (
                            e2_e This, 
                            callback
                        ));
                        expecting_st = mtype_of_st (STRecv (t_msg, st_continuation));
                        _disable_session = false;
                        _children = [];
                        _is_intermediate = true;
                        _receive_id = Some receive_id;
                    }, auto_fplace EmptyMainType)
                ]
            in

            let generated_ports =
                (* When session is initiated by an inport *)
                List.flatten (List.map (function port ->
                    if Common.TypingUtils.is_suffix_st  (auto_fplace (STRecv(t_msg, st_continuation))) (match (fst port.value).expecting_st.value with SType st -> st) then 
                        (* p or one of its stages is concerned by the receive*)
                        generate_intermediate_port_for_port (fst port.value).name
                    else( 
                        []
                    )
                ) ports)
                @
                ( (* When session is initiated by an outport *)
                    List.flatten (List.map (function (port:outport) ->
                        if Common.TypingUtils.is_suffix_st (auto_fplace (STRecv(t_msg, st_continuation))) (match (fst port.value).protocol.value with 
                            | SType st -> 
                                st 
                            | mt -> raise (Error.PlacedDeadbranchError (port.place, Printf.sprintf "Wrong outport type %s" (show__main_type mt)))
                        ) then 
                            (* p or one of its stages is concerned by the receive*)
                            generate_intermediate_port_for_port (fst port.value).name
                        else( 
                            []
                        )
                    ) outports)
                )
            in

            if generated_ports = [] then
                Error.error " not suffix for [%s] : %s" (Atom.to_string cdcl.name) (show_session_type (auto_fplace (STRecv(t_msg, st_continuation))));

            generated_ports
        in



        (* generated ports must be add before user defined ports,
           since they are children of user-defined ports *)
        let nports = List.length ports in
        let ports = (List.flatten (List.map (generate_intermediate_ports ports) receive_entries)) @ ports in
        logger#debug "number of added intermediate_ports %d" ((List.length ports) - nports);

        (* Update main ports *)
        let ports = List.map (function (port:port) -> 
            { port with 
                value = ({ (fst port.value) with
                    _children = 
                        (match Hashtbl.find_opt children (fst port.value).name with
                            | None -> []
                            | Some l -> l
                        ) @ (fst port.value)._children
                }, snd port.value)
            }
        ) ports in
        let outports = List.map (function (port:outport) -> 
            { port with 
                value = ({ (fst port.value) with
                    _children = 
                        (match Hashtbl.find_opt children (fst port.value).name with
                            | None -> []
                            | Some l -> l
                        ) @ (fst port.value)._children
                }, snd port.value)
            }
        ) outports in

        let body = 
            (List.map (function port -> {value=auto_plgannot(Inport port); place = port.place@fplace}) ports) @
            (List.map (function port -> {value=auto_plgannot (Outport port); place = port.place@fplace}) outports) @
            body_wo_ports_outports in

        (** get_intermediate_port (session, st) 
            main_port_id, st -> this.intermediate_port_name    
        *)


        let body = 
            if Config.is_first_apply_pass pass_name then begin
                (*List<Map<TSessionId, ?>> this.intermediate_states = new ArrayList(); [this. ....]; registration at each creation*)
                (* Tips: we use Atom.builtin since Java code in externals needs to access the state, we can template it 
                *)
                let a_intermediate_states = Atom.fresh "intermediate_states" in
                let intermediate_states_index = auto_place(auto_plgannot(State( auto_place({ 
                    ghost = false;
                    type0 = mtype_of_ct (TList(
                                mtype_of_ct (TDict(
                                    mtype_of_ft TUUID, 
                                    mtype_of_ft TWildcard 
                                ))
                    ));
                    name = a_intermediate_states;
                    body = Some (e2_e(BlockExpr(List, [])))
                })))) in
               
                (* Registere session in case there exists other ports,
                    intermediate session are routed to intermediate_receive port 
                    N.B some kind of dynamic port rerouting
                    TODO REFACTOR: 
                    Maybe an option is just to maintain one port and to update the callback instead of create intermediate ports
                *)
                let registerd_sessions = auto_place(auto_plgannot(State( auto_place({ 
                    ghost = false;
                    type0 = mtype_of_ct (TDict(
                        mtype_of_ft TUUID, 
                        mtype_of_ct (TInport (mtype_of_st STBottom)) 
                    ));
                    name = a_registered_sessions;
                    body = Some (e2_e(Block2Expr(Dict, [])))
                })))) in
                
                (* Intermediate_futures stores futures holding results of non void methods using receive
                *)
                let intermediate_futures = auto_place(auto_plgannot(State( auto_place({ 
                    ghost = false;
                    type0 = mtype_of_ct (TDict(
                        mtype_of_ft TUUID, 
                        mtype_of_ct (TFuture (mtype_of_ft TBottom)) 
                    ));
                    name = a_intermediate_futures;
                    body = Some (e2_e(Block2Expr(Dict, [])))
                })))) in

                (* Intermediate_prebinders stores prebinders for post-conditions delayed by a receive
                *)
                let intermediate_prebinders = auto_place(auto_plgannot(State( auto_place({ 
                    ghost = false;
                    type0 = mtype_of_ct (TDict(
                        mtype_of_ft TUUID, 
                        mtype_of_ct (TList (mtype_of_ft TBottom)) 
                    ));
                    name = a_intermediate_prebinders;
                    body = Some (e2_e(Block2Expr(Dict, [])))
                })))) in


                body @ [intermediate_states_index; registerd_sessions; intermediate_futures; intermediate_prebinders]
            end
            else body
        in

        ComponentStructure { cdcl with body = body}
    and rcdcl cdcl = map_place rewrite_component_dcl cdcl 

    and rewrite_term place = function
    | EmptyTerm -> EmptyTerm
    | Comments c -> Comments c
    | Stmt stmt -> Stmt stmt
    | Component cdcl -> Component (rcdcl cdcl)
    | Class cl -> Class cl
    | Function fcdcl -> 
        (* Precondition ensures there is no receive in function_declaration*)
        Function fcdcl
    | Typealias _ as t -> t
    | Typedef _ as t -> t
    | Derive _ as t -> t
    and rterm term = map_place (transparent_plgannot rewrite_term) term

    and rewrite_program_call_with_continuation = 
        (* rewrite all call of methods registered methods_with_continuations in such that
            m_call(....) becomes

            Future<m.ret_type> f = m_call(...);
            f().get(timeout_custom, TimeUnit.MILLISECONDS);
        *)
        let exclude_next = ref false in
        let callsite_selector = function
            | CallExpr ({value=VarExpr f, _}, _) when Atom.hint f = "wait_future" && Atom.is_builtin f -> (* already rewritten *)
                exclude_next := true;
                false
            | CallExpr ({value=AccessExpr({value=This, _}, {value=VarExpr x, _}), _}, _) ->
                if !exclude_next then (  
                    exclude_next := false;
                    false
                ) else Hashtbl.find_opt methods_with_continuations x <> None 
            | _ -> false
        in

        let callsite_rewriter _ mt = function 
            | CallExpr _ as e ->  
                CallExpr(
                    e2var (Atom.builtin "wait_future"),
                    [
                        e2_e(UnopExpr (UnpackOrPropagateResult,
                        auto_fplace (e, mt)));
                        e2_lit (IntLit 30000) (* timeout in milliseconds, TODO load it from config file *)
                    ]
                )
        in

        rewrite_expr_program callsite_selector callsite_rewriter
    and rewrite_program program = 
        program
        |> List.map rterm
        |> rewrite_program_call_with_continuation
    
    (*****************************************************)
    let name = pass_name 
    let displayed_pass_shortdescription = "recv has been eliminated from IR"
    let displayed_ast_name = "IR recvelim"
    let show_ast = true
    let global_at_most_once_apply = false  
    (* To ensure that this pass only insert one "intermediate_states" per component
        we protect the insertion with "is_first_apply name"
    *)



    let precondition program = 
        (* Check: no receive in function_declaration
                since we can not create async port to handle it (not in a component)
        *)
        let fdcls = List.filter (function |{value={v=Function _}} -> true |_ -> false) program in
        let fdcls = List.map (function |{value={v=Function fdcl}} -> fdcl.value) fdcls in
        List.iter (function (fdcl:_function_dcl) -> 
            List.iter 
                (function stmt -> ignore (collect_expr_stmt (Some fdcl.name) Atom.Set.empty receive_selector (receive_collector "receive can not be defined into function (only inside component methods)") stmt))
                fdcl.body
            ) fdcls;

        program
    let postcondition program = 
        (* Check: no more receive *)
        ignore (collect_expr_program Atom.Set.empty receive_selector (receive_collector "receive() remains in IR after Rewriting") program);
        
        program

    let apply_program = rewrite_program
end