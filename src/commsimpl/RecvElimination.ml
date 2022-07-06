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
    let logger = make_log_of "RecvElimination"
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
            annotations = main_annotations;
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
    (* rcev -> toplevel let or toplevel expression statement (return etc ...) or nested let .. =recv in block (if, ..)*)
    let rec to_X_form place stmt : stmt list =
        let fplace = (Error.forge_place "Core.Rewrite.to_X_form" 0 0) in
        let auto_place smth = {place = place; value=smth} in
        let auto_fplace smth = {place = fplace; value=smth} in

        (* 
            extract_recv "f(s.recv(...))"
            =>
            [ "fresh_name = s.recv(...)"], "f(fresh_name)"
        *)
        let recv_selector = function
        | (CallExpr ({value=(VarExpr x, _)}, [s])as e) when Atom.is_builtin x && Atom.hint x = "receive" -> true
        | _ -> false
        in
        let recv_rewriter parent_opt mt_e = function
        | (CallExpr ({value=(VarExpr x, _)}, [s]) as e) when Atom.is_builtin x && Atom.hint x = "receive" ->
            begin
                match mt_e.value with
                | CType {value=TTuple [_; {value=SType _}]} -> ()
                | _ -> raise (Error.PlacedDeadbranchError (mt_e.place, Printf.sprintf "to_X_form: type of the receive() is incorrect\n%s\n it should match the following pattern\nCType {value=TTuple [_; {value=SType _}]}" (show_main_type mt_e)))
            end;

            logger#warning ">>>> extract_recvs -> detect receive %s"(Error.show place);
            let tmp = Atom.fresh "tmp_receive" in
            let recv = auto_place (e, mt_e) in 
            [
                auto_fplace (LetStmt (
                    mt_e,
                    tmp, 
                    recv)
                );
            ], (VarExpr tmp, mt_e )
        in

        let stmt_exclude = function
        | LetStmt (_, _, {value=(CallExpr ({value=(VarExpr x, _)}, [s]),_) as e}) as stmt  when Atom.is_builtin x && Atom.hint x = "receive" ->
            logger#warning ">>>> to_X_form -> detect receive";
            true 
        | _ -> false 
        in

        rewrite_exprstmts_stmt None stmt_exclude recv_selector recv_rewriter {place; value=stmt}

    type method_info = ((expr_variable * main_type * main_type) option * expr option * _method0)

    (*@param where res - name let res = receive*)
    let compute_intermediate_args remaining_stmts res_opt = 
        let _, intermediate_args = List.fold_left_map free_vars_stmt Atom.Set.empty remaining_stmts in
        let intermediate_args = List.flatten intermediate_args in

        (* Safety check *)
        List.iter (function |({value=EmptyMainType}, x) -> logger#error "%s" (Atom.to_string x);assert(false) | _ -> ()) intermediate_args;

        (* Remove components *)
        let intermediate_args = List.filter (function (_, x) -> 
            (Str.string_match (Str.regexp "^[A-Z].*") (Atom.hint x) 0) = false) intermediate_args 
        in

        (* Case "let res = receive(..)"
            res = Tuple.of(e, session); res must not be loaded from an intermediate state
        *)
        let intermediate_args = 
            (
                match res_opt with 
                | None -> Fun.id
                | Some res -> List.filter (function (_, x) -> x <> res) 
            )
            intermediate_args 
        in

        intermediate_args


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


    let rec split_body a_registered_sessions (main_name, main_annotations) acc_stmts (next_method:_method0) : stmt list -> state list * (Atom.atom * main_type * session_type * expr) list * _method0 list =
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
            logger#debug "receive at %s" (Error.show place1);
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


            let receive_id = Atom.fresh ((Atom.to_string current_method.name)^"receive_id") in

            let a_intermediate_port_name = Atom.fresh ((Atom.hint current_method.name)^"_intermediate_port") in 
            let e_current_intermediate_port = e2_e(
                CallExpr(
                    e2_e(AccessExpr(
                        e2_e This,
                        e2var (Atom.builtin "__get_intermediate_port")
                    )),
                    [
                        s
                    ]
            )) in
            
            (*** Creating link between current_method and next_method, before shifting ***)
            let intermediate_states, current_method, next_method = rewrite_methodint a_registered_sessions current_method next_method (Some s) e_current_intermediate_port intermediate_args (let_x, t_msg, st_continuation) in

            (*** Creation of the intermediate ports ***)
            let receive_entries = [(receive_id, t_msg, st_continuation, e2var next_method.name)] in


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


            let intermediate_states2, receive_entries2, intermediate_methods2 = split_body a_registered_sessions (main_name, main_annotations) [] next_method stmts in           

            intermediate_states@intermediate_states2,
            receive_entries@receive_entries2,
            intermediate_methods@intermediate_methods2
        
        (* If case - conditional branching is painfull *)
        | ({place; value = IfStmt (e, stmt1, stmt2_opt)} as stmt) :: stmts -> 
            logger#debug "recv-elim IfStmt";
            (* DEBUG Code split_body (main_name, main_annotations) acc_stmts next_method (stmt1::stmts)*)
            let _,_,flag1 = collect_expr_stmt None Atom.Set.empty receive_selector (fun _ _ _ -> [true]) stmt1 in
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
                        let _, ftvars = List.split (List.map (free_tvars_stmt Atom.Set.empty) stmts) in
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
                        but since cook introduce unique name we do not need to preserve it a separate BlockStmt
                    *)
                    let branch_stmts = stmt_branch :: (duplicate_stmts stmts) in

                    let next_method_branch = fresh_next_method main_name main_annotations t_msg_cont in
                    let intermediate_states_branch, receive_entries_branch, intermediate_methods_branch =
                        let intermediate_states_branch, receive_entries_branch, intermediate_methods_branch = 
                            split_body a_registered_sessions (main_name, main_annotations) [] next_method_branch branch_stmts in
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
                split_body a_registered_sessions (main_name, main_annotations) (stmt::acc_stmts) next_method stmts
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
                split_body a_registered_sessions (main_name, main_annotations) acc_stmts next_method full_stmts
            end
            else split_body a_registered_sessions (main_name, main_annotations) (stmt::acc_stmts) next_method stmts2

            
        (* TODO for others stmt that can host nested let*)
        | stmt::stmts -> split_body a_registered_sessions (main_name, main_annotations) (stmt::acc_stmts) next_method stmts





    let rec rewrite_method0 a_registered_sessions place (m:_method0) = 
        let fplace = (Error.forge_place "Core.Rewrite.rewrite_method0" 0 0) in
        let auto_fplace smth = {place = fplace; value=smth} in

        let stmts = List.flatten (List.map (function stmt -> to_X_form stmt.place stmt.value) m.body) in

        let intermediate_states, receive_entries, intermediate_methods = split_body a_registered_sessions (m.name, m.annotations) [] {m with body = []} stmts in
        let intermediate_methods = List.map auto_fplace intermediate_methods in

        receive_entries, intermediate_states, intermediate_methods
    and rmethod0 a_registered_sessions = map0_place (rewrite_method0 a_registered_sessions)

    (* return name of intermediate states * citems *)
    and rewrite_component_item a_registered_sessions place = 
    let fplace = (Error.forge_place "Core.Rewrite" 0 0) in
    let auto_place smth = {place = place; value=smth} in
    let auto_fplace smth = {place = fplace; value=smth} in
    function
    | State _ as citem -> 
        ([], []), [auto_place (auto_plgannot citem)]
    | Contract _ as citem -> 
        ([], []), [auto_place (auto_plgannot citem)] 
    | Method m as citem -> 
        let receive_entries, intermediate_states, intermediate_methods = rmethod0 a_registered_sessions m in

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
    and rcitem a_registered_sessions = map0_place (transparent0_plgannot(rewrite_component_item a_registered_sessions))

    and rewrite_component_dcl place : _component_dcl -> _component_dcl = 
    let fplace = (Error.forge_place "Core.Rewrite" 0 0) in
    let auto_place smth = {place = place; value=smth} in
    let auto_fplace smth = {place = fplace; value=smth} in
    function
    | ComponentAssign _ as cdcl -> cdcl
    | ComponentStructure cdcl -> 
        (* Define component names *)
        let a_registered_sessions = Atom.fresh "registered_session" in 


        let body = cdcl.body in

        (* Elimination of sync receiv *)
        let tmp, body = List.split(List.map (rcitem a_registered_sessions) body) in
        let intermediate_state_names, receive_entries = List.split tmp in
        let intermediate_state_names = List.flatten(intermediate_state_names) in
        let body = List.flatten body in
        let receive_entries  = List.flatten receive_entries in

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
        let generate_intermediate_ports ports (receive_id, t_msg, st_continuation, callback) = 
            logger#debug "generate_intermediate_port for %s and ports %d" (Atom.to_string cdcl.name) (List.length ports);
            (* Remove ports that are already binded to receive *)
            let ports = List.filter (function p -> Bool.not (fst p.value)._is_intermediate) ports in

            let generate_intermediate_port_for_port portname =
                let intermediate_port_name = Atom.fresh ((Atom.to_string receive_id)^"_intermediate_port") in

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
                    }, auto_fplace EmptyMainType)
                ]
            in

            (* When session is initiated by an inport *)
            List.flatten (List.map (function port ->
                if Common.TypingUtils.is_suffix_st  (auto_fplace (STRecv(t_msg, st_continuation))) (match (fst port.value).expecting_st.value with SType st -> st) then 
                    (* p or one of its stages is concerned by the receive*)
                    generate_intermediate_port_for_port (fst port.value).name
                else []
            ) ports)
            @
            (* When session is initiated by an outport *)
            List.flatten (List.map (function (port:outport) ->
                if Common.TypingUtils.is_suffix_st (auto_fplace (STRecv(t_msg, st_continuation))) (match (fst port.value).protocol.value with 
                    | SType st -> st 
                    | mt -> raise (Error.PlacedDeadbranchError (port.place, Printf.sprintf "Wrong outport type %s" (show__main_type mt)))
                ) then 
                    (* p or one of its stages is concerned by the receive*)
                    generate_intermediate_port_for_port (fst port.value).name
                else []
            ) outports)
        in

        (* generated ports must be add before user defined ports,
           since they are children of user-defined ports *)
        let ports = (List.flatten (List.map (generate_intermediate_ports ports) receive_entries)) @ ports in

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
                    FIXME Atom.builtin -> Atom.fresh to preserve binder unicity
                *)
                let a_intermediate_states = Atom.builtin "intermediate_states" in
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

                body @ [intermediate_states_index; registerd_sessions]
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

    and rewrite_program program = 
        List.map rterm program
    
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