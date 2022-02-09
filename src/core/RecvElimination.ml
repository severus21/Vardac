open IR
open Easy_logging
open Utils
open AstUtils

let logger = Logging.make_logger "_1_ compspec" Debug [];;
let fplace = (Error.forge_place "RecvElimination" 0 0) 
let auto_fplace smth = {place = fplace; value=smth}
include AstUtils2.Mtype.Make(struct let fplace = fplace end)

let receive_selector = function 
    | (CallExpr ({value=(VarExpr x, _)}, [s; bridge])as e) when Atom.is_builtin x && Atom.hint x = "receive" -> true
    | _ -> false

let receive_collector msg parent_opt env e = 
    let parent = match parent_opt with | None -> "Toplevel" | Some p -> Atom.to_string p in
    Error.error e.place "%s. Parent = %s" msg parent

(*
    input [x_1; ... ; x_n]
    return [x_1; ..; x_n-1], x_n
*)
(* TODO move it out *)
let split_last = 
    let rec _split_last acc = function
        | [] -> failwith "splitlast"
        | x::[] -> List.rev acc,  x
        | x::xs -> _split_last (x::acc) xs
    in
    _split_last []

(***************************************************)

module type Sig = sig
    include IRCompilationPass.Pass
end
let troloc = ref 0 (* TODO remove *)

module Make () : Sig = struct
    (*
        Architecture remains unchanged - rewriting architecture is done in an other module (to be written)
        - get ride of session.receive only use ports
            TODO FIXME only scan component method at this point
    *)


    (*******************************************************)
    (* rcev -> toplevel let or toplevel expression statement (return etc ...) or nested let .. =recv in block (if, ..)*)
    let rec to_X_form place stmt : stmt list =
        let fplace = (Error.forge_place "Core.Rewrite.to_X_form" 0 0) in
        let auto_place smth = {place = place; value=smth} in
        let auto_fplace smth = {place = fplace; value=smth} in

        let flag_debug = ref false in

        (* 
            extract_recv "f(s.recv(...))"
            =>
            [ "fresh_name = s.recv(...)"], "f(fresh_name)"
        *)
        let recv_selector = function
        | (CallExpr ({value=(VarExpr x, _)}, [s; bridge])as e) when Atom.is_builtin x && Atom.hint x = "receive" -> true
        | _ -> false
        in
        let recv_rewriter parent_opt mt_e = function
        | (CallExpr ({value=(VarExpr x, _)}, [s; bridge]) as e) when Atom.is_builtin x && Atom.hint x = "receive" ->
            begin
                match mt_e.value with
                | CType {value=TTuple [_; {value=SType _}]} -> ()
                | _ -> raise (Error.PlacedDeadbranchError (mt_e.place, Printf.sprintf "to_X_form: type of the receive() is incorrect\n%s\n it should match the following pattern\nCType {value=TTuple [_; {value=SType _}]}" (show_main_type mt_e)))
            end;

            logger#warning ">>>> extract_recvs -> detect receive %s"(Error.show place);
            flag_debug := true;
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
        | LetStmt (_, _, {value=(CallExpr ({value=(VarExpr x, _)}, [s; bridge]),_) as e}) as stmt  when Atom.is_builtin x && Atom.hint x = "receive" ->
            logger#warning ">>>> to_X_form -> detect receive";
            true 
        | _ -> false 
        in

        let stmts = rewrite_exprstmts_stmt None stmt_exclude recv_selector recv_rewriter {place; value=stmt} in
        (* Debug *)
        (*if !flag_debug then (
            (Format.fprintf Format.std_formatter "%a" (Error.pp_list "\n" (fun out stmt -> Format.fprintf out "%s " (show_stmt stmt))) stmts);
            failwith "";
        );*)
        stmts

    type method_info = ((expr_variable * main_type * main_type) option * expr option * _method0)

    (*@param where res - name let res = receive*)
    let compute_intermediate_args remaining_stmts res_opt = 
        let _, intermediate_args = List.fold_left_map free_vars_stmt Atom.Set.empty remaining_stmts in
        let intermediate_args = List.flatten intermediate_args in
        (* TODO remove 
        let _, bind_in_stage = List.fold_left_map free_vars_stmt Atom.Set.empty stage_stmts in 
        let bind_in_stage = List.flatten bind_in_stage in
        *)

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

        (* TODO remove
        (* Remove args that are binded inside stage_stmts *)
        let excluded2 = Atom.Set.of_seq (List.to_seq (List.map snd bind_in_stage)) in
        List.filter (function (_,x) -> Atom.Set.find_opt x excluded2 <> None) intermediate_args
        *)
        intermediate_args


    (* 
        @param s1_opt - receive(s1, ..) in m1, s1_opt = Some(s1) if m1 is the main method 
        @param intermediate_args - args that should propagated from 1 to 2 
        @param (res, t_msg, st_continuation) - tuple<T_msg, st_continuation> res = receive(...); that separate both methods
    *)
    let rewrite_methodint (m1 : _method0) (m2 : _method0) s1_opt intermediate_args (res, t_msg, st_continuation) : state list * _method0 * _method0 =
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

        match intermediate_args with
        | [] -> (* Only load recv in the [res] variable *)
            let m2 = { m2 with 
                (* TODO add cleansing when timeout *)
                body = load_recv_result @ m2.body} in

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

            let intermediate_state = auto_fplace (StateDcl {
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
        
            (*** Add footer to m2 to load the intermediate in state and store the resulut of receive in [res] ***)
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
                            e2_e( CallExpr(
                                e2var (Atom.builtin "nth"),
                                [ 
                                    e2_lit (IntLit i);
                                    e2var local_tmp_args 
                                ]
                            ))
                        )) 
                    ) intermediate_args
                )
                @ m2.body; 
            } in

            (*** Returns ***)
            [intermediate_state], m1, m2


    let rec split_body (main_name, main_annotations) acc_stmts (next_method:_method0) : stmt list -> state list * port list * _method0 list =
        let fplace = (Error.forge_place "Core.Rewrite.split_body" 0 0) in
        let auto_fplace smth = {place = fplace; value=smth} in

        function
        | [] -> 
            let current_method = { next_method with 
                body = next_method.body @ (List.rev acc_stmts);
            } in

            [], [], [ current_method ]
        | {place; value=LetStmt ({value=CType{value=TTuple [t_msg;{value = SType st_continuation}]}}, let_x, {value=(CallExpr ({value=(VarExpr x, _)}, [s; bridge]),_) as e})}::stmts  when Atom.is_builtin x && Atom.hint x = "receive" -> 
            (*** Prepare ***)
            let stage_stmts = next_method.body @ (List.rev acc_stmts) in
            let current_method = { next_method with 
            body = stage_stmts; } in

            (*** Shifting***)
            (*
                creating the new_next_method and the current next_method becomes the current method
            *)
            let next_method = {
                annotations = main_annotations;
                ghost = false;
                ret_type = mtype_of_ft TVoid; 
                name = Atom.fresh ((Atom.hint main_name)^"_intermediate");
                args = [
                    auto_fplace (t_msg, Atom.fresh "e");
                    auto_fplace (mtype_of_st st_continuation.value, Atom.fresh "session");
                ]; 
                body = []; (*Will be update later*)
                contract_opt = None;
                on_destroy = false;
                on_startup = false;
            } in

            (*** Gathering intells ***)
            let intermediate_args = compute_intermediate_args stmts (Some let_x) in

            
            (*** Creating link between current_method and next_method, before shifting ***)
            let intermediate_states, current_method, next_method = rewrite_methodint current_method next_method (Some s) intermediate_args (let_x, t_msg, st_continuation) in

            (*** Creation of the intermediate port ***)
            let intermediate_port = auto_fplace ({
                name = Atom.fresh ((Atom.hint current_method.name)^"_intermediate_port");
                input = bridge;
                callback = e2_e (AccessExpr (
                    e2_e This, 
                    e2var next_method.name
                ));
                expecting_st = mtype_of_st (STRecv (t_msg, st_continuation));
            }, auto_fplace EmptyMainType) in
            let intermediate_ports = [intermediate_port] in

            (*** Since we introduce intermediate let (even for same variable) we need to attribute fresh identities ***)
            let to_rename = Atom.VMap.of_list (List.map (function (_,x) -> x, Atom.fresh (Atom.hint x)) intermediate_args) in
            let renaming x = match Atom.VMap.find_opt x to_rename with 
                | Some y -> y  
                | None -> x
            in
            (* rename remaining stmts*)
            let stmts = List.map (rename_stmt renaming) stmts in 
            (* renaming let i = nth(res, ...)*)
            let next_method = {next_method with body = List.map (rename_stmt renaming) next_method.body } in

            (*** Returns and rec call***)
            (*  NB. Initial param_current_method is unused 
                See whitepaper for details
            *)
            let intermediate_methods = [current_method] in 


            let intermediate_states2, intermediate_ports2, intermediate_methods2 = split_body (main_name, main_annotations) [] next_method stmts in           

            intermediate_states@intermediate_states2,
            intermediate_ports@intermediate_ports2,
            intermediate_methods@intermediate_methods2
        
        (* If case - conditional branching is painfull *)
        | ({place; value = IfStmt (e, stmt1, stmt2_opt)} as stmt) :: stmts -> 
            let intermediate_states1, intermediate_ports1, intermediate_methods1 =
                let intermediate_states1, intermediate_ports1, intermediate_methods1 = 
                    split_body (main_name, main_annotations) [] {next_method with body = []} [stmt1] in
                intermediate_states1, intermediate_ports1, intermediate_methods1
            in
            
            let intermediate_states2, intermediate_ports2, intermediate_methods2 =
                match stmt2_opt with
                | None -> [], [], []
                | Some stmt2 ->
                    let intermediate_states2, intermediate_ports2, intermediate_methods2 = 
                        split_body (main_name, main_annotations) [] {next_method with body = []} [stmt2] in
                    intermediate_states2, intermediate_ports2, intermediate_methods2
            in

            (*** Returns and rec call***)
            if intermediate_methods1 <> [] || intermediate_methods2 <> [] then
            begin
                logger#debug "Detect receive in If block";
                (* There is at least one receive inside stmt1 or stmt2_opt *)


                (*** 
                    stmts should be add to branches with receive 
                    i.e. method creation + call 
                ***)
                let intermediate_states3, intermediate_ports3, intermediate_methods3 = split_body (main_name, main_annotations) [] next_method stmts in           
                
                let m0, ms = 
                    match intermediate_methods3 with
                        | m0::ms -> m0, ms
                        | _ -> failwith "ERROR in RECVelim if"
                in

                let stmt_continuation = 
                    auto_fplace (ExpressionStmt (e2_e (CallExpr (
                        e2_e (AccessExpr (e2_e This, e2var m0.name)),
                        List.map (* TODO not sure if args are passed in the correct order *)
                            (function (_, x) -> e2var x)
                            (
                                List.flatten (snd (List.fold_left_map free_vars_stmt (Atom.Set.of_list (List.map (function param -> snd param.value) m0.args)) m0.body))
                            )
                    ))))
                in
                let ms1, mlast1 = split_last intermediate_methods1 in
                let mlast1 = 
                    { mlast1 with
                        body = mlast1.body @ 
                        [ stmt_continuation ]
                    }
                in
                let intermediate_methods1 = ms1 @ [mlast1] in

                let ms2, mlast2 = split_last intermediate_methods1 in
                let mlast2 = 
                    { mlast2 with
                        body = mlast2.body @ 
                        [ stmt_continuation ]
                    }
                in
                let intermediate_methods2 = ms2 @ [mlast2] in

                (*** Built stmt added to stage_stmts ***)
                let m1_0::intermediate_methods1 = intermediate_methods1 in
                let m2_0::intermediate_methods2 = intermediate_methods2 in

                let stmt = auto_fplace (IfStmt(
                    e,
                    (match intermediate_ports1 with 
                    | [] -> 
                        stmt1 (* no receive in stmt1 *)
                    | _ -> begin
                        logger#debug "Detect receive in If block1";
                        (* at least a receive somewhere in stmt1 *) 
                        auto_fplace (BlockStmt m1_0.body)
                    end),
                    (match intermediate_ports2 with 
                    | [ _ ] -> stmt2_opt (* no receive in stmt2 *)
                    | _ -> begin
                        logger#debug "Detect receive in If block2";
                        (* at least a receive somewhere in stmt2 *) 
                        Some (auto_fplace (BlockStmt m2_0.body))
                    end)
                )) in

                (* Sanity check *)
                collect_expr_stmt None Atom.Set.empty receive_selector (fun _ _ _ -> assert false) stmt;

                
                (*** Finish parent split execution branch since there is conditional branching *)
                let current_method = { next_method with 
                    body = next_method.body @ (List.rev (stmt::acc_stmts));
                } in

                intermediate_states1@intermediate_states2@intermediate_states3,
                intermediate_ports1@intermediate_ports2@intermediate_ports3,
                current_method::intermediate_methods1@intermediate_methods2@intermediate_methods3
            end
            else
                (* If has no receive inside *)
                split_body (main_name, main_annotations) (stmt::acc_stmts) next_method stmts
        | ({value = BlockStmt stmts1} as stmt)::stmts2 ->
            let intermediate_states1, intermediate_ports1, intermediate_methods1 = split_body (main_name, main_annotations) [] next_method stmts1 in


            if intermediate_ports1 <> [] then
            begin (* receive in stmts1 *)
                let intermediate_states2, intermediate_ports2, intermediate_methods2 = split_body (main_name, main_annotations) (stmt::acc_stmts) next_method stmts2 in
                let m0::intermediate_methods2 = intermediate_methods2 in

                (*** Add stmts2 as a continuation of Block ***)
                (* TODO does it works with BlockExpr IfStmt receive ???? *)
                let ms, mlast = split_last intermediate_methods1 in
                let mlast = 
                    { mlast with
                        body = mlast.body @ 
                        [
                            auto_fplace (ExpressionStmt (e2_e (CallExpr (
                                e2_e (AccessExpr (e2_e This, e2var m0.name)),
                                List.map (* TODO not sure if args are passed in the correct order *)
                                    (function (_, x) -> e2var x)
                                    (
                                        List.flatten (snd (List.fold_left_map free_vars_stmt (Atom.Set.of_list (List.map (function param -> snd param.value) m0.args)) m0.body))
                                    )     
                            ))))
                        ]
                    }
                in
                let intermediate_methods2 = ms @ [mlast] in

                (*** Compute stmt to be added to ***)
                let m1_0::intermediate_methods1 = intermediate_methods1 in
                let stmt = auto_fplace (BlockStmt m1_0.body) 
                in

                (*** Finish parent split execution branch since there is conditional branching *)
                let current_method = { next_method with 
                    body = next_method.body @ (List.rev (stmt::acc_stmts));
                } in

                intermediate_states1@intermediate_states2,
                intermediate_ports1@intermediate_ports2,
                current_method::intermediate_methods1@intermediate_methods2

            end
            else split_body (main_name, main_annotations) (stmt::acc_stmts) next_method stmts2

            
        (* TODO for others stmt that can host nested let*)
        | stmt::stmts -> split_body (main_name, main_annotations) (stmt::acc_stmts) next_method stmts



    let rec rewrite_method0 place (m:_method0) : port list * state list * method0 list = 
        let fplace = (Error.forge_place "Core.Rewrite.rewrite_method0" 0 0) in
        let auto_fplace smth = {place = fplace; value=smth} in

        let stmts = List.flatten (List.map (function stmt -> to_X_form stmt.place stmt.value) m.body) in

        (* Debug *)
        (*let m = {m with body = []} in
        let blblbl = function
        | LetStmt (_, _, {value=(CallExpr ({value=(VarExpr x, _)}, [s; bridge]),_) as e}) as stmt  when Atom.is_builtin x && Atom.hint x = "receive" ->
            incr troloc;
            logger#warning ">>>> to_X_form -> correct at the end %d" !troloc;
            true 
        | _ -> false 
        in
        List.map (rewrite_stmt_stmt false blblbl (fun place stmt -> [stmt])) stmts;*)
        (* End debug *)


        let intermediate_states, intermediate_ports, intermediate_methods = split_body (m.name, m.annotations) [] {m with body = []} stmts in
        logger#debug "nbr intermediate_methods %d" (List.length intermediate_methods);
        let intermediate_methods = List.map auto_fplace intermediate_methods in

        intermediate_ports, intermediate_states, intermediate_methods
    and rmethod0 m = rewrite_method0 m.place m.value

    (* return name of intermediate states * citems *)
    and rewrite_component_item place : _component_item -> expr_variable list * component_item list = 
    let fplace = (Error.forge_place "Core.Rewrite" 0 0) in
    let auto_place smth = {place = place; value=smth} in
    let auto_fplace smth = {place = fplace; value=smth} in
    function
    | State _ as citem -> [], [auto_place citem]
    | Contract _ as citem -> [], [auto_place citem] 
    | Method m as citem -> 
        let intermediate_ports, intermediate_states, intermediate_methods = rmethod0 m in
        if intermediate_ports = [] then 
            [], [auto_place citem]
        else 
            List.map (function | {value=StateDcl s} -> s.name) intermediate_states
            , (List.map (function p-> auto_fplace (Inport p)) intermediate_ports) @ 
            (List.map (function s-> auto_fplace (State s)) intermediate_states)
            @
            (List.map (function m-> auto_fplace (Method m)) intermediate_methods)
    | (Inport _ as citem) | (Outport _ as citem) -> [], [auto_place citem]
    | Term t -> [], [auto_place (Term (rterm t))]
    | Include _ as citem -> [], [auto_place citem]
    and rcitem citem : expr_variable list * component_item list = rewrite_component_item citem.place citem.value 

    and rewrite_component_dcl place : _component_dcl -> _component_dcl = 
    let fplace = (Error.forge_place "Core.Rewrite" 0 0) in
    let auto_place smth = {place = place; value=smth} in
    let auto_fplace smth = {place = fplace; value=smth} in
    function
    | ComponentAssign _ as cdcl -> cdcl
    | ComponentStructure cdcl -> 
        let body = cdcl.body in

        (* Elimination of sync receiv *)
        let intermediate_state_names, body = List.split(List.map rcitem body) in
        let intermediate_state_names = List.flatten(intermediate_state_names) in
        let body = List.flatten body in

        (*List<Map<UUID, ?>> this.intermediate_states = new ArrayList(); [this. ....]; registration at each creation*)
        let a_intermediate_states = Atom.fresh "intermediate_states" in
        let intermediate_states_index = auto_place(State( auto_place(StateDcl { 
            ghost = false;
            type0 = auto_fplace(CType(auto_fplace (TList(
                        auto_fplace(CType(auto_fplace (TDict(
                            auto_fplace (CType (auto_fplace (TFlatType TUUID))), 
                            auto_fplace (CType (auto_fplace (TFlatType TWildcard))) 
                        ))))
            ))));
            name = a_intermediate_states;
            body = Some (e2_e(BlockExpr(List, [])))
        }))) in
        let body = body @ [intermediate_states_index] in 

        ComponentStructure { cdcl with body }
    and rcdcl cdcl = map_place rewrite_component_dcl cdcl 

    and rewrite_term place = function
    | EmptyTerm -> EmptyTerm
    | Comments c -> Comments c
    | Stmt stmt -> Stmt stmt
    | Component cdcl -> Component (rcdcl cdcl)
    | Function fcdcl -> 
        (* Precondition ensures there is no receive in function_declaration*)
        Function fcdcl
    | Typealias _ as t -> t
    | Typedef _ as t -> t
    | Derive _ as t -> t
    and rterm term = map_place rewrite_term term

    and rewrite_program program = 
        List.map rterm program
    
    (*****************************************************)

    let displayed_pass_shortdescription = "recv has been eliminated from IR"
    let displayed_ast_name = "IR recvelim"
    let show_ast = true


    let precondition program = 
        (* Check: no receive in function_declaration
                since we can not create async port to handle it (not in a component)
        *)
        let fdcls = List.filter (function |{value=Function _} -> true |_ -> false) program in
        let fdcls = List.map (function |{value=Function fdcl} -> fdcl.value) fdcls in
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