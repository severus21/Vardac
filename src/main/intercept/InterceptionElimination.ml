open Core
open AstUtils
open IR
open Easy_logging
open IRMisc
open InterceptUtils
open Common
 


module type TArgs = sig
    (* name -> interceptor_info *)
    val interceptors_info : (Atom.atom, interceptor_info) Hashtbl.t
end

module Make (Args: TArgs) = struct
    let logger = Core.Utils.make_log_of "InterceptionElimination" 

    let fplace = (Error.forge_place "Intercept" 0 0) 
    let auto_fplace smth = {place = fplace; value=smth}
    include AstUtils2.Mtype.Make(struct let fplace = fplace end)

    (******************* Shared state of the pass **************************)
    include Args


    let resolved_dependencies = ref Atom.Set.empty

    (*************** Step 0 - gather intell ******************)
    let methods_of (base_interceptor : component_structure) : method0 list =
        let citems = List.filter (function {value={v=Method _}} -> true | _ -> false) base_interceptor.body in
        List.map (function |{value={v=Method m}} -> m) citems

    let extract_onboard_methods (bi_methods : method0 list) : method0 list = 
        List.filter (function (m : method0) -> List.exists (function | Onboard _ -> true | _ -> false ) m.value.annotations ) bi_methods 
    
    let extract_session_intercept_methods  (bi_methods : method0 list) : method0 list = 
        List.filter (function (m : method0) -> List.exists (function | SessionInterceptor _ -> true | _ -> false ) m.value.annotations ) bi_methods 

    let extract_message_intercept_methods  (bi_methods : method0 list) : method0 list = 
        List.filter (function (m : method0) -> List.exists (function | MsgInterceptor _ -> true | _ -> false ) m.value.annotations ) bi_methods 

    (* TODO/TODOC is there a way to index this methods and not to run is_subtype in an O(n²) strategy for pairing ports with interception methods ???? *)

    module InterceptedPortSet = struct
        include Set.Make(struct 
            type t = (Atom.atom * (_port * main_type))(* name * port_def * tport *)
            let compare (a,_) (c, _) = Atom.compare a c
        end)

        let to_list set = List.of_seq (to_seq set)
    end

    (**
        @returns set of intercepted_ports of a schema 
        Do not captures the ports of nested schemas - since they are hidden for the outside.
    *)
    let extract_intercepted_ports_of_schema (schema_struct : component_structure) : InterceptedPortSet.t = 
        let intercepted_ports = List.map (function
        | {value={v=Inport p}} -> Some ((fst p.value).name, p.value) | _ -> None ) schema_struct.body in
        let intercepted_ports = List.filter Option.is_some intercepted_ports in
        let intercepted_ports = List.map Option.get intercepted_ports in

        InterceptedPortSet.of_seq (List.to_seq intercepted_ports)




    module InterceptedOutportSet = struct
        include Set.Make(struct 
            type t = (Atom.atom * (_outport * main_type))(* name * port_def * tport *)
            let compare (a,_) (c, _) = Atom.compare a c
        end)

        let to_list set = List.of_seq (to_seq set)
    end

    (**
        @returns set of intercepted_outputports of a schema 
        Do not captures the ports of nested schemas - since they are hidden for the outside.
    *)
    let extract_intercepted_outputports_of_schema (schema_struct : component_structure) : InterceptedOutportSet.t = 
        let intercepted_outputports = List.map (function
        | {value={v=Outport p}} -> Some ((fst p.value).name, p.value) | _ -> None ) schema_struct.body in
        let intercepted_outputports = List.filter Option.is_some intercepted_outputports in
        let intercepted_outputports = List.map Option.get intercepted_outputports in

        InterceptedOutportSet.of_seq (List.to_seq intercepted_outputports)

    let compute_intercepted_outputports program (schemas : Atom.Set.t) = 
        let intercepted_outputports_per_schema = Hashtbl.create 16 in

        let selector = function
            | Component {value = ComponentStructure cstruct} -> Atom.Set.find_opt cstruct.name schemas <> None
            | _ -> false
        in
        let collector _ _ = function
            | Component {value = ComponentStructure cstruct} -> 
                Hashtbl.add intercepted_outputports_per_schema cstruct.name (extract_intercepted_outputports_of_schema cstruct);
                []
        in

        (* Hydrate hashtbl *)
        ignore (collect_term_program true selector collector program);

        intercepted_outputports_per_schema

    (*************** Step 1 - Activation onboarding block******************)

    let generate_onboard_index interceptor_info onboard_methods = 
        (* for a given interceptor *)
        let onboard_index = Hashtbl.create 16 in

        List.iter ( function (m:method0) ->
            List.iter (function 
                | Onboard schemas -> 
                    List.iter (function schema -> Hashtbl.add onboard_index schema m.value.name) schemas  
                | _ -> ()) m.value.annotations
        ) onboard_methods;

        onboard_index

    (**
        method onboard_A for schema A
    *)
    let get_onboard_of_schema default_onboard onboard_index schema = 
        match Hashtbl.find_opt onboard_index schema with
        | Some m -> m
        | None -> default_onboard 

    
    (** branch: if flag == "Schema" ... *)
    let generate_main_callback_branch interceptor_info default_onboard onboard_index (e_this_b_onboard, e_this_onboarded_activations) ((param_schema, e_param_schema), (param_s, e_param_s)) schema: branch_stmt =
        let a_mt = mtype_of_ct (TActivationRef (mtype_of_cvar schema)) in 
        let e_onboard_A = e2_e (AccessExpr(
            e2_e This,
            e2var (get_onboard_of_schema default_onboard onboard_index schema)
        )) in 

        let local_res, e_local_res = e_param_of "res" in
        let local_res2, e_local_res2 = e_param_of "_res" in
        let local_s2, e_local_s2 = e_param_of "_s" in
        let local_s3, e_local_s3 = e_param_of "__s" in
        let local_a, e_local_a = e_param_of "a" in
        let local_p, e_local_p = e_param_of "p" in
        let local_flag, e_local_flag = e_param_of "flag" in


        {
            branch_label = schema_to_label fplace schema;
            branch_s = local_s2;
            body = auto_fplace (BlockStmt [
                auto_fplace (LetStmt (
                    mtype_of_ct (TTuple [ 
                        mtype_of_ct (TTuple [ a_mt; mtype_of_ft TPlace]);
                        mtype_of_st (STSend (mtype_of_ft TBool, auto_fplace STEnd))
                    ]),
                    local_res,
                    e2_e (CallExpr (
                        e2var (Atom.builtin "receive"),
                        [
                            e2var local_s2;
                        ]
                    ))
                ));
                auto_fplace (LetStmt (
                    mtype_of_ct (TTuple [ a_mt; mtype_of_ft TPlace]),
                    local_res2,
                    e2_e (AccessExpr (
                        e_local_res,
                        e2var (Atom.builtin "_0")
                    ))
                ));
                auto_fplace (LetStmt (
                    mtype_of_st (STSend (mtype_of_ft TBool, auto_fplace STEnd)),
                    local_s3,
                    e2_e (AccessExpr (
                        e_local_res,
                        e2var (Atom.builtin "_1")
                    ))
                ));
                auto_fplace (LetStmt (
                    a_mt, 
                    local_a,
                    e2_e (AccessExpr (
                        e_local_res2,
                        e2var (Atom.builtin "_0")
                    ))
                ));
                auto_fplace (LetStmt (
                    mtype_of_ft TPlace,
                    local_p,
                    e2_e (AccessExpr (
                        e_local_res2,
                        e2var (Atom.builtin "_1")
                    ))
                ));


                auto_fplace( LetStmt (
                    mtype_of_ft TBool,
                    local_flag,
                    e2_e ( CallExpr (
                        e_onboard_A,
                        [
                            e_local_a;
                            e_local_p
                        ]
                    ))
                ));
                auto_fplace(IfStmt(
                    e_local_flag,
                    auto_fplace (ExpressionStmt (e2_e(CallExpr (
                        e2var (Atom.builtin "add2dict"),
                        [
                            e_this_onboarded_activations;
                            aid_of fplace e_local_a;
                            e_local_a
                        ]
                    )))),
                    None
                ));

                auto_fplace (ExpressionStmt (
                    e2_e (CallExpr (
                        e2var (Atom.builtin "fire"),
                        [
                            e2var local_s3;
                            e_local_flag 
                        ]
                    ))
                ))
            ])
        }

    (**
        - states and port related to onboarding
        - per schema onboarding policy
            - programer-defined onboard method -> have been injected earlier when inlining base interceptor into interceptor
            - default onboard method
        - main onboard callback
    *)
    let generate_onboard_block base_interceptor interceptor_info : interceptor_info * component_item list = 
        (*** Collect intells ***)
        let onboard_methods = extract_onboard_methods (methods_of base_interceptor) in
        let onboard_index = generate_onboard_index interceptor_info onboard_methods in

        (*** States and port ***)
        assert( interceptor_info.b_onboard_state = None );
        let interceptor_info = {interceptor_info with
            b_onboard_state = Some (Atom.fresh "b_onboard")
        } in

        let this_b_onboard = Option.get interceptor_info.b_onboard_state in 
        let e_this_b_onboard = e2_e (AccessExpr (
                e2_e This, 
                e2var this_b_onboard
            )) in
        let statedef_b_onboard = auto_fplace (auto_plgannot(State (auto_fplace {
            ghost = false;
            type0 = interceptor_info.onboard_info.b_onboard_mt;
            name = this_b_onboard;
            body = Some (e2_lit VoidLit); (* set by constructor *)
        }))) in

        let this_onboarded_activations = Atom.fresh "onboarded_activations" in 
        assert( interceptor_info.this_onboarded_activations = None );
        let interceptor_info = {interceptor_info with
            this_onboarded_activations = Some (this_onboarded_activations)
        } in

        let e_this_onboarded_activations = e2_e (AccessExpr (
            e2_e This, 
            e2var this_onboarded_activations
        )) in
        let statedef_onboarded_activations = auto_fplace (auto_plgannot(State (auto_fplace ({
            ghost = false;
            type0 = mtype_of_ct (TDict (mtype_of_ft TActivationID, mtype_of_ct (TActivationRef (mt_internals_of fplace (Atom.Set.to_list interceptor_info.intercepted_schemas)))) );
            name = this_onboarded_activations;
            body = Some ( e2_e (Block2Expr (Dict, [])));
        })))) in

        let callback_onboard = Atom.fresh "onboard" in
        let port_onboard = Atom.fresh "port_onboard" in
        let port_onboard_def = auto_fplace (auto_plgannot(Inport (auto_fplace (
            {
                name = port_onboard;
                expecting_st = mtype_of_st (dual interceptor_info.onboard_info.st_onboard).value; 
                callback = e2_e (AccessExpr (
                    e2_e This, 
                    e2var callback_onboard
                ));
                _disable_session = false;
                _children = [];
                _is_intermediate = false;
                _receive_id = None;
            },
            auto_fplace EmptyMainType
        )))) in

        (*** Generate default_onboard ***)
        let default_onboard = Atom.fresh "default_onboard" in
        let default_onboard_def = auto_fplace (auto_plgannot(Method (auto_fplace {
            annotations = [];
            ghost = false;
            ret_type = mtype_of_ft TBool;
            name = default_onboard;
            args = [
                auto_fplace (mtype_of_ct (TActivationRef (mtype_of_ft TWildcard)), Atom.fresh "a");
                auto_fplace (mtype_of_ft TPlace, Atom.fresh "p_of_a")
            ];
            on_destroy = false;
            on_startup = false;
            contract_opt = None;
            body = [
                auto_fplace (ReturnStmt (
                    e2_lit (BoolLit true)
                ))
            ]
        }))) in


        (*** Main callback ***)
        let param_schema, e_param_schema = e_param_of "schema" in
        let param_s, e_param_s = e_param_of "s" in


        let callback_onboard_def = auto_fplace (auto_plgannot(Method (auto_fplace {
            annotations = [];
            ghost = false;
            ret_type = mtype_of_ct (TResult (mtype_of_ft TVoid, mtype_of_var (Atom.builtin "error")));
            name = callback_onboard;
            args = [
                auto_fplace (mtype_of_ft TBLabel, param_schema);
                auto_fplace (mtype_of_st (dual interceptor_info.onboard_info.st_onboard).value, param_s)
            ];
            on_destroy = false;
            on_startup = false;
            contract_opt = None;
            body = [auto_fplace(BranchStmt {
                s = e_param_s;
                label_opt = Some e_param_schema;
                branches = List.map (generate_main_callback_branch interceptor_info default_onboard onboard_index (e_this_b_onboard, e_this_onboarded_activations) ((param_schema, e_param_schema), (param_s, e_param_s))) (Atom.Set.to_list interceptor_info.intercepted_schemas)
            })]
        }))) in

        { interceptor_info with
            this_port_onboard = Some port_onboard
        }, [
            auto_fplace (auto_plgannot(Term (auto_fplace (auto_plgannot(Comments
                (auto_fplace(DocComment "******************** Onboarding Block ********************"))
            )))));
            statedef_b_onboard;
            statedef_onboarded_activations;
            port_onboard_def;
            default_onboard_def;
            callback_onboard_def;
        ]

    (*************** Step 2 - Onstartup and inline other base component citems ******************)

    (**
        @return an hydrated copy of interceptor_info
    *)
    let include_base_citems interceptor_info (base_interceptor : component_structure) : interceptor_info * component_item list = 

        (*** Add states to store in/out bridges and onboarding bridge ***)
        
        (* Hydrate inout_statebridges *)
        assert( interceptor_info.inout_statebridges_info = None);
        let interceptor_info = {interceptor_info with 
            inout_statebridges_info = Some (List.mapi ( fun i (b_intercepted, _, b_mt) -> 
                logger#warning ">><< %d" i;
                Atom.fresh ("b_out_"^(string_of_int i)^"_"),
                Atom.fresh ("b_in_"^(string_of_int i)^"_"),
                b_mt
            )  interceptor_info.inout_bridges_info);
        }
        in


        let inout_bridges_states = List.flatten (List.map ( function (b_out, b_in, b_mt) ->
            [ 
                auto_fplace (auto_plgannot(State (auto_fplace ({
                    ghost = false;
                    type0 = b_mt;
                    name = b_in;
                    body = Some (e2_lit VoidLit)
                } ))));
                auto_fplace (auto_plgannot(State (auto_fplace ({
                    ghost = false;
                    type0 = b_mt;
                    name = b_out;
                    body = Some (e2_lit VoidLit)
                } ))))
            ]
        ) (Option.get interceptor_info.inout_statebridges_info)) in

        (*** Retrun citems ***)
        interceptor_info,  base_interceptor.body@inout_bridges_states

    (*************** Step 1.C network infrastructure *********************)
    (* port_name, port_def, (this_bridge_out, this_bridge_in) *)
    let used_outports : (string, Atom.atom * _outport) Hashtbl.t = Hashtbl.create 16
    let used_inports :  (string, Atom.atom * _port) Hashtbl.t = Hashtbl.create 16


    let get_init_outport b_intercepted interface =
        let key_outport = Printf.sprintf "%s_outport_%s__%d_" (Atom.to_string b_intercepted) interface 0 in
        
        Option.map fst (Hashtbl.find_opt used_outports key_outport)

    (*
        1 => an intecepted activation can not initiate a session on the bridge
        2 => an intecepted activation can not listen on session creation on the bridge
        3 => an intercepted activation can initiate or listen 
    *)
    let kind_of_intercepted_bridge interceptor_info = function
    | {value=CType {value = TBridge tbridge}} -> begin
        let right = tbridge.out_type in
        let left = tbridge.in_type in

        let flag1 = List.fold_left (fun flag schema -> 
            flag || ( TypingUtils.is_subtype (mtype_of_cvar schema) left) 
        ) false (Atom.Set.to_list interceptor_info.intercepted_schemas) in
        let flag2 = List.fold_left (fun flag schema -> 
            flag || (TypingUtils.is_subtype (mtype_of_cvar schema) right) 
        ) false (Atom.Set.to_list interceptor_info.intercepted_schemas) in

        match flag1, flag2 with
        | false, true   -> 1 
        | true, false   -> 2
        | true, true    -> 3
        | false, false  -> raise (Error.DeadbranchError "intercepted bridge must have a bridge type!")
    end
    | _ -> raise (Error.DeadbranchError "intercepted bridge must have a bridge type!")
    let generate_network_infrastructure interceptor_info =
        let intercepted_bridges = interceptor_info.inout_bridges_info in

        let aux (b_intercepted, _, tb_intercepted) = 
            let st_intercepted = match tb_intercepted.value with
                | CType {value=TBridge {protocol}} -> begin
                    match protocol.value with
                    | SType st -> st
                end
            in

            let st_out, st_in = 
                let dst = (dual st_intercepted) in
                match (IRMisc.unfold_st_star st_intercepted).value with
                | STBranch _ | STRecv _ ->  dst, st_intercepted 
                | _ -> st_intercepted, dst
            in

            (* interface \in [egress, ingress]*)
            let unroll_ports interface i st_stage =
                match st_stage with
                | STBranch _ | STRecv _ ->
                    let name = Atom.fresh (Printf.sprintf "%s_inport_%s__%d_" (Atom.to_string b_intercepted) interface i) in
                    Some (
                        interface,
                        name,
                        Some {
                            name = name;
                            expecting_st = mtype_of_st st_stage;
                            callback = e2_lit VoidLit; (* will be hydrated later on*)
                            _disable_session = false;
                            _children = [];
                            _is_intermediate = false;
                            _receive_id = None;
                        },
                        None
                    )
                | STSelect _ | STSend _ when i == 0 ->
                    let name = Atom.fresh (Printf.sprintf "%s_outport_%s__%d_" (Atom.to_string b_intercepted) interface i) in
                    Some (
                        interface,
                        name,
                        None,
                        Some {
                            name = name;
                            protocol = mtype_of_st st_stage;
                            _children = [];
                        }
                    )
                | STSelect _ | STSend _ when i > 0 -> None
            in

            let aux st_egress_0 st_ingress_0 = 
                let egress_ports = List.mapi (unroll_ports "egress") (stages_of_st st_egress_0) in
                let ingress_ports = List.mapi (unroll_ports "ingress")  (stages_of_st st_ingress_0) in

                let egress_ports = List.filter_map Fun.id egress_ports in
                let ingress_ports = List.filter_map Fun.id ingress_ports in

                List.iter (function (interface,name,in_opt,out_opt) -> 
                    logger#info "~- %s" (Atom.to_string name);    
                    begin
                    match in_opt with
                    | Some port ->
                        Hashtbl.add used_inports (Atom.hint name) (name,port) 
                    | None -> ()
                    end;
                    begin
                    match out_opt with
                    | Some port ->
                        Hashtbl.add used_outports (Atom.hint name) (name,port) 
                    | None -> ()
                    end;
                    ()
                ) (ingress_ports@egress_ports);
                ()
            in


            match kind_of_intercepted_bridge interceptor_info tb_intercepted with
            | 1 -> begin
                (* stage_0 egress => in /\ stage_0 ingress => out *) 
                let st_egress_0, st_ingress_0 = st_in, st_out in 
                aux st_egress_0 st_ingress_0
            end
            | 2 -> begin
            (* stage_0 egress => out /\ stage_0 ingress => in *) 
                let st_egress_0, st_ingress_0 = st_out, st_in in 
                aux st_egress_0 st_ingress_0
            end
            | 3 -> begin
                aux st_out st_in;
                aux st_in st_out
            end
        in

        List.map aux intercepted_bridges


        
    (*************** Step 2 - Interception session block******************)
    let generate_sessions_block interceptor_info : interceptor_info * component_item list = 
        let this_4external2internal = Atom.fresh "convert_4external2internal" in
        let this_4internal2external = Atom.fresh "convert_4internal2external" in
        let this_4external = Atom.fresh "sessions_4ext" in
        let this_4internal = Atom.fresh "sessions_4int" in

        assert(interceptor_info.sessions_info = None);
        let interceptor_info = {interceptor_info with
            sessions_info = Some { 
                this_4external2internal;
                this_4internal2external; this_4external; 
                this_4internal; 
            }
        } in

        interceptor_info, [
            auto_fplace (auto_plgannot(Term (auto_fplace (auto_plgannot(Comments
                (auto_fplace(DocComment "******************** Intercepted Sessions Handling Block ********************"))
            )))));

            auto_fplace (auto_plgannot(State (auto_fplace ({
                ghost = false;
                type0 = mtype_of_ct (TDict (mtype_of_ft TSessionID, mtype_of_ft TSessionID));
                name = this_4external2internal;
                body = Some (e2_e (Block2Expr (Dict, [])));
            }))));
            auto_fplace (auto_plgannot(State (auto_fplace ({
                ghost = false;
                type0 = mtype_of_ct (TDict (mtype_of_ft TSessionID, mtype_of_ft TSessionID));
                name = this_4internal2external;
                body = Some (e2_e (Block2Expr (Dict, [])));
            }))));
            auto_fplace (auto_plgannot(State (auto_fplace ({
                ghost = false;
                type0 = mtype_of_ct (TDict (mtype_of_ft TSessionID, mtype_of_st STWildcard));
                name = this_4external;
                body = Some (e2_e (Block2Expr (Dict, [])));
            }))));
            auto_fplace (auto_plgannot(State (auto_fplace ({
                ghost = false;
                type0 = mtype_of_ct (TDict (mtype_of_ft TSessionID, mtype_of_st STWildcard));
                name = this_4internal;
                body = Some (e2_e (Block2Expr (Dict, [])));
            }))));
        ]

    (*************** Ingress Egress utils ******************)

    let filter_session_interceptors flag_anonymous = 
        List.filter (function (m:method0) ->
            List.exists (function | SessionInterceptor annot -> annot.anonymous = flag_anonymous | _ -> false ) m.value.annotations 
        )

    let paired_interceptor_stage (left_mt, right_mt, st_continuation, tmsg) (mt_A, mt_B, st3, tmsg3)=
        TypingUtils.is_subtype left_mt  mt_A
        && TypingUtils.is_subtype right_mt mt_B
        && TypingUtils.is_subtype (mtype_of_st st_continuation.value) (mtype_of_st st3.value)
        && TypingUtils.is_subtype tmsg tmsg3  

    (**
        @return if exists the msginterceptor function for [intercepted_bridge] at stage [i]
    *)
    let get_msginterceptor interceptor_info (msg_interceptors: method0 list) tb_intercepted_bridge i (tmsg, st_continuation) =
        logger#info"<> get_msginterceptor";
        let left_mt = tb_intercepted_bridge.in_type in
        let right_mt = tb_intercepted_bridge.out_type in

        let filter (m: method0) =
            (* Well-formedness of msginterceptor should have been checked during typechecking *)
            let [param_from; param_to; param_continuation_in; param_continuation_out; param_msg] = m.value.args in


            let mt_A = mt_activation_2_mt_schema (fst param_from.value) in
            let mt_B = mt_activation_2_mt_schema (fst param_to.value) in
            let st3 = match fst param_continuation_in.value with | {value=SType st} -> st in (* type of continuation_in*)
            let tmsg3 = fst param_msg.value in 

            logger#info "try paired msginterceptor [%s] with %b \n\t%s \n<\n\t %s\nAND\n\t%s \n<\n\t %s\nAND\n\t%s \n<\n\t %s\nAND\n\t%s \n<\n\t %s"  
                (Atom.to_string m.value.name) 
                (TypingUtils.is_subtype (mtype_of_st st_continuation.value) (mtype_of_st st3.value))
                (show_main_type left_mt) (show_main_type mt_A)
                (show_main_type right_mt) (show_main_type mt_B)
                (show_session_type st_continuation) (show_session_type st3)
                (show_main_type tmsg) (show_main_type tmsg3);

            paired_interceptor_stage (left_mt, right_mt, st_continuation, tmsg) (mt_A, mt_B, st3, tmsg3)
        in 

        (*
        (* DEBUG *)
        List.iter (function (m:method0) -> 
            logger#info "> msginterceptor %s" (Atom.to_string m.value.name)
        ) msg_interceptors;
        *)

        match List.filter filter msg_interceptors with
        | [] -> None
        | [ m ] -> Some m.value.name
        | ms -> Error.perror interceptor_info.base_interceptor_place "Multiple msg interceptors are defined for the same msg (+ context) in %s" (Atom.to_string interceptor_info.base_interceptor_name)

    let get_sessioninterceptor_anon  interceptor_info (session_interceptors: method0 list) tb_intercepted_bridge i (tmsg, st_continuation) = 
        let left_mt = tb_intercepted_bridge.in_type in
        let right_mt = tb_intercepted_bridge.out_type in

        let filter (m: method0) =
            (* Well-formedness of sessioninterceptor should have been checked during typechecking *)
            let [onboarded_activations; param_from; param_b_other_side; _; param_msg] = m.value.args in

            let mt_A = mt_activation_2_mt_schema (fst param_from.value) in
            (* Loss of precesion compare to non anonymous case 
                mt_Bs is overapproximated using the in_type of b_other_side param
            *)
            let mt_Bs = match (fst param_b_other_side.value).value with
                | CType{ value = TBridge tb } -> tb.out_type (* see whitepaper *)
            in
            let st3 = st_continuation in
            let tmsg3 = fst param_msg.value in 

            logger#error "trying to pair session_anon callback for \n\t%s < %s" 
                (show_main_type right_mt)
                (show_main_type mt_Bs)
            ;

            paired_interceptor_stage (left_mt, right_mt, st_continuation, tmsg) (mt_A, mt_Bs, st3, tmsg3)
        in 

        let session_interceptors = filter_session_interceptors true session_interceptors in

        match List.filter filter session_interceptors with
        | [] -> None
        | [ m ] -> Some m.value.name
        | ms -> Error.perror interceptor_info.base_interceptor_place "Multiple session interceptors, for non anonymous case, are defined for the same msg (+ context) in %s" (Atom.to_string interceptor_info.base_interceptor_name)

    let get_sessioninterceptor_not_anon interceptor_info (session_interceptors: method0 list) tb_intercepted_bridge i (tmsg, st_continuation) = 
        let left_mt = tb_intercepted_bridge.in_type in
        let right_mt = tb_intercepted_bridge.out_type in

        let filter (m: method0) =
            (* Well-formedness of msginterceptor should have been checked during typechecking *)
            let [onboarded_activations; param_from; param_b_other_side; param_to; param_msg] = m.value.args in

            let mt_A = mt_activation_2_mt_schema (fst param_from.value) in
            let mt_B = mt_activation_2_mt_schema (fst param_to.value) in
            let st3 = st_continuation in
            let tmsg3 = fst param_msg.value in 

            paired_interceptor_stage (left_mt, right_mt, st_continuation, tmsg) (mt_A, mt_B, st3, tmsg3)
        in 

        let session_interceptors = filter_session_interceptors false session_interceptors in

        match List.filter filter session_interceptors with
        | [] -> None
        | [ m ] -> Some m.value.name 
        | ms -> Error.perror interceptor_info.base_interceptor_place "Multiple session interceptors, for non anonymous case, are defined for the same msg (+ context) in %s" (Atom.to_string interceptor_info.base_interceptor_name)

    (* flag =true if egress else ingress *)
    let generate_skeleton_callback_msg (flag_egress, aux_ongoing__e_skeleton_s_out, aux_ongoing__es_skeleton_update_metadata) interceptor_info msg_interceptors (b_intercepted, tb_intercepted) i tmsg st_continuation = 
        let param_msg, e_param_msg = e_param_of "msg" in
        let param_s_in, e_param_s_in = e_param_of "s_in" in

        logger#info "generate_skeleton_callback_msg %s" (Atom.to_string b_intercepted);
       (* begin
            (* Sanity check *)
            match st_continuation.value with 
            | STRecv _ | STBranch _ when flag_egress -> ()
            | _ when flag_egress -> raise (Error.DeadbranchError (Printf.sprintf "generate_skeleton_callback_msg wrong continuation type for %s: duality error" (Atom.to_string b_intercepted)))
            | _ -> ()
        end;
*)

        let local_from, e_local_from = e_param_of "from" in
        let local_to, e_local_to = e_param_of "to" in
        let local_s_out, e_local_s_out = e_param_of "s_out_msg" in
        let local_s_out2, e_local_s_out2 = e_param_of "s_out_next" in

        let mt_out, mt_out2 = 
            match tmsg.value with
            | CType{ value = TFlatType TBLabel} -> 
                (* select*)
                let mt_out2 = dual st_continuation in
                (* TODO fixme, the first returned type should be {b1:st1; ... bn:stn} not just st_i for sti=st_continuation *)
                mtype_of_st st_continuation.value,  mtype_of_st mt_out2.value  
            | _ -> (* fire *)
                let mt_out2 = dual st_continuation in
                mtype_of_st (STSend (tmsg, mt_out2)), mtype_of_st mt_out2.value 
        in 



        let e_res_msginterceptor = 
            let log_token = Printf.sprintf "%s_callback_ongoing__%s__%d_" (if flag_egress then "egress" else "ingress") (Atom.to_string b_intercepted) i in
            logger#info "searching message interceptor for %s" log_token;

            match (get_msginterceptor interceptor_info msg_interceptors tb_intercepted i (tmsg, st_continuation)) with
            | Some x_to -> 
                logger#warning "Some @msginterceptor(...) for bridge type %s" log_token;
                e2_e (CallExpr (
                    e2_e (AccessExpr ( 
                        e2_e This, 
                        e2var x_to
                    )),
                    [
                        e_local_from;
                        e_local_to;
                        e_param_s_in;
                        e_local_s_out;
                        e_param_msg;
                    ]
                ))
            | None -> begin 
                (* Case there is no user defined function *)
                logger#warning "No @msginterceptor(...) for bridge type %s" log_token;
                match tmsg.value with
                | CType {value=TFlatType TBLabel} ->
                    e2_e (CallExpr (
                        e2var (Atom.builtin "select"),
                        [
                            e_local_s_out;
                            e_param_msg;
                        ]
                    )) 
                | _ -> 
                    e2_e (CallExpr (
                        e2var (Atom.builtin "fire"),
                        [
                            e_local_s_out;
                            e_param_msg;
                        ]
                    )) 
            end
        in

        let left_mt = tb_intercepted.in_type in
        let right_mt = tb_intercepted.out_type in

        let sessions_info = Option.get interceptor_info.sessions_info in


        let e_exist_sout, e_get_sout = aux_ongoing__e_skeleton_s_out sessions_info e_param_s_in in

        auto_fplace {
            annotations = [];
            ghost = false;
            ret_type = mtype_of_ct (TResult (mtype_of_ft TVoid, mtype_of_var (Atom.builtin "error")));
            name = Atom.fresh (Printf.sprintf "%s_callback_ongoing__%s__%d_" (if flag_egress then "egress" else "ingress") (Atom.to_string b_intercepted) i);
            args = [
                auto_fplace (tmsg, param_msg);
                auto_fplace (mtype_of_st st_continuation.value, param_s_in);
            ];
            body = [
                auto_fplace(IfStmt( (* FIXME TODO currently an unregistered session is dropped -> e.g. loadbalancer example for put/delete on other shards *)
                    e_exist_sout,
                    auto_fplace(BlockStmt ([
                        (*** Computing from, to and s_out ***)
                        auto_fplace (LetStmt(
                            mt_out,
                            local_s_out,
                            e2_e(CastExpr(
                                mt_out,
                                e_get_sout))
                        ));

                        auto_fplace (LetStmt(
                            mtype_of_ct (TActivationRef left_mt),
                            local_from,
                            e2_e (CallExpr (
                                e2var (Atom.builtin "session_from"),
                                [ e_param_s_in ]
                            ))
                        ));
                        auto_fplace (LetStmt(
                            mtype_of_ct (TActivationRef right_mt),
                            local_to,
                            e2_e (CallExpr (
                                e2var (Atom.builtin "session_to"),
                                [ e2var local_s_out ]
                            ))
                        ));

                        (* TODO assert  ... *)

                        (*** Apply msg interceptor ***)
                        auto_fplace (LetStmt(
                            mt_out2,
                            (*mtype_of_ct (TResult (mt_out2, mtype_of_var (Atom.builtin "error"))),*)
                            local_s_out2,
                            e2_e (UnopExpr (UnpackOrPropagateResult, e_res_msginterceptor))
                        ));
                    ]
                    (*** Update metadata ***)
                    @ (aux_ongoing__es_skeleton_update_metadata sessions_info e_param_s_in e_local_s_out2)
                    )),
                    None))
            ]
            ;
            contract_opt = None;
            on_destroy = false;
            on_startup = false;
        }

    let generate_skeleton_callback_sessioninit (flag_egress, aux_sessioninit__e_skeleton_establishing_s_out, aux_sessioninit__es_skeleton_update_metadata) interceptor_info msg_interceptors session_interceptors b_intercepted tb_intercepted   this_outport_init_opt this_b_out this_b_in i this_callback_msg tmsg st_continuation = 
        let param_msg, e_param_msg = e_param_of "msg" in
        let param_s_in, e_param_s_in = e_param_of "s_in" in

        let local_from, e_local_from = e_param_of "from" in
        let local_to_opt, e_local_to_opt = e_param_of "to_opt" in
        let local_to_schema, e_local_to_schema = e_param_of "to_schema" in
        let local_to, e_local_to = e_param_of "to" in
        let local_s_out, e_local_s_out = e_param_of "s_out_init" in


        let interceptor_by_schema = get_sessioninterceptor_anon interceptor_info session_interceptors tb_intercepted i (tmsg, st_continuation) in 
        let interceptor_by_activation = get_sessioninterceptor_not_anon interceptor_info session_interceptors tb_intercepted i (tmsg, st_continuation) in

        let e_this_onboarded_activations = e2_e (AccessExpr ( e2_e This, e2var (Option.get interceptor_info.this_onboarded_activations))) in

        let left_mt = tb_intercepted.in_type in
        let right_mt = tb_intercepted.out_type in

        let sessions_info = Option.get interceptor_info.sessions_info in


        let anon_branching_stmts = [
            auto_fplace (LetStmt(
                (* FIXME Java does correctly handles type if specified inside two encapsulated generics *)
                mtype_of_ct (TOption (mtype_of_ct (TActivationRef right_mt))),
                local_to_opt,
                e2_e(OptionExpr None) 
            ));
            auto_fplace(IfStmt(
                e2_e (CallExpr (
                    e2var (Atom.builtin "is_none"), 
                    [ e2_e (CallExpr ( 
                        e2var (Atom.builtin "session_to_2_"), 
                        [ e_param_s_in ]))])),
            (* Case anonymous init *)
            (
                match interceptor_by_schema with
                | None -> 
                    logger#warning "No @sessioninterceptor(true, ...) for bridge type %s" (show_tbridge tb_intercepted);
                    auto_fplace (ReturnStmt( 
                        e2_e (ResultExpr (None, Some (e2_lit (StringLit "Can not generate a default sessioninit interceptor for anonymous case"))))
                    ))
                | Some x_to ->
                    auto_fplace(AssignExpr(
                        local_to_opt,
                        e2_e (UnopExpr (UnpackOrPropagateResult,
                            e2_e (CallExpr(
                                e2_e (AccessExpr ( 
                                    e2_e This, 
                                    e2var x_to
                                )),
                                [
                                    e_this_onboarded_activations;
                                    e_local_from;
                                    e2_e(AccessExpr (e2_e This, e2var (if flag_egress then this_b_in else this_b_out))); (* bridge on the other side *)
                                    e2_e (CallExpr( 
                                        e2var (Atom.builtin "schemaof"),
                                        [ 
                                            e2_e (CallExpr (
                                                e2var (Atom.builtin "session_to"),
                                                [ e_param_s_in ]
                                            ))
                                        ]
                                    ));
                                    e_param_msg;
                                ]
                            ))
                        ))
                    ))
            )
            ,
            (* Case non anonymous init *)
            Some(
                match interceptor_by_activation with
                | None ->
                    (* Case there is no user defined function *)
                    logger#warning "No @sessioninterceptor(false, ...) for bridge type %s" (show_tbridge tb_intercepted);

                    auto_fplace(AssignExpr(
                        local_to_opt,
                        e2_e (CallExpr ( 
                            e2var (Atom.builtin "session_to_2_"), [ e_param_s_in ])) (* by default, use the requested to *)
                    ))
                | Some x_to ->
                    auto_fplace(AssignExpr(
                        local_to_opt,
                        e2_e (UnopExpr (UnpackOrPropagateResult,
                            e2_e (CallExpr(
                                e2_e (AccessExpr ( 
                                    e2_e This, 
                                    e2var x_to
                                )),
                                [
                                    e_this_onboarded_activations;
                                    e_local_from;
                                    e2_e(AccessExpr (e2_e This, e2var (if flag_egress then this_b_in else this_b_out))); (* bridge on the other side *)
                                    e2_e (CastExpr( (* This fix a java type inference limitation *)
                                        mtype_of_ct (TActivationRef right_mt),
                                        e2_e (CallExpr( 
                                            e2var (Atom.builtin "option_get"),
                                            [ 
                                                e2_e (CallExpr (
                                                    e2var (Atom.builtin "session_to_2_"),
                                                    [ e_param_s_in ]
                                                ))
                                            ]
                                        ))
                                    ));
                                    e_param_msg;
                                ]
                            ))
                        ))
                    ))
            )
        )) 
        ] in

        auto_fplace {
            annotations = [];
            ghost = false;
            ret_type = mtype_of_ct (TResult (mtype_of_ft TVoid, mtype_of_var (Atom.builtin "error")));
            name = Atom.fresh (Printf.sprintf "%s_callback_sessioninit__%s__%d_" (if flag_egress then "egress" else "ingress") (Atom.to_string b_intercepted) i);
            args = [
                auto_fplace (tmsg, param_msg);
                auto_fplace (mtype_of_st st_continuation.value, param_s_in);
            ];
            body = [
                (* TODO assert(s_in.to_2_ != None ); *)

                (*** Computing from and to ***)
                auto_fplace (LetStmt(
                    mtype_of_ct (TActivationRef left_mt),
                    local_from,
                    e2_e (CallExpr (
                        e2var (Atom.builtin "session_from"),
                        [ e_param_s_in ]
                    ))
                ))
            ] @ anon_branching_stmts @ [
                (* TODO assert ... *)
                auto_fplace(IfStmt(
                    e2_e(BinopExpr( e_local_to_opt, Equal, e2_e (OptionExpr None))),     
                    auto_fplace (BlockStmt [
                        auto_fplace(ExpressionStmt (e2_e (CallExpr(
                            e2var (Atom.builtin "print"),
                            [ e2_lit (StringLit (Printf.sprintf "%s request refused!" (if flag_egress then "Egress" else "Ingress"))) ]))));
                        auto_fplace (ReturnStmt (e2_lit VoidLit))
                    ]),
                    None
                ));
                auto_fplace (LetStmt(
                    mtype_of_ct (TActivationRef right_mt),
                    local_to,
                    e2_e(CastExpr(
                        mtype_of_ct (TActivationRef right_mt),
                        e2_e (CallExpr( 
                            e2var (Atom.builtin "option_get"),
                            [ e_local_to_opt ]
                        ))
                    ))
                ));

                (*** Establishing s_out ***)
                auto_fplace(LetStmt(
                    tb_intercepted.protocol,
                    local_s_out,
                    e2_e(CastExpr(
                        tb_intercepted.protocol, (* FIXME Java compiler does infer the type conversion*)
                        aux_sessioninit__e_skeleton_establishing_s_out  this_outport_init_opt e_local_to)))); 
            ]

            (*** Updating metdata ***)
            @ (aux_sessioninit__es_skeleton_update_metadata sessions_info e_param_s_in e_local_s_out)

            @ [
                (*** Process the first message ***)
                auto_fplace (ReturnStmt(
                    e2_e (CallExpr (
                        e2_e (AccessExpr(
                            e2_e This,
                            e2var this_callback_msg 
                        )),
                        [
                            e_param_msg;
                            e_param_s_in;
                        ]
                    ))
                ));
            ];
            contract_opt = None;
            on_destroy = false;
            on_startup = false;
        }

    (**
        @param i - n° of the stage. 0 == session init 
    *)
    let generate_skeleton_block_per_intercepted_bridge_per_st_stage  (flag_egress, generate_skeleton_callback_msg, generate_skeleton_callback_sessioninit) interceptor_info msg_interceptors session_interceptors b_intercepted tb_intercepted  this_outport_init_opt this_b_out this_b_in i st_stage = 
        logger#warning "generate_skeleton_block_per_intercepted_bridge_per_st_stage for %s - %d:\n\t-%s\nt\t-%s\n\t-%s" 
            (if flag_egress then "egress" else "ingress")
            i
            (show__session_type st_stage)
            ;
        let e_this_b_out = e2_e (AccessExpr (e2_e This, e2var this_b_out)) in
        let e_this_b_in = e2_e (AccessExpr (e2_e This, e2var this_b_in)) in

        (*** Callback names ***)
        let callback_name = Atom.fresh (Printf.sprintf "callback_%s__%s__%d_" (if flag_egress then "egress" else "ingress") (Atom.to_string b_intercepted) i) in

        (***Inport & Outport generation ***)
        let interface = if flag_egress then "egress" else "ingress" in
        let key_inport = Printf.sprintf "%s_inport_%s__%d_" (Atom.to_string b_intercepted) interface i in 
        let key_outport = Printf.sprintf "%s_outport_%s__%d_" (Atom.to_string b_intercepted) interface i in

        logger#info "generate_skeleton_block_per_intercepted_bridge_per_st_stage";
        
        Seq.iter (function x -> logger#error "glup %s => %s" key_outport x) (Hashtbl.to_seq_keys used_outports);
        Seq.iter (function x -> logger#error "glups %s => %s" key_inport x) (Hashtbl.to_seq_keys used_inports);

        (* Maj inport with callback *)
        begin
            match Hashtbl.find_opt used_inports key_inport with 
            | None -> [] 
            | Some (name, inport) -> begin
                logger#info "maj callback for  %s" (Atom.to_string name);     
                begin
                    match fst inport.callback.value with
                    | LitExpr _ -> ()
                    | _ -> raise (Error.DeadbranchError (Printf.sprintf "callback for %s has already been updated" key_inport))
                end;
                let inport = { inport with callback = 
                    e2_e (AccessExpr(
                        e2_e This,
                        e2var callback_name
                    ));
                } in
                Hashtbl.replace used_inports key_inport (name, inport);



                let (tmsg, st_continuation) : main_type * session_type = msgcont_of_st (auto_fplace st_stage) in
                (* Correct st_continuation *)
                let st_continuation = match tmsg.value with
                    | CType{value=TFlatType TBLabel} -> dual st_continuation
                    | _ -> st_continuation
                in

                (*** Msg interception callback ***)
                let callback_msg : method0 = generate_skeleton_callback_msg interceptor_info msg_interceptors (b_intercepted, tb_intercepted) i tmsg st_continuation in

                (*** Session interception callback ***)
                let callback_session_init : method0 option = 
                    if i = 0 then 
                        match this_outport_init_opt with
                        | None -> None (* this interface does not have outport for this bridge*)
                        | Some this_outport_init ->
                        Some (generate_skeleton_callback_sessioninit interceptor_info msg_interceptors session_interceptors b_intercepted tb_intercepted this_outport_init this_b_out this_b_in i callback_msg.value.name tmsg st_continuation)
                    else None 
                in

                logger#error "generating callback for %s" key_inport;
                (*** Main callback ***)
                let param_msg, e_param_msg= e_param_of "msg" in
                let param_s_in, e_param_s_in = e_param_of "s_in" in
                let callback : method0 = 
                    if i = 0 then 
                        auto_fplace {
                            annotations = [];
                            ghost = false;
                            ret_type = mtype_of_ct (TResult (mtype_of_ft TVoid, mtype_of_var (Atom.builtin "error")));
                            name = callback_name;
                            args = [
                                auto_fplace (tmsg, param_msg);
                                auto_fplace (mtype_of_st st_continuation.value, param_s_in);
                            ];
                            body = [
                                auto_fplace (IfStmt(
                                    e2_e (CallExpr( 
                                        e2var (Atom.builtin "is_init_stage"),
                                        [ e_param_s_in ]
                                    )),
                                    auto_fplace (ReturnStmt (
                                        e2_e (CallExpr(
                                            e2_e (AccessExpr(
                                                e2_e This,
                                                e2var (Option.get callback_session_init).value.name 
                                            )),
                                            [
                                                e_param_msg;
                                                e_param_s_in
                                            ]
                                        ))
                                    )),
                                    Some (auto_fplace (ReturnStmt (
                                        e2_e (CallExpr(
                                            e2_e (AccessExpr(
                                                e2_e This,
                                                e2var callback_msg.value.name 
                                            )),
                                            [
                                                e_param_msg;
                                                e_param_s_in
                                            ]
                                        ))
                                    )))
                                ))
                            ];
                            contract_opt = None;
                            on_destroy = false;
                            on_startup = false;
                        }
                    else 
                        auto_fplace { callback_msg.value with name = callback_name } 
                in

                [
                    auto_fplace (auto_plgannot(Method callback));
                ]
                @ (if i = 0 then
                    [
                        auto_fplace (auto_plgannot(Method (Option.get callback_session_init)));
                        auto_fplace (auto_plgannot(Method callback_msg));
                    ]
                else [])
            end
        end


    let generate_skeleton_block_per_intercepted_bridge (flag_egress, generate_skeleton_block_per_intercepted_bridge) interceptor_info msg_interceptors session_interceptors b_intercepted this_outport_init_opt this_b_out this_b_in b_mt : component_item list = 
        let tb_intercepted = (match b_mt with | {value = CType {value = TBridge tb}} -> tb) in
        let p_st = (match tb_intercepted.protocol with 
            | {value = SType st} -> st
            | _ -> failwith (Printf.sprintf "TODO resolve type aliasing using an external fct or requires that type aliasing should have been eliminated before using [generate_%s_block_per_intercepted_bridge]" (if flag_egress then "egress" else "ingress"))
        ) in
        let st_stages = stages_of_st p_st in

        List.flatten (
            List.mapi
                (generate_skeleton_block_per_intercepted_bridge interceptor_info msg_interceptors session_interceptors b_intercepted tb_intercepted this_outport_init_opt this_b_out this_b_in )
                st_stages
        )

    (*************** Step 3 - Ingress generation ******************)


    let aux_ongoing__e_ingress_s_out sessions_info e_param_s_in = 
        let e_sid = 
            e2_e (CallExpr( 
                e2var (Atom.builtin "get2dict"),
                [ 
                    e2_e (AccessExpr (e2_e This, e2var sessions_info.this_4internal2external));
                    e2_e (CallExpr (
                        e2var (Atom.builtin "sessionid"),
                        [ e_param_s_in ]
                    ))
                ]
            ))
        in

        e2_e (CallExpr( 
            e2var (Atom.builtin "exist2dict"),
            [ 
                e2_e (AccessExpr (e2_e This, e2var sessions_info.this_4external));
                e_sid;
            ]
        )),
        e2_e (CallExpr( 
            e2var (Atom.builtin "get2dict"),
            [ 
                e2_e (AccessExpr (e2_e This, e2var sessions_info.this_4external));
                e_sid;
            ]
        ))

    let aux_ongoing__es_ingress_update_metadata sessions_info e_param_s_in e_local_s_out2 = 
        [
            auto_fplace (ExpressionStmt (e2_e(CallExpr(
                e2var (Atom.builtin "add2dict"),
                [
                    e2_e (AccessExpr (e2_e This, e2var sessions_info.this_4external));
                    e2_e (CallExpr (
                        e2var (Atom.builtin "sessionid"),
                        [ e_local_s_out2 ]
                    ));
                    e_local_s_out2
                ]
            ))));
            auto_fplace (ExpressionStmt (e2_e(CallExpr(
                e2var (Atom.builtin "add2dict"),
                [

                    e2_e (AccessExpr (e2_e This, e2var sessions_info.this_4internal));
                    e2_e (CallExpr (
                        e2var (Atom.builtin "sessionid"),
                        [ e_param_s_in ]
                    ));
                    e_param_s_in
                ]
            ))));
        ]

    let generate_ingress_callback_msg = generate_skeleton_callback_msg (false, aux_ongoing__e_ingress_s_out,aux_ongoing__es_ingress_update_metadata)
    let aux_sessioninit__e_ingress_establishing_s_out this_outport_init_opt e_local_to =  
        e2_e (CallExpr(
            e2var (Atom.builtin "initiate_session_with"),
            [
                e2_e (AccessExpr( e2_e This, e2var this_outport_init_opt));
                e_local_to;
            ]
        ))

    let aux_sessioninit__es_ingress_update_metadata sessions_info e_param_s_in e_local_s_out = 
        [
            auto_fplace (ExpressionStmt (e2_e(CallExpr(
                e2var (Atom.builtin "add2dict"),
                [
                    e2_e (AccessExpr (e2_e This, e2var sessions_info.this_4external2internal));
                    e2_e (CallExpr (
                        e2var (Atom.builtin "sessionid"),
                        [ e_local_s_out ]
                    ));
                    e2_e (CallExpr (
                        e2var (Atom.builtin "sessionid"),
                        [ e_param_s_in ]
                    ));
                ]
            ))));
            auto_fplace (ExpressionStmt (e2_e(CallExpr(
                e2var (Atom.builtin "add2dict"),
                [

                    e2_e (AccessExpr (e2_e This, e2var sessions_info.this_4internal2external));
                    e2_e (CallExpr (
                        e2var (Atom.builtin "sessionid"),
                        [ e_param_s_in ]
                    ));
                    e2_e (CallExpr (
                        e2var (Atom.builtin "sessionid"),
                        [ e_local_s_out ]
                    ));
                ]
            ))));
        ] @ aux_ongoing__es_ingress_update_metadata sessions_info e_param_s_in e_local_s_out

    let generate_ingress_callback_sessioninit =
    generate_skeleton_callback_sessioninit 
        (false, aux_sessioninit__e_ingress_establishing_s_out, aux_sessioninit__es_ingress_update_metadata) 

    let generate_ingress_block_per_intercepted_bridge_per_st_stage = 
        generate_skeleton_block_per_intercepted_bridge_per_st_stage  (false, generate_ingress_callback_msg, generate_ingress_callback_sessioninit)

    let generate_ingress_block_per_intercepted_bridge = generate_skeleton_block_per_intercepted_bridge (false, generate_ingress_block_per_intercepted_bridge_per_st_stage) 

    let generate_ingress_block interceptor_info base_interceptor : component_item list = 
        logger#warning "Generating ingress block";
        let msg_interceptors = extract_message_intercept_methods (methods_of base_interceptor) in
        let session_interceptors = extract_session_intercept_methods (methods_of base_interceptor) in

        auto_fplace (auto_plgannot(Term (auto_fplace (auto_plgannot(Comments
            (auto_fplace(DocComment "******************** Ingress Block ********************"))
        )))))
        :: List.flatten(
            List.map (function ((b_intercepted, _, b_mt), (this_b_out, this_b_in, _)) ->

                logger#warning "Generating ingress interface block for bridge [%s]" (Atom.to_string b_intercepted);

                auto_fplace (auto_plgannot(Term (auto_fplace (auto_plgannot(Comments
                    (auto_fplace(DocComment (Printf.sprintf "*** Ingress Interface Block for bridge [%s] ***" (Atom.to_string b_intercepted))))))
                )))
                :: (generate_ingress_block_per_intercepted_bridge interceptor_info  msg_interceptors session_interceptors b_intercepted (get_init_outport b_intercepted "egress") this_b_out this_b_in b_mt)  (* NB this outport, if exists, is on the dual interface *) 
            ) (List.combine interceptor_info.inout_bridges_info (Option.get interceptor_info.inout_statebridges_info)) 
        )

    (*************** Step 4 - Egress generation ******************)

    let aux_ongoing__e_egress_s_out sessions_info e_param_s_in = 
        let e_sid = 
            e2_e (CallExpr( 
                e2var (Atom.builtin "get2dict"),
                [ 
                    e2_e (AccessExpr (e2_e This, e2var sessions_info.this_4external2internal));
                    e2_e (CallExpr (
                        e2var (Atom.builtin "sessionid"),
                        [ e_param_s_in ]
                    ))
                ]
            ))
        in
        e2_e (CallExpr( 
            e2var (Atom.builtin "exist2dict"),
            [ 
                e2_e (AccessExpr (e2_e This, e2var sessions_info.this_4internal));
                e_sid;
            ]
        )),
        e2_e (CallExpr( 
            e2var (Atom.builtin "get2dict"),
            [ 
                e2_e (AccessExpr (e2_e This, e2var sessions_info.this_4internal));
                e_sid;
            ]
        ))

    let aux_ongoing__es_egress_update_metadata sessions_info e_param_s_in e_local_s_out2 = 
        [
            auto_fplace (ExpressionStmt (e2_e(CallExpr(
                e2var (Atom.builtin "add2dict"),
                [
                    e2_e (AccessExpr (e2_e This, e2var sessions_info.this_4internal));
                    e2_e (CallExpr (
                        e2var (Atom.builtin "sessionid"),
                        [ e_local_s_out2 ]
                    ));
                    e_local_s_out2
                ]
            ))));
            auto_fplace (ExpressionStmt (e2_e(CallExpr(
                e2var (Atom.builtin "add2dict"),
                [

                    e2_e (AccessExpr (e2_e This, e2var sessions_info.this_4external));
                    e2_e (CallExpr (
                        e2var (Atom.builtin "sessionid"),
                        [ e_param_s_in ]
                    ));
                    e_param_s_in
                ]
            ))));
        ]
    
    let generate_egress_callback_msg = generate_skeleton_callback_msg (true, aux_ongoing__e_egress_s_out,aux_ongoing__es_egress_update_metadata)

    let aux_sessioninit__e_egress_establishing_s_out this_port_out_egress  e_local_to =  
        e2_e (CallExpr(
            e2var (Atom.builtin "initiate_session_with"),
            [
                e2_e (AccessExpr( e2_e This, e2var this_port_out_egress));
                e_local_to;
            ]
        ))

    let aux_sessioninit__es_egress_update_metadata sessions_info e_param_s_in e_local_s_out = 
        [
            auto_fplace (ExpressionStmt (e2_e(CallExpr(
                e2var (Atom.builtin "add2dict"),
                [
                    e2_e (AccessExpr (e2_e This, e2var sessions_info.this_4internal2external));
                    e2_e (CallExpr (
                        e2var (Atom.builtin "sessionid"),
                        [ e_local_s_out ]
                    ));
                    e2_e (CallExpr (
                        e2var (Atom.builtin "sessionid"),
                        [ e_param_s_in ]
                    ));
                ]
            ))));
            auto_fplace (ExpressionStmt (e2_e(CallExpr(
                e2var (Atom.builtin "add2dict"),
                [

                    e2_e (AccessExpr (e2_e This, e2var sessions_info.this_4external2internal));
                    e2_e (CallExpr (
                        e2var (Atom.builtin "sessionid"),
                        [ e_param_s_in ]
                    ));
                    e2_e (CallExpr (
                        e2var (Atom.builtin "sessionid"),
                        [ e_local_s_out ]
                    ));
                ]
            ))));
        ] @ aux_ongoing__es_egress_update_metadata sessions_info e_param_s_in e_local_s_out

    let generate_egress_callback_sessioninit =
    generate_skeleton_callback_sessioninit 
        (true, aux_sessioninit__e_egress_establishing_s_out, aux_sessioninit__es_egress_update_metadata) 

    let generate_egress_block_per_intercepted_bridge_per_st_stage = 
        generate_skeleton_block_per_intercepted_bridge_per_st_stage  (true, generate_egress_callback_msg, generate_egress_callback_sessioninit)


    let generate_egress_block_per_intercepted_bridge = generate_skeleton_block_per_intercepted_bridge (true, generate_egress_block_per_intercepted_bridge_per_st_stage) 

    let generate_egress_block interceptor_info base_interceptor : component_item list = 
        logger#warning "Generating egress block";
        let msg_interceptors = extract_message_intercept_methods (methods_of base_interceptor) in
        let session_interceptors = extract_session_intercept_methods (methods_of base_interceptor) in

        auto_fplace (auto_plgannot(Term (auto_fplace (auto_plgannot(Comments
            (auto_fplace(DocComment "******************** Egress Block ********************"))
        )))))
        :: List.flatten(
            List.map (function ((b_intercepted, _, b_mt), (this_b_out, this_b_in, _)) ->
                logger#warning "Generating egress interface block for bridge [%s]" (Atom.to_string b_intercepted);

                auto_fplace (auto_plgannot(Term (auto_fplace (auto_plgannot(Comments
                    (auto_fplace(DocComment (Printf.sprintf "*** Egress Interface Block for bridge [%s] ***" (Atom.to_string b_intercepted))))))
                )))
                :: (generate_egress_block_per_intercepted_bridge interceptor_info  msg_interceptors session_interceptors b_intercepted (get_init_outport b_intercepted "ingress") this_b_out this_b_in b_mt) (* NB this outport, if exists, is on the dual interface *) 
            ) (List.combine interceptor_info.inout_bridges_info (Option.get interceptor_info.inout_statebridges_info)) 
        )

    (*************** Step 5 - Onstartup generation ******************)

    let generate_onstartup interceptor_info citems = 
        (*** Collect intells ***)
        let base_onstartup_opt = List.find_opt (function | {value={v=Method m}} -> m.value.on_startup | _ -> false) citems in
        let base_onstartup_opt = Option.map (function | {value={v=Method m}} -> m) base_onstartup_opt in
        let citems_wo_onstartup = List.filter (function | {value={v=Method m}} -> Bool.not m.value.on_startup | _ -> true) citems in 

        (*** Create onstartup ***)
        let param_b_onboarding, e_param_b_onboarding = e_param_of "b_onboarding" in
        let onstartup_params_inout = (List.map (function (_, _, b_mt) -> 
                (Atom.fresh "param_b_out", Atom.fresh "param_b_in", b_mt) 
        ) interceptor_info.inout_bridges_info) in
        let onstartup_params = 
            auto_fplace (interceptor_info.onboard_info.b_onboard_mt, param_b_onboarding)
            ::
            (List.flatten (List.map (function (param_b_out, param_b_in, b_mt) -> 
                [
                    auto_fplace (b_mt, param_b_out);
                    auto_fplace (b_mt, param_b_in);
                ]) onstartup_params_inout)
            )
            @ (match base_onstartup_opt with
                | None -> []
                | Some m -> m.value.args 
            )
        in

        let aux ((this_b_out, this_b_in, _), ( param_b_out, param_b_in, _)) = 
            auto_fplace (AssignThisExpr(
                this_b_out, 
                e2var param_b_out
            ))::
            auto_fplace (AssignThisExpr(
                this_b_in, 
                e2var param_b_in 
            ))::
            (List.map (function (this_outport_name,_) ->
                auto_fplace (ExpressionStmt(
                    e2_e (CallExpr(
                        e2var (Atom.builtin "bind"),
                        [ 
                            e2var this_outport_name;    
                            (* FIXME store the interface in used_outports/inports*)
                            if Utils.str_contains (Atom.to_string this_outport_name) "_egress_" then
                                e2var param_b_out 
                            else
                                e2var param_b_in 
                        ]
                    ))
                ))
            ) (List.of_seq (Hashtbl.to_seq_values used_outports))) @
            (List.map (function (this_inport_name,_) ->
                            logger#error "><> %b %s" (Utils.str_contains (Atom.to_string this_inport_name) "_ingress_" ) (Atom.to_string this_inport_name);
                auto_fplace (ExpressionStmt(
                    e2_e (CallExpr(
                        e2var (Atom.builtin "bind"),
                        [ 
                            e2var this_inport_name;    
                            if Utils.str_contains (Atom.to_string this_inport_name) "_egress_" then
                                e2var param_b_out 
                            else
                                e2var param_b_in 
                        ]
                    ))
                ))
            ) (List.of_seq (Hashtbl.to_seq_values used_inports)))
        in

        let onstartup = auto_fplace (auto_plgannot(Method (auto_fplace {
            annotations = (match base_onstartup_opt with Some m -> m.value.annotations | _ -> []);
            ghost = false;
            ret_type = mtype_of_ft TVoid;
            name = Atom.fresh "onstartup"; 
            args = onstartup_params;
            contract_opt = None;
            body = 
            auto_fplace (AssignThisExpr(
                Option.get interceptor_info.b_onboard_state,
                e2var param_b_onboarding
            ))
            ::
            auto_fplace (ExpressionStmt(
                e2_e (CallExpr(
                    e2var (Atom.builtin "bind"),
                    [ 
                        e2var (Option.get interceptor_info.this_port_onboard);
                        e2var param_b_onboarding 
                    ]
                ))
            ))
            ::
            (List.flatten(List.map aux (List.combine (Option.get interceptor_info.inout_statebridges_info) onstartup_params_inout)))
            @ (
                match base_onstartup_opt with
                | None -> []
                | Some m -> m.value.body
            );
            on_startup = true;
            on_destroy = false;
        }))) in

        (*** Retrun citems ***)
        onstartup :: citems_wo_onstartup



    (*************** Interception Elimination ******************)

    let generate_interceptor base_interceptor interceptor_info : _term = 
        logger#info "generate_interceptor";

        logger#debug "0 > \n%s" (show_interceptor_info interceptor_info);

        let interceptor_info, onboard_block = generate_onboard_block base_interceptor interceptor_info in
        logger#debug "1 > \n%s" (show_interceptor_info interceptor_info);

        let interceptor_info, inlined_onstartup_block = include_base_citems interceptor_info base_interceptor in
        logger#debug "2 > \n%s" (show_interceptor_info interceptor_info);

        let interceptor_info, sessions_block = generate_sessions_block interceptor_info in
        logger#debug "3 > \n%s" (show_interceptor_info interceptor_info);

        generate_network_infrastructure interceptor_info;

        let ingress_block = generate_ingress_block interceptor_info base_interceptor in
        let egress_block = generate_egress_block interceptor_info base_interceptor in
        let inlined_onstartup_block = generate_onstartup interceptor_info inlined_onstartup_block in

        let outports = List.map (function (name, outport)->
            logger#info "&& %s" (Atom.to_string name);
            auto_fplace (auto_plgannot(Outport (auto_fplace (outport, auto_fplace EmptyMainType))))
        ) (List.of_seq (Hashtbl.to_seq_values used_outports)) in
        let inports = List.map (function (name, inport)->
            logger#info "&& %s" (Atom.to_string name);
            auto_fplace (auto_plgannot(Inport (auto_fplace (inport, auto_fplace EmptyMainType))))
        ) (List.of_seq (Hashtbl.to_seq_values used_inports)) in

        Component (auto_fplace (ComponentStructure {
            target_name = SameAs base_interceptor.name; 
            annotations = base_interceptor.annotations;
            name = interceptor_info.name;
            headers = base_interceptor.headers;
            body = 
                onboard_block
                @ inlined_onstartup_block
                @ inports
                @ outports
                @ sessions_block
                @ ingress_block
                @ egress_block
        }))

    let makeinterceptor_selector = function 
    | Component {value=ComponentAssign {name; value={value=(AppCExpr ({value=VarCExpr functorname, _}, args)), _}}} when Atom.hint functorname = "MakeInterceptor" && Atom.is_builtin functorname -> true 
    | _ -> false
    let makeinterceptor_rewriter program parent_opt place = function
    | Component {value=ComponentAssign {name=interceptor_name; value={value=(AppCExpr ({value=VarCExpr functorname, _}, args)), _}}} as term -> begin

        (* Ad-hoc functor since we do not have meta programming capabilities *)
        match args with
        | [{value=VarCExpr base_interceptor_name,_;}; {value=AnyExpr {value=BlockExpr (List, intercepted_schemas), _}, _}] 
        when List.fold_left (function flag -> function | {value=BoxCExpr {value=VarCExpr _,_}, _} -> true | _ -> false) true intercepted_schemas -> begin

            let base_interceptor_place, base_interceptor = get_schema program base_interceptor_name in
            
            (*** Prepare base_interceptor for the current specialization ***)
            (* Attribute fresh name for all citems to fulfills the semantics of binders. Each binders introduce a fresh name. *)

            (* No name for onstartup / ondestroy *)
            let citems_wo_onstartup = List.filter (function | {value={v=Method m}} -> Bool.not m.value.on_startup && Bool.not m.value.on_destroy | _ -> true) base_interceptor.body in 

            (** Rename citems (refreshing all identity of bindings) *)
            let _, freevars = List.split (List.map (free_vars_component_item Atom.Set.empty) citems_wo_onstartup) in
            let freevars = Atom.Set.of_list (List.map snd (List.flatten freevars)) in

            let _, freetvars = List.split (List.map (free_tvars_component_item ~flag_tcvar:true  Atom.Set.empty) citems_wo_onstartup) in
            let freetvars = Atom.Set.of_list (List.flatten freetvars) in

            let renaming = 
                let state = Hashtbl.create 256 in
                function x -> 
                if Atom.is_builtin x then x (* TODO guarantee *) 
                else
                    match Hashtbl.find_opt state x with
                    | None -> 
                        (* x should not be a variable binded outside the included citems *)
                        if Atom.Set.find_opt x freevars = None && Atom.Set.find_opt x freetvars = None then 
                        begin
                            let y = Atom.fresh (Atom.hint x) in 
                            Hashtbl.add state x y;
                            logger#debug "rename %s -> %s" (Atom.to_string x) (Atom.to_string y);
                            y
                        end
                        else x
                    | Some y -> y
            in
            let base_interceptor = {base_interceptor with body = List.map (rename_component_item ~flag_rename_attribute:true renaming) base_interceptor.body} in
            (*** End prepare ***)


            (* Generated by ctx elim *)
            let interceptor_info = 
                match Hashtbl.find_opt interceptors_info interceptor_name with 
                | Some x -> assert(x.from_ctx_elim); x  
                | None -> 
                    let intercepted_schemas = Atom.Set.of_seq (List.to_seq (List.map (function {value=BoxCExpr {value=VarCExpr schema,_}, _} -> schema) intercepted_schemas)) in

                    (* Compute onboard_info *)
                    let st_onboard = st_onboard_of intercepted_schemas in
                    let b_onboard_mt = b_onboarf_mt_of interceptor_name (Atom.Set.to_list intercepted_schemas) (mtype_of_st st_onboard.value) in
                    let onboard_info = {st_onboard; b_onboard_mt} in

                    { from_ctx_elim = false;
                        name = interceptor_name; 
                        base_interceptor_name;
                        base_interceptor_place;

                        onboard_info;
                        inout_bridges_info = failwith "TODO howto to compute inout_bridges_info for low level API - add this to whitepapre";

                        intercepted_schemas;
                        dependencies = Atom.Set.empty;

                        b_onboard_state = None;
                        inout_statebridges_info = None;
                        sessions_info = None;
                        this_onboarded_activations = None;
                        this_port_onboard = None;
                    }
            in

            logger#error "%s > %s subset %s" (Atom.to_string interceptor_name) (Atom.Set.show interceptor_info.dependencies) (Atom.Set.show !resolved_dependencies);
            if Atom.Set.subset interceptor_info.dependencies !resolved_dependencies then(
                (* The dependencies have been resolved *)
                resolved_dependencies := (Atom.Set.add interceptor_name (!resolved_dependencies));

                (*** Check that intercepted_schemas can be captured by base_interceptor ***)

                (* schema_name -> capturable_by_schemas *)
                let all_schemas = Hashtbl.create 16 in 
                let _ = collect_term_program 
                    true (* recursive to collect all schemas of the AST *)
                    (function | Component _ -> true |_ -> false) 
                    (fun _ place -> function 
                        | Component {value = ComponentStructure cstruct } -> begin 
                            match List.filter (function | Capturable _ -> true | _ -> false) cstruct.annotations with
                            | [] -> []
                            | [Capturable annot] -> 
                                Hashtbl.add all_schemas cstruct.name (Atom.Set.of_seq (List.to_seq annot.allowed_interceptors));
                                []
                            | _ -> Error.perror place "At most one capturable annotations per schema."
                        end
                        | Component {value=ComponentAssign {name; value={value=(AppCExpr ({value=VarCExpr functorname, _}, args)), _}}} when Atom.hint functorname = "MakeInterceptor" && Atom.is_builtin functorname -> []| Component {value=ComponentAssign _ } -> failwith "componentassign are not yet supported by InterceptionElimination"
                ) program in
                
                Atom.Set.iter (function schema -> 
                    assert(Hashtbl.length all_schemas > 0);
                    let allowed_interceptors = 
                        try
                            Hashtbl.find all_schemas schema
                        with Not_found -> failwith (Printf.sprintf "schema [%s] not found in [all_schemas]" (Atom.to_string schema))
                    in
                    if Bool.not (Atom.Set.mem base_interceptor_name allowed_interceptors) then    
                        Error.perror place "%s can not be intercepted by %s. To make it capturable add ```@capturable`` annotation to %s." (Atom.value schema) (Atom.value interceptor_info.base_interceptor_name) (Atom.value schema);
                ) interceptor_info.intercepted_schemas;
                
                [ generate_interceptor base_interceptor interceptor_info ]
            ) else (
                [ term ]
            )


        end
        | _ -> Error.perror place "Illformed MakeInterceptor functor: MakeInterceptor(BaseInterceptor, [intercepted_schemas])"
    end

    let intercept_elim_program program =
        (* Elimination of MakeInterceptor *)
        let past_resolved_dependencies = ref Atom.Set.empty in
        let program = ref (rewrite_term_program makeinterceptor_selector (makeinterceptor_rewriter program) program) in 
        let max_depth = ref 2 in
        (* Fix point on resolved_dependencies *)
        begin
            while Bool.not (Atom.Set.subset !resolved_dependencies !past_resolved_dependencies)  do
                logger#debug ">> resolved_dependencies %s" (Atom.Set.show !resolved_dependencies);
                program := rewrite_term_program makeinterceptor_selector (makeinterceptor_rewriter !program) !program;
                past_resolved_dependencies := (!resolved_dependencies);
                decr max_depth;
                assert (!max_depth > 0);
                ()
            done
        end;

        !program

    (*********************************************************)
    let name = "InterceptionElimination"
    let displayed_pass_shortdescription = "interception logic has been eliminated from IR"
    let displayed_ast_name = "interception-eliminated IR"
    let show_ast = true
    let global_at_most_once_apply = false

    let precondition program = program

    let postcondition program = 
        (* Check: no MakeInterceptor *)
        ignore (collect_term_program true makeinterceptor_selector (fun _ place -> raise (Error.PlacedDeadbranchError (place, "InterceptionElimination: MakeInterceptor remains in IR"))) program);

        program 
    let apply_program = intercept_elim_program
end

