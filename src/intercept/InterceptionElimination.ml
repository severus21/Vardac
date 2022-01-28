open Core
open AstUtils
open IR
open Easy_logging
open IRMisc
open InterceptUtils

let logger = Logging.make_logger ("_1_ compspec.Intercept") Debug [];;

let fplace = (Error.forge_place "Intercept" 0 0) 
let auto_fplace smth = {place = fplace; value=smth}


include AstUtils2.Mtype.Make(struct let fplace = fplace end)

module type TArgs = sig
    (* name -> interceptor_info *)
    val interceptors_info : (Atom.atom, interceptor_info) Hashtbl.t
end

module Make (Args: TArgs) = struct

    (******************* Shared state of the pass **************************)
    include Args

    (*************** Step 0 - gather intell ******************)
    let methods_of (base_interceptor : component_structure) : method0 list =
        let citems = List.filter (function {value=Method _} -> true | _ -> false) base_interceptor.body in
        List.map (function |{value=Method m} -> m) citems

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

    (*
        Returns set of intercepted_ports of a schema 
        Do not captures the ports of nested schemas - since they are hidden for the outside.
    *)
    let extract_intercepted_ports_of_schema (schema_struct : component_structure) : InterceptedPortSet.t = 
        let intercepted_ports = List.map (function
        | {value=Port p} -> Some ((fst p.value).name, p.value) | _ -> None ) schema_struct.body in
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

    (*
        Returns set of intercepted_outputports of a schema 
        Do not captures the ports of nested schemas - since they are hidden for the outside.
    *)
    let extract_intercepted_outputports_of_schema (schema_struct : component_structure) : InterceptedOutportSet.t = 
        let intercepted_outputports = List.map (function
        | {value=Outport p} -> Some ((fst p.value).name, p.value) | _ -> None ) schema_struct.body in
        let intercepted_outputports = List.filter Option.is_some intercepted_outputports in
        let intercepted_outputports = List.map Option.get intercepted_outputports in

        InterceptedOutportSet.of_seq (List.to_seq intercepted_outputports)

    let compute_intercepted_outputports program (schemas : Atom.Set.t) = 
        let intercepted_outputports_per_schema = Hashtbl.create 16 in

        let selector = function
            | Component {value = ComponentStructure cstruct} -> Atom.Set.find_opt cstruct.name schemas <> None
            | _ -> false
        in
        let collector _ = function
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

    (*
        method onboard_A for schema A
    *)
    let get_onboard_of_schema default_onboard onboard_index schema = 
        match Hashtbl.find_opt onboard_index schema with
        | Some m -> m
        | None -> default_onboard 

    let e_param_of str = 
        let param = Atom.fresh str in
        param, e2var param
    
    (* branch: if flag == "Schema" ... *)
    let generate_main_callback_branch interceptor_info default_onboard onboard_index (e_this_b_onboard, e_this_onboarded_activations) ((param_schema, e_param_schema), (param_s, e_param_s)) schema: stmt =
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

        auto_fplace (IfStmt (
            e2_e (BinopExpr (
                e2var param_schema,
                StructuralEqual, 
                schema_to_label fplace schema
            )),
            auto_fplace (BlockStmt [
                (* ... local_s2 = select(param_s, label); *)
                auto_fplace (LetStmt (
                    mtype_of_ct (TTuple [ 
                        mtype_of_ct (TTuple [ a_mt; mtype_of_ft TPlace]);
                        mtype_of_st (STSend (mtype_of_ft TBool, auto_fplace STEnd))
                    ]),
                    local_s2,
                    e2_e (CallExpr (
                        e2var (Atom.builtin "select"),
                        [
                            e_param_s;
                            e_param_schema
                        ]
                    ))
                ));


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
                            e_this_b_onboard
                        ]
                    ))
                ));
                auto_fplace (LetStmt (
                    mtype_of_ct (TTuple [ a_mt; mtype_of_ft TPlace]),
                    local_res2,
                    e2_e (CallExpr (
                        e2var (Atom.builtin "nth"),
                        [
                            e_local_res;
                            e2_lit (IntLit 0)
                        ]
                    ))
                ));
                auto_fplace (LetStmt (
                    mtype_of_st (STSend (mtype_of_ft TBool, auto_fplace STEnd)),
                    local_s3,
                    e2_e (CallExpr (
                        e2var (Atom.builtin "nth"),
                        [
                            e_local_res;
                            e2_lit (IntLit 1)
                        ]
                    ))
                ));
                auto_fplace (LetStmt (
                    a_mt, 
                    local_a,
                    e2_e (CallExpr (
                        e2var (Atom.builtin "nth"),
                        [
                            e_local_res2;
                            e2_lit (IntLit 0)
                        ]
                    ))
                ));
                auto_fplace (LetStmt (
                    mtype_of_ft TPlace,
                    local_p,
                    e2_e (CallExpr (
                        e2var (Atom.builtin "nth"),
                        [
                            e_local_res2;
                            e2_lit (IntLit 1)
                        ]
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
            ]),
            None
        ))   

    (*
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
        let statedef_b_onboard = auto_fplace (State (auto_fplace (StateDcl {
            ghost = false;
            type0 = interceptor_info.onboard_info.b_onboard_mt;
            name = this_b_onboard;
            body = None;
        }))) in

        let this_onboarded_activations = Atom.fresh "onboarded_activations" in 
        let e_this_onboarded_activations = e2_e (AccessExpr (
            e2_e This, 
            e2var this_onboarded_activations
        )) in
        let statedef_onboarded_activations = auto_fplace (State (auto_fplace (StateDcl {
            ghost = false;
            type0 = mtype_of_ct (TDict (mtype_of_ft TActivationID, mtype_of_ct (TActivationRef (mt_internals_of fplace (Atom.Set.to_list interceptor_info.intercepted_schemas)))) );
            name = this_onboarded_activations;
            body = Some ( e2_e (Block2Expr (Dict, [])));
        }))) in

        let callback_onboard = Atom.fresh "onboard" in
        let port_onboard = Atom.fresh "port_onboard" in
        let port_onboard_def = auto_fplace (Port (auto_fplace (
            {
                name = port_onboard;
                input = e_this_b_onboard; 
                expecting_st = mtype_of_st interceptor_info.onboard_info.st_onboard.value; 
                callback = e2_e (AccessExpr (
                    e2_e This, 
                    e2var callback_onboard
                ));
            },
            auto_fplace EmptyMainType
        ))) in

        (*** Generate default_onboard ***)
        let default_onboard = Atom.fresh "default_onboard" in
        let default_onboard_def = auto_fplace (Method (auto_fplace {
            annotations = [];
            ghost = false;
            ret_type = mtype_of_ft TBool;
            name = callback_onboard;
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
        })) in


        (*** Main callback ***)
        let param_schema, e_param_schema = e_param_of "schema" in
        let param_s, e_param_s = e_param_of "s" in


        let callback_onboard_def = auto_fplace (Method (auto_fplace {
            annotations = [];
            ghost = false;
            ret_type = mtype_of_ft TVoid;
            name = callback_onboard;
            args = [
                auto_fplace (mtype_of_ft TStr, param_schema);
                auto_fplace (mtype_of_st interceptor_info.onboard_info.st_onboard.value, param_s)
            ];
            on_destroy = false;
            on_startup = false;
            contract_opt = None;
            body = List.map (generate_main_callback_branch interceptor_info default_onboard onboard_index (e_this_b_onboard, e_this_onboarded_activations) ((param_schema, e_param_schema), (param_s, e_param_s))) (Atom.Set.to_list interceptor_info.intercepted_schemas)
        })) in

        interceptor_info, [
            auto_fplace (Term (auto_fplace (Comments
                (auto_fplace(DocComment "******************** Onboarding Block ********************"))
            )));
            statedef_b_onboard;
            statedef_onboarded_activations;
            port_onboard_def;
            default_onboard_def;
            callback_onboard_def;
        ]

    (*************** Step 2 - Onstartup and inline other base component citems ******************)

    (*
        return an hydrated copy of interceptor_info
    *)
    let include_base_citems interceptor_info (base_interceptor : component_structure) : interceptor_info * component_item list = 
        (*** Collect intells ***)
        let base_onstartup_opt = get_onstartup base_interceptor in
        let citems_wo_onstartup = List.filter (function | {value=Method m} -> Bool.not m.value.on_startup | _ -> true) base_interceptor.body in 

        (*** Add states to store in/out bridges and onboarding bridge ***)
        
        (* Hydrate inout_statebridges *)
        assert( interceptor_info.inout_statebridges_info = None);
        let interceptor_info = {interceptor_info with 
            inout_statebridges_info = Some (List.mapi ( fun i (_, _, b_mt) -> 
                Atom.fresh ("b_out_"^string_of_int i),
                Atom.fresh ("b_in_"^string_of_int i),
                b_mt
            )  interceptor_info.inout_bridges_info);
        }
        in


        let inout_bridges_states = List.flatten (List.map ( function (b_out, b_in, b_mt) ->
            [ 
                auto_fplace (State (auto_fplace (StateDcl {
                    ghost = false;
                    type0 = b_mt;
                    name = b_in;
                    body = None 
                } )));
                auto_fplace (State (auto_fplace (StateDcl {
                    ghost = false;
                    type0 = b_mt;
                    name = b_out;
                    body = None 
                } )))
            ]
        ) (Option.get interceptor_info.inout_statebridges_info)) in

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

        let onstartup = auto_fplace (Method (auto_fplace {
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
            (List.map (function ((this_b_out, this_b_in, _), (param_b_out, param_b_in, _)) -> 
                auto_fplace (AssignThisExpr(
                    this_b_out, 
                    e2var param_b_out
                ));
                auto_fplace (AssignThisExpr(
                    this_b_in, 
                    e2var param_b_in 
                ));
                
            ) (List.combine (Option.get interceptor_info.inout_statebridges_info) onstartup_params_inout))
            @ (
                match base_onstartup_opt with
                | None -> []
                | Some m -> m.value.body
            );
            on_startup = true;
            on_destroy = false;
        })) in

        (*** Retrun citems ***)
        interceptor_info, onstartup :: (citems_wo_onstartup @ inout_bridges_states)


    (*************** Step 2 - Interception session block******************)
    let generate_sessions_block interceptor_info : interceptor_info * component_item list = 
        let this_4external2internal = Atom.fresh "4external2internal" in
        let this_4internal2external = Atom.fresh "4internal2external" in
        let this_sessions_4ext = Atom.fresh "sessions_4ext" in
        let this_sessions_4int = Atom.fresh "sessions_4int" in

        assert(interceptor_info.sessions_info = None);
        let interceptor_info = {interceptor_info with
            sessions_info = Some { 
                this_4external2internal;
                this_4internal2external; this_sessions_4ext; 
                this_sessions_4int; 
            }
        } in

        interceptor_info, [
            auto_fplace (Term (auto_fplace (Comments
                (auto_fplace(DocComment "******************** Intercepted Sessions Handling Block ********************"))
            )));

            auto_fplace (State (auto_fplace (StateDcl {
                ghost = false;
                type0 = mtype_of_ct (TDict (mtype_of_ft TSessionID, mtype_of_ft TSessionID));
                name = this_4external2internal;
                body = Some (e2_e (Block2Expr (Dict, [])));
            })));
            auto_fplace (State (auto_fplace (StateDcl {
                ghost = false;
                type0 = mtype_of_ct (TDict (mtype_of_ft TSessionID, mtype_of_ft TSessionID));
                name = this_4internal2external;
                body = Some (e2_e (Block2Expr (Dict, [])));
            })));
            auto_fplace (State (auto_fplace (StateDcl {
                ghost = false;
                type0 = mtype_of_ct (TDict (mtype_of_ft TSessionID, mtype_of_ft TWildcard));
                name = this_sessions_4ext;
                body = Some (e2_e (Block2Expr (Dict, [])));
            })));
            auto_fplace (State (auto_fplace (StateDcl {
                ghost = false;
                type0 = mtype_of_ct (TDict (mtype_of_ft TSessionID, mtype_of_ft TWildcard));
                name = this_sessions_4int;
                body = Some (e2_e (Block2Expr (Dict, [])));
            })));
        ]



    (*************** Step 3 - Ingress generation ******************)


    let has_kind_ingress interceptor_info = function
    | {value=CType {value = TBridge tbridge}} ->
        let right = tbridge.out_type in

        List.fold_left (fun flag schema -> 
            flag || (TypingUtils.is_subtype (mtype_of_cvar schema) right) 
        ) false (Atom.Set.to_list interceptor_info.intercepted_schemas)
    | _ -> raise (Error.DeadbranchError "intercepted bridge must have a bridge type!")

    let generate_ingress_block interceptor_info base_interceptor : component_item list = 
        failwith "TODO"

    (*************** Step 4 - Egress generation ******************)

    let generate_egress_callback_sessioninit interceptor_info st = 
        failwith "TODO"
    let generate_egress_callback_msg interceptor_info st_stage = 
        failwith "TODO"

    (*
        @param i - n° of the stage. 0 == session init 
    *)
    let generate_egress_block_per_intercepted_bridge_per_st_stage  interceptor_info b_intercepted this_b_out this_b_int b_mt i st_stage = 
        let e_this_b_out = e2_e (AccessExpr (e2_e This, e2var this_b_out)) in
        let e_this_b_int = e2_e (AccessExpr (e2_e This, e2var this_b_int)) in

        (*** Callback names ***)
        let egress_callback_name = Atom.fresh (Printf.sprintf "callback_egress__%s__%d" (Atom.to_string b_intercepted) i) in

        (*** Port & Outport generation ***)
        let egress_outport_name = Atom.fresh (Printf.sprintf "egress_outport__%s__%d" (Atom.to_string b_intercepted) i) in
        let egress_outport = auto_fplace (Outport (auto_fplace ({
            name = egress_outport_name;
            input = e_this_b_out;
        }, auto_fplace EmptyMainType))) in

        let egress_inport_name = Atom.fresh (Printf.sprintf "egress_inport__%s__%d" (Atom.to_string b_intercepted) i) in
        let egress_inport = auto_fplace (Port (auto_fplace ({
            name = egress_inport_name;
            input = e_this_b_int;
            expecting_st = mtype_of_st st_stage;
            callback = e2_e (AccessExpr(
                e2_e This,
                e2var egress_callback_name
            ));
        }, auto_fplace EmptyMainType))) in

        (*** Session interception callback ***)
        let callback_session_init : method0 option = 
            if i = 0 then 
                Some (generate_egress_callback_sessioninit interceptor_info st_stage)
            else None 
        in

        (*** Msg interception callback ***)
        let callback_msg : method0 = generate_egress_callback_msg interceptor_info st_stage in

        let tmsg, st_continuation = failwith "TODO to compute it reuse existing fct for Akka or RecvElim" in

        (*** Main callback ***)
        let param_msg = Atom.fresh "msg" in
        let param_s_in = Atom.fresh "s_in" in
        let egress_callback : method0 = 
            if i = 0 then 
                auto_fplace {
                    annotations = [];
                    ghost = false;
                    ret_type = mtype_of_ft TVoid;
                    name = egress_callback_name;
                    args = [
                        auto_fplace (tmsg, param_msg);
                        auto_fplace (st_continuation, param_s_in);
                    ];
                    body = [
                        auto_fplace (IfStmt(
                            e2_e (CallExpr( 
                                e2var (Atom.builtin "is_init_stage"),
                                [ e2var param_s_in ]
                            )),
                            auto_fplace (ReturnStmt (
                                e2_e (CallExpr(
                                    e2_e (AccessExpr(
                                        e2_e This,
                                        e2var (Option.get callback_session_init).value.name 
                                    )),
                                    [
                                        e2var param_msg;
                                        e2var param_s_in
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
                                        e2var param_msg;
                                        e2var param_s_in
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
                auto_fplace { callback_msg.value with name = egress_callback_name } 
        in

        [
            egress_outport;
            egress_inport;
            auto_fplace (Method egress_callback);
        ]
        @ (if i = 0 then
            [
                auto_fplace (Method (Option.get callback_session_init));
                auto_fplace (Method callback_msg);
            ]
        else [])


    let generate_egress_block_per_intercepted_bridge interceptor_info b_intercepted this_b_out this_b_int b_mt : component_item list = 
        let p_st = (match b_mt with | {value = CType {value = TBridge tb}} ->
            match tb.protocol with 
            | {value = SType st} -> st
            | _ -> failwith "TODO resolve type aliasing using an external fct or requires that type aliasing should have been eliminated before using [generate_egress_block_per_intercepted_bridge]"
        ) in
        let st_stages = stages_of_st p_st in

        List.flatten (
            List.mapi
                (generate_egress_block_per_intercepted_bridge_per_st_stage interceptor_info b_intercepted this_b_out this_b_int b_mt)
                st_stages
        )
    
    let has_kind_egress interceptor_info = function
    | {value=CType {value = TBridge tbridge}} ->
        let left = tbridge.in_type in

        List.fold_left (fun flag schema -> 
            flag || ( TypingUtils.is_subtype (mtype_of_cvar schema) left) 
        ) false (Atom.Set.to_list interceptor_info.intercepted_schemas)
    | _ -> raise (Error.DeadbranchError "intercepted bridge must have a bridge type!")

    let generate_egress_block program interceptor_info base_interceptor : component_item list = 
        auto_fplace (Term (auto_fplace (Comments
            (auto_fplace(DocComment "******************** Egress Block ********************"))
        )))
        :: List.flatten(
            List.map (function ((b_intercepted, _, b_mt), (this_b_out, this_b_int, _)) ->
                if Bool.not (has_kind_egress interceptor_info b_mt) then []
                else begin
                    auto_fplace (Term (auto_fplace (Comments
                        (auto_fplace(DocComment (Printf.sprintf "*** Egress Block for bridge [%s] ***" (Atom.to_string b_intercepted))))
                    )))
                    :: (generate_egress_block_per_intercepted_bridge interceptor_info b_intercepted this_b_out this_b_int b_mt) 
                end
            ) (List.combine interceptor_info.inout_bridges_info (Option.get interceptor_info.inout_statebridges_info)) 
        )


    (*************** Interception Elimination ******************)

    let generate_interceptor base_interceptor interceptor_info : _term = 

        let interceptor_info, onboard_block = generate_onboard_block base_interceptor interceptor_info in
        let interceptor_info, inlined_onstartup_block = include_base_citems interceptor_info base_interceptor in
        let interceptor_info, sessions_block = generate_sessions_block interceptor_info in
        let ingress_block = generate_ingress_block interceptor_info base_interceptor in
        let egress_block = generate_egress_block (failwith "TODO parent program or parent intercepted_outputports") interceptor_info base_interceptor in


        Component (auto_fplace (ComponentStructure {
            target_name = base_interceptor.target_name; 
            annotations = base_interceptor.annotations;
            name = interceptor_info.name;
            args = [];
            body = 
                onboard_block
                @ inlined_onstartup_block
                @ sessions_block
                @ ingress_block
                @ egress_block
        }))

    let makeinterceptor_selector = function 
    | Component {value=ComponentAssign {name; value={value=(AppCExpr ({value=VarCExpr functorname, _}, args)), _}}} when Atom.hint functorname = "MakeInterceptor" && Atom.is_builtin functorname -> true 
    | _ -> false
    let makeinterceptor_rewriter program place = function
    | Component {value=ComponentAssign {name=interceptor_name; value={value=(AppCExpr ({value=VarCExpr functorname, _}, args)), _}}} -> begin
        (* Ad-hoc functor since we do not have meta programming capabilities *)
        match args with
        | [{value=VarCExpr base_interceptor_name,_;}; {value=AnyExpr {value=BlockExpr (List, intercepted_schemas), _}, _}] 
        when List.fold_left (function flag -> function | {value=BoxCExpr {value=VarCExpr _,_}, _} -> true | _ -> false) true intercepted_schemas -> begin

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

                        onboard_info;
                        inout_bridges_info = failwith "TODO howto to compute inout_bridges_info for low level API - add this to whitepapre";

                        intercepted_schemas;

                        b_onboard_state = None;
                        inout_statebridges_info = None;
                        sessions_info = None;
                    }
            in


            (*** Check that intercepted_schemas can be captured by base_interceptor ***)

            (* schema_name -> capturable_by_schemas *)
            let all_schemas = Hashtbl.create 16 in 
            let _ = collect_term_program 
                true (* recursive to collect all schemas of the AST *)
                (function | Component _ -> true |_ -> false) 
                (function place -> function 
                    | Component {value = ComponentStructure cstruct } -> begin 
                        match List.filter (function | Capturable _ -> true | _ -> false) cstruct.annotations with
                        | [Capturable annot] -> 
                            Hashtbl.add all_schemas cstruct.name (Atom.Set.of_seq (List.to_seq annot.allowed_interceptors));
                            []
                        | _ -> Error.error place "At most one capturable annotations per schema."
                    end
            ) in
            
            Atom.Set.iter (function schema -> 
                let allowed_interceptors = (Hashtbl.find all_schemas schema) in
                if Bool.not (Atom.Set.mem base_interceptor_name allowed_interceptors) then    
                    Error.error place "%s can not be intercepted by %s. To make it capturable add ```@capturable`` annotation to %s." (Atom.value schema) (Atom.value interceptor_info.base_interceptor_name) (Atom.value schema);
            ) interceptor_info.intercepted_schemas;
            
            (*TODO and checks pass and select port accordingly 
            *)

            let base_interceptor : component_structure = get_schema program interceptor_info.base_interceptor_name in

            [ generate_interceptor base_interceptor interceptor_info ]
        end
        | _ -> Error.error place "Illformed MakeInterceptor functor: MakeInterceptor(BaseInterceptor, [intercepted_schemas])"
    end

    let intercept_elim_program program =
        (* Elimination of MakeInterceptor *)
        let program = rewrite_term_program makeinterceptor_selector (makeinterceptor_rewriter program) program in 

        program
end


(* TODO to remove the followings *)


(*
    void callback(tmsg msg, tsession s){
        tsession_out sessionout = ... from s;
        activation<> from = ...;
        activation<> to = ...;

        (* Case one user define an interceptor *)
        option<tmsg> res = this.intercept(from, to, sessionin, sessionout, msg);

        if(res not =equals= None){ 
            fire(session_out, option.get res);
        }

        (* Case no interceptor *)
        fire(session_out, msg);

    }
*)
let generate_callback (base_interceptor : component_structure) port_name (expecting_st, t_bridge) = 
    let a_msg, a_session_in = Atom.fresh "msg", Atom.fresh "session_in" in 
    let t_msg, t_session_in = match expecting_st.value with 
        | STRecv (t_msg, t_session) -> t_msg, t_session
        | STBranch branches -> mtype_of_ft TStr, expecting_st
    in
    let a_session_out, t_session_out = Atom.fresh "session_out", dual expecting_st in
    let mt_session_out, mt_session_in = mtype_of_st t_session_out.value, mtype_of_st t_session_in.value in

    let a_from, a_to = Atom.fresh "from", Atom.fresh "to" in
    let t_from, t_to = mtype_of_ct (TActivationRef t_bridge.in_type), mtype_of_ct (TActivationRef t_bridge.out_type) in
    let a_res, t_res = Atom.fresh "res", mtype_of_ct (TOption t_msg) in

    (* TODO/REFACTOR for perf built once a htbl of intecptors per base_interceptors *)
    let user_defined_interceptor_selector = function
    | {value=Method m} when List.mem (failwith "previously: Intercept") m.value.annotations  -> (* only consider method marked as interceptor *)
        
        let flag1 = match m.value.ret_type.value with
            | CType {value=TOption t } -> equal_mtype t t_msg 
            | _ -> Error.error m.place "Invalid intercept method, return type should be an option"
        in

        let flag2 = match m.value.args with
            | [_from; _to; _session_in; _session_out; _msg] ->
                (* NB two bridges that have the same protocol and that have the same input output type will have the same interceptor.
                However, programmer can distinguish between both by accessing the unique bridge identifier.
                TODO TODOC add this to doc
                *)
                (Core.TypingUtils.is_subtype (fst _from.value) t_from) &&
                (Core.TypingUtils.is_subtype (fst _to.value) t_to) &&
                equal_mtype (fst _session_in.value) (auto_fplace (SType t_session_in)) &&
                equal_mtype (fst _session_out.value) (auto_fplace (SType t_session_out)) &&
                equal_mtype (fst _msg.value) t_msg
            | _ -> Error.error m.place "Invalid intercept method, expected aruments should have the following form [from, to, session_in, session_out, msg]"
        in
        flag1 && flag2
        | _ -> false
    in

        
    let user_define_interceptor_opt : method0 option = match List.filter user_defined_interceptor_selector base_interceptor.body with
        | [] -> None
        | [{value=Method m}] -> Some m 
        | m::ms -> Error.error (List.fold_left (fun acc m -> acc@m.place) m.place ms) "Multiple interceptors defined with the same signature" (* TODO should be checked before generating the callback and for all @capturable component *)
    in

    {
        annotations = [];
        ghost = false;
        ret_type = mtype_of_ft TVoid;
        name = Atom.fresh ((Atom.value port_name) ^ "__callback");
        args = [
            auto_fplace (t_msg, a_msg); 
            auto_fplace (mt_session_in, a_session_in)
        ]; 
        body = [ 
            auto_fplace (LetStmt (mt_session_out, a_session_out, failwith "TODO get session out"));
            auto_fplace (LetStmt(t_from, a_from, 
                e2_e (CallExpr(
                    e2var (Atom.builtin "session_from"),
                    [
                        auto_fplace (VarExpr a_session_in, mt_session_in)
                    ]
                ))
            ));
            auto_fplace (LetStmt(t_to, a_to, 
                e2_e (CallExpr(
                    e2var (Atom.builtin "session_to"),
                    [
                        auto_fplace (VarExpr a_session_out, mt_session_out)
                    ]
                ))
            ));

            (match user_define_interceptor_opt with
                | None ->
                    auto_fplace (ExpressionStmt (e2_e(
                        CallExpr(
                            e2var (Atom.fresh "fire"),
                            [
                                auto_fplace (VarExpr a_session_out, mt_session_out);
                                auto_fplace (VarExpr a_msg, t_msg)
                            ]
                        )
                    )))
                | Some user_defined_interceptor ->
                    auto_fplace (BlockStmt [
                        auto_fplace (LetStmt (
                            t_res,
                            a_res,
                            e2_e (CallExpr(
                                e2var (Atom.fresh "fire"),

                                [
                                    auto_fplace (VarExpr a_from, t_from); 
                                    auto_fplace (VarExpr a_to, t_to);
                                    auto_fplace (VarExpr a_session_in, mt_session_in);
                                    auto_fplace (VarExpr a_session_out, mt_session_out); 
                                    auto_fplace (VarExpr a_msg, t_msg); 
                                ]
                            ))
                        ));
                        auto_fplace (IfStmt(
                            auto_fplace(UnopExpr(Not, auto_fplace(BinopExpr(
                                auto_fplace (VarExpr a_res, t_res),
                                StructuralEqual,
                                auto_fplace (OptionExpr None, t_res)
                            ), mtype_of_ft TBool)), mtype_of_ft TBool),
                            auto_fplace (ExpressionStmt(
                                e2_e (CallExpr(
                                    e2var (Atom.fresh "fire"),
                                    [
                                        e2var a_session_out; 
                                        e2_e (CallExpr (
                                            e2var  (Atom.builtin "option_get"),
                                            [
                                                auto_fplace (VarExpr a_res, t_res)
                                            ] 
                                        ))
                                    ]
                                ))
                            )),
                            None
                        ))
                    ])
            )
        ];
        contract_opt = None;
        on_destroy = false;
        on_startup = false;
    }

let make_citem_for_intercepted_component program base_interceptor intercepted_cname = 
    let [intercepted_struct] : component_structure list = 
        collect_term_program 
        false 
        (function | Component {value=ComponentStructure {name}} -> name = intercepted_cname | _ -> false) (function place -> function | Component {value=ComponentStructure cstruct} -> [cstruct]) program 
    in

    let interception_states = [] in 

    (* Input ports and bridges *)
    let intercepted_input_ports = List.filter (
        function 
        |{value=Port _} -> true | _ -> false) intercepted_struct.body
    in

    (* NB. port type is left unchanged *)
    let interception_callbacks, interception_ports = List.split (List.map (function |{place; value=Port {value=p,mt_p; place=p_port}} -> 
        let port_name = Atom.fresh ("interceptor_pinput_"^(Atom.to_string intercepted_cname)^"_"^(Atom.to_string p.name)) in
        let t_bridge:tbridge = match mt_p.value with 
            | CType {value=TPort ({value=CType {value=TBridge t_bridge}},_)} -> t_bridge 
            | _ -> raise (Error.PlacedDeadbranchError (mt_p.place, "Can not extract bridge type"))
        in
        let callback = generate_callback base_interceptor port_name ((match p.expecting_st.value with | SType st -> st | _ -> raise (Error.PlacedDeadbranchError (p.expecting_st.place, "port expecting_st must be a session type"))), t_bridge) in
        
        auto_fplace (Method (auto_fplace callback)), auto_fplace (Port (auto_fplace ({
            name = port_name;
            input = p.input;
            expecting_st = p.expecting_st; (* FIXME if not anonymous add the identity propagation ?? *)
            callback = e2var callback.name;
        }, mt_p)))    
    ) intercepted_input_ports) in
    
    


    (* Output ports and bridges *)
    failwith "TODO intercept output of intercepted";
    (* FIXME TODO Receive case ??? -> should have been rewritten or smth else *)

    (interception_states, interception_ports, interception_callbacks)
(*
    replace 
    bridge< ... | A, ..., ...> -> bridge<....| A | Interceptor, ..., ...> 
    bridge< ..., ... | A, ..., ...> -> bridge<..., ....| A | Interceptor, ...> 

    REFACTOR
    For performance, one can do one update pass for all tuple (intercepted_name, interceptor_name)
    For readability and code reuse, we do one pass per (intercepted_name, interceptor_name)
*)
let update_bridges_types program (intercepted_name, interceptor_name) = 
    let aux_selector = function 
        | CompType {value=CompTUid intercepted_name} -> true
        | _ -> false
    in
    let in_selector = function
    | CType {value=TBridge tbridge} -> 
        let _, collected_elts, _ = collect_type_mtype None Atom.Set.empty aux_selector (fun _ _ _ -> []) tbridge.in_type in
        collected_elts <> []
    | _ -> false
    in
    let in_rewriter = function 
        | CType {value=TBridge tbridge; place} ->  CType {place; value=TBridge {tbridge with 
            in_type = mtype_of_ct (TUnion (tbridge.in_type, mtype_of_ct (TActivationRef interceptor_name)))
        }}
    in
    let out_selector = function
    | CType {value=TBridge tbridge} -> 
        let _, collected_elts, _ = collect_type_mtype None Atom.Set.empty aux_selector (fun _ _ _ -> []) tbridge.out_type in
        collected_elts <> []
    | _ -> false
    in
    let out_rewriter = function 
        | CType {place; value=TBridge tbridge} ->  CType {place; value=TBridge {tbridge with 
            out_type = mtype_of_ct (TUnion (tbridge.out_type, mtype_of_ct (TActivationRef interceptor_name)))
        }}
    in 

    program 
    |> rewrite_type_program in_selector in_rewriter  
    |> rewrite_type_program out_selector out_rewriter


let makeinterceptor_selector = function 
| Component {value=ComponentAssign {name; value={value=(AppCExpr ({value=VarCExpr functorname, _}, args)), _}}} when Atom.hint functorname = "MakeInterceptor" && Atom.is_builtin functorname -> true 
| _ -> false

let makeinterceptor_rewriter program place = function
| Component {value=ComponentAssign {name; value={value=(AppCExpr ({value=VarCExpr functorname, _}, args)), _}}} -> begin
    (* Ad-hoc functor since we do not have meta programming capabilities *)
    match args with
    | [{value=VarCExpr interceptor_name,_;}; {value=AnyExpr {value=BlockExpr (List, component_types), _}, _}] 
    when List.fold_left (function flag -> function | {value=BoxCExpr {value=VarCExpr _,_}, _} -> true | _ -> false) true component_types ->
        let spawned_component_types = List.map (function
            |{value=BoxCExpr {value=VarCExpr cname, _}, _} -> cname
        ) component_types in

        let base_interceptor : component_structure = InterceptUtils.get_schema program interceptor_name in
            
        (* base_interceptor must be a component not a functor *)
        assert(base_interceptor.args = []);

        let base_onstartup = InterceptUtils.get_onstartup base_interceptor in 
        let m_interceptors = List.filter (function |
        {value=Method m} -> List.mem (failwith "previously: Intercept") m.value.annotations | _ -> false) base_interceptor.body in
        let other_citems =List.filter (function |
        {value=Method m} -> Bool.not (List.mem (failwith "previously: Intercept") m.value.annotations) | _ -> true) base_interceptor.body in


        let onstartup_args = 
            [] @
            match base_onstartup with
            | None -> []
            | Some m -> m.value.args 
        in
        let onstartup_body = 
            [] @
            match base_onstartup with
            | None -> []
            | Some m -> m.value.body 
        in
        let onstartup = auto_fplace (Method (auto_fplace {
            annotations = (match base_onstartup with | None -> [] | Some m -> m.value.annotations);
            ghost = false;
            ret_type = mtype_of_ft TVoid;
            name = Atom.fresh "interceptor_onstartup";
            args = onstartup_args;
            body = onstartup_body; 
            contract_opt = (match base_onstartup with | None -> None | Some m -> m.value.contract_opt);
            on_startup = true;
            on_destroy = false;
        })) in (* TODO we need to add logic here ?? *)

        let tmp = List.map (make_citem_for_intercepted_component program base_interceptor) spawned_component_types in
        let interception_states = List.flatten (List.map (function (x,_,_) -> x) tmp) in 
        let interception_ports = List.flatten (List.map (function (_,y,_) -> y) tmp) in 
        let interception_callbacks = List.flatten (List.map (function (_,_,z) -> z) tmp) in 


        let structure = {
            target_name = base_interceptor.target_name; 
            annotations = base_interceptor.annotations; (* NB. remove annotations that have been consumed *)
            name = name; 
            args = []; (* Not a functor *)
            body = 
                onstartup ::
                other_citems @
                interception_states @
                interception_ports @
                interception_callbacks;
        }
        in

        [ Component (auto_fplace (ComponentStructure structure)) ]
    | _ -> Error.error place "Functor [MakeInterceptor] expect two arguments : the Interceptor component and a list of Component type that should be intercepted" 
end

let apply_intercept_program program =
    (* Step 0. resolve MakeInterceptor *)
    let program = rewrite_term_program makeinterceptor_selector (makeinterceptor_rewriter program) program in 


    (* TODO rewrite bridges types *)
    List.fold_left update_bridges_types program (failwith "TODO rewrite bridgeis");

    (* Step 1. InterceptedActivationRef *)
    failwith "TODO apply_intercept_program"

    program

(*********************************************************)

let displayed_pass_shortdescription = "interception logic has been eliminated from IR"
let displayed_ast_name = "interception-eliminated IR"
let show_ast = true
let precondition program = program
let postcondition program = program
let apply_program = apply_intercept_program
