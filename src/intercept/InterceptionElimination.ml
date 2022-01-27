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

    (*************** Step 1 - Activation onboarding generation ******************)
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



    (*TODO Where to place this *)
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
    let generate_onboard_bloc place interceptor_info (interceptor : component_structure) = 
        (*** Collect intells ***)
        let onboard_methods = extract_onboard_methods (methods_of interceptor) in
        let onboard_index = generate_onboard_index interceptor_info onboard_methods in

        (*** States and port ***)
        let this_b_onboard = Atom.fresh "b_onboard" in 
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
            type0 = mtype_of_ct (TDict (mtype_of_ft TActivationID, mtype_of_ct (TActivationRef (mt_internals_of place (Atom.Set.to_list interceptor_info.intercepted_schemas)))) );
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

        [
            statedef_b_onboard;
            statedef_onboarded_activations;
            port_onboard_def;
            default_onboard_def;
            callback_onboard_def;
        ]


    (*************** Step 2 - Interception session handling generation ******************)
    (*************** Step 3 - Ingress generation ******************)
    (*************** Step 4 - Egress generation ******************)

    (*************** Interception Elimination ******************)

    (*** Interceptor generation ***)

    (* TODO check that InterceptedSchemas are annotated with capturable and checks pass and select port accordingly 
        build a globlal index for the all program
    *)

    (* TODO hydrate interceptors_info for non ctx crafted interceptor *)

    (*** Elim ***)

end


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
        collect_term_program (function | Component {value=ComponentStructure {name}} -> name = intercepted_cname | _ -> false) (function place -> function | Component {value=ComponentStructure cstruct} -> [cstruct]) program 
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
