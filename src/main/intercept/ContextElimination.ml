open InterceptUtils
open Core
open AstUtils
open IR
open Easy_logging
open IRMisc
open IRUtils
 


module Make () = struct
    let logger = Core.Utils.make_log_of "ContextElimination" 

    let fplace = (Error.forge_place "Intercept.ContextElimination" 0 0) 
    let auto_fplace smth = {place = fplace; value=smth}
    let auto_annote (x: 'a) : 'a plg_annotated = {
        plg_annotations = [];
        v = x
    }
    include AstUtils2.Mtype.Make(struct let fplace = fplace end)

    (******************* Shared state of the pass **************************)
    (* 
        State used to deduplicate and place, interceptor specialization i.e. interception make functor
    *)

    (* key for dedup *)
    type interceptor_key = Atom.t * Atom.Set.t

    (** 
        Mapping from key to 
        - the MakeInterceptor stmt used to create the key related interceptor schema
        - the onboarding protocol name
        - the onboarding session type
        - the onboarding protocol def
    *)
    let interceptor_makes : (interceptor_key, (component_dcl * (Atom.atom * session_type * _term))) Hashtbl.t = Hashtbl.create 16 

    (* cf. 4.b.1 *)
    let to_specialized_defined_policy : (Atom.atom, (Atom.atom * Atom.atom) list) Hashtbl.t = Hashtbl.create 16

    module AtomOptionSet = Set.Make(struct 
        type t = Atom.atom option
        let compare = Option.compare Atom.compare 
    end)

    (** 
        Store the schemas (called parents) that owns a ctx using the interceptor schema binded to the key
        * key -> parent set
    *)
    let interceptor_parents : (interceptor_key, AtomOptionSet.t) Hashtbl.t = Hashtbl.create 16

    let show_key (cname, cnames) = 
        Printf.sprintf "Key<%s><%s>" (Atom.show cname) (Atom.Set.show cnames) 


    (* TODO use interceptors_info to avoid recomputing interceptors_schemas

        Warning: interceptor_info should only stores information that are exchanged between ctx elim and intercep elim
    *)
    let interceptors_info : (Atom.atom, interceptor_info) Hashtbl.t = Hashtbl.create 16 

    (* Stores registering methods for a schema B to onboard an intercepted children a:A in i:I
        (parent_schema i.e. B) -> {(intercepted_schema i.e. A, interceptor_schema i.e I) -> registering method name of B}
    *)
    let do_onboards_of : (Atom.atom, (Atom.atom * Atom.atom, Atom.atom) Hashtbl.t) Hashtbl.t = Hashtbl.create 16
    let do_onboard_of parent_schema intercepted_schema interceptor_schema = 
        match Hashtbl.find_opt do_onboards_of parent_schema with 
        | Some htbl -> begin
            match Hashtbl.find_opt htbl (intercepted_schema, interceptor_schema) with 
            | Some x -> x
            | None ->
                let x = Atom.fresh (Printf.sprintf "onboard_%s_in_%s" (Atom.to_string intercepted_schema) (Atom.to_string interceptor_schema)) in
                Hashtbl.add htbl (intercepted_schema, interceptor_schema) x;
                x
        end
        | None -> 
            let x = Atom.fresh (Printf.sprintf "onboard_%s_in_%s" (Atom.to_string intercepted_schema) (Atom.to_string interceptor_schema)) in
            let htbl = Hashtbl.create 16 in
            Hashtbl.add htbl (intercepted_schema, interceptor_schema) x;
            Hashtbl.add do_onboards_of parent_schema htbl;
            x

    (*************** Step 0 - gather intell ******************)

    (*** Step 0.A - intercepted_schemas + exposed_activations ***)


    module ExposedActivationSet = Set.Make(struct 
        type t = Atom.atom * Atom.atom (* (schema, activation variable) *)
        let compare (a, b) (c, d) = 
            let r1 = Atom.compare a b in
            let r2 = Atom.compare c d in

            if r1 = r2 then 0
            else 
                if r1 = -1 * r2 then r1 - r2 
                else r1 + r2 
    end)

    (**
        Returns the set of intercepted_schemas
        N.B. 
            - works because binders introduce unique variable names
            - only capture direct intercepted_schemas (see interception whitepaper for details)
    *)
    let intercepted_schemas_of_stmt place cname stmt = 
        let selector = function (* TODO ctxelim_rewrite_stmt duplicat this selector, both should have the same semantics *)
            (* direct *)
            | Spawn _ -> true
            (* indirect = indirect-call *)
            (* TODO *)

            | _ -> false
        in
        let collector parent_opt _ = function
            | {place; value=Spawn spawn, _} -> begin 
                match fst spawn.c.value with
                | VarCExpr c -> [c]
                | _ -> Error.perror place "spawn first arg should have been reduce into a cexpr value (i.e. component name)"
            end
            | _ -> []
        in 


        let _, intercepted_schemas, _ = collect_expr_stmt None Atom.Set.empty selector collector stmt in

        Atom.Set.of_seq (List.to_seq intercepted_schemas)

    (*
        cname - base interceptor name
        stmts - ctx body 
    *)
    let intercepted_schemas_of_ctx place cname stmts : Atom.Set.t =
        let intercepted_schemas_sets = List.map (intercepted_schemas_of_stmt place cname) stmts in
        List.fold_left (fun acc set -> Atom.Set.union acc set) Atom.Set.empty intercepted_schemas_sets

    (**
        Returns set of exposed_activations : (schema, atom) Set - 
        N.B. 
            - works because binders introduce unique variable names
            - only capture direct-simpl exposed activations (see interception whitepaper for details)
    *)
    let exposed_activations_of_stmt place cname stmt : ExposedActivationSet.t = 
        let selector = function
            (* direct-simpl*)
            | LetStmt (_, _, {value=Spawn _, _}) -> true

            (* direct-complex*)
            (* TODO not processed neeed control flow - TODOC search spawn or activation in left hand side of a let then search where the activation is spawned then decide*)

            (* no direct exposure since inner binders can leak out ctx stmt *) 
            | ForeachStmt (_, _, _, _) | IfStmt(_,_,_) | MatchStmt (_,_) | BlockStmt _ | WithContextStmt (_,_,_,_)  -> false 

            (* indirect *)
            (* TODO *)

            | _ -> false
        in
        let collector parent_opt place = function
            | LetStmt (_, x, {value=Spawn spawn, _}) -> begin 
                match fst spawn.c.value with
                | VarCExpr c -> [(c, x)]
                | _ -> Error.perror place "spawn first arg should have been reduce into a cexpr value (i.e. component name)"
            end
            | _ -> []
        in 

        let exposed_activations = collect_stmt_stmt None selector collector stmt in
        ExposedActivationSet.of_seq (List.to_seq exposed_activations)

    (**
        @param cname - base interceptor name
        @param stmts - ctx body 
    *)
    let exposed_activations_of_ctx place cname stmts : ExposedActivationSet.t =
        let exposed_activations_sets = List.map (exposed_activations_of_stmt place cname) stmts in
        List.fold_left (fun acc set -> ExposedActivationSet.union acc set) ExposedActivationSet.empty exposed_activations_sets


        (* 
            TODOC
            * Interception egress/ingress communication: Direct/Indirect spawns have only knowledge of intercepted bridges (interceptor <--> internals) - scope mechanism
            * Interception identity: Binders inside inner stmts can not escape and (direct/indirect) spawns in control-flow expression (e.g. ``if (e) {}``) can not be binded  

            exposed_activations - for handling identity exposure but not for actual msg interception (based on bridge renaming), i.e activation captured by binders (
                direct ``let a = spawn ...`` 
                direct-complex ``let res = (1, spawn ...)``
                indirect-call ``let ... = f()`` where f generate an activation - can be recursive
                indirect-assign ``somemap[key] = spawn ...`` - indirect binded if somewhere let .. = somemap[somekey] or if inherited from ctx parent scope (i.e. defined externaly) and not a binder (i.e. not in exposed_activations) if there is no ``let ... = somemap[...]`` + not inherited
                    - could have and indirect-assign behind a call (fct_assign(spawn ..., somemap))
            )

            intercepted_schemas - needed to make interceptor <: Internals ; introduce custom logic to avoid building something like union of left/right intercepted_bridges

            relation between both
            * schemas(exposed_activations) \subset intercepted_schemas
            * intercepted_schemas \not\subset schemas(exposed_activations) in the general case because all activation not binded spawned in body are part of intercepted components but not exposed_activations ``e.g. spawn A(...)`` 
        *)

    module InterceptedBridgeSet = Set.Make(struct 
        type t = composed_type * Atom.atom (* TBridge, name *)
        let compare (_, b) (_, d) = Atom.compare b d
    end)

    (**
        @returns set of intercepted_bridges : (bridge type:TBridge, atom) Set - 
        N.B. 
            - works because binders introduce unique variable names
            - complet capture
    *)
    let intercepted_bridges_of_ctx place cname stmts : InterceptedBridgeSet.t = 
        let fvars = List.map (function stmt -> snd (free_vars_stmt Atom.Set.empty stmt)) stmts in
        let fvars = List.flatten fvars in

        let intercepted_bridges = List.map (function (mt,x) ->
            match mt.value with (* FIXME do not work if m.value is an alias of bridge -> us is_subtype or type equal if they handle aliasing *)
            | CType ({ value = TBridge _ } as tb) -> Some (tb, x)
            | _-> None
        ) fvars in

        let intercepted_bridges = List.filter (function opt -> opt <> None) intercepted_bridges in
        let intercepted_bridges = List.map Option.get intercepted_bridges in
        InterceptedBridgeSet.of_seq (List.to_seq intercepted_bridges)

    (*************** Step 1 - Specialized interceptor generation ******************)

    (*** Step 1.A - TODO ***)

    (**  Generates an interceptor key per context 
        key : (cname, set of intercepted_schemas)
    *)
    let key_of_ctx place cname stmts = 
        let intercepted_schemas = intercepted_schemas_of_ctx place cname stmts in
        let key = (cname, intercepted_schemas) in

        key

    let interceptor_name_of_ctx place cname stmts = 
        Atom.fresh ("Interceptor"^(Atom.value cname))


    (*** Step 1.B - Generation  ***)

    (**
        Generates ``component InterceptorXX = MakeInterceptor(BaseInterceptor, intercepted_schemas)``;
    *)
    let generate_make_schema place cname stmts intercepted_schemas = 
        let interceptor_name = interceptor_name_of_ctx place cname stmts in
        let intercepted_schemas = (List.of_seq (Atom.Set.to_seq intercepted_schemas)) in

        auto_fplace (ComponentAssign {
            name = interceptor_name;
            value = ce2_ce (
                AppCExpr (
                    ce2var (Atom.builtin "MakeInterceptor"), 
                    [
                        ce2var cname;
                        ce2_ce (AnyExpr 
                            (e2_e (BlockExpr(
                                AstUtils.List,
                                List.map (function x -> 
                                    e2_e (BoxCExpr (ce2var x))
                                ) intercepted_schemas
                            ))
                        ))
                    ]
                ));
        })

    let generate_onboard_typedef place interceptor_name intercepted_schemas = 
        let st_onboard = st_onboard_of intercepted_schemas in

        let p_onboard = Atom.fresh ("p_onboard_"^(Atom.value interceptor_name)) in 
        let p_def_onboard = ProtocolDef (p_onboard, mtype_of_st st_onboard.value) in
        (p_onboard, st_onboard, Typedef (auto_fplace p_def_onboard))

    (**
        Register each ctx inside shared state (interceptor_parents, interceptor_makes) in order to deduplicate schemas.
    *)
    let ctxelim_prepare_stmt (parent_opt : Atom.atom option) place : _stmt -> unit list = function
        | WithContextStmt (anonymous_mod, cname, e, stmts) -> 
            let key = key_of_ctx place cname stmts in
            let intercepted_schemas = intercepted_schemas_of_ctx place cname stmts in

            logger#debug "ctxelim_prepare: %s" (show_key key);

            (* Deduplicate interceptor component type *)
            try
                match Hashtbl.find_opt interceptor_makes key with
                | Some _ -> begin 
                    try
                        Hashtbl.add interceptor_parents key (AtomOptionSet.add parent_opt (Hashtbl.find interceptor_parents key));
                        [] 
                    with Not_found -> failwith "key %s not found in interceptor_parents" key
                end
                | None -> begin

                    let make_schema_stmt = generate_make_schema place cname stmts intercepted_schemas in
                    let (p_onboard, st_onboard, p_def_onboard) = generate_onboard_typedef place cname intercepted_schemas in

                    Hashtbl.add interceptor_parents key (AtomOptionSet.singleton parent_opt);
                    Hashtbl.add interceptor_makes key (make_schema_stmt, (p_onboard, st_onboard, p_def_onboard));
                    []
                end
            with Not_found ->
                failwith "key %s not found in interceptor_parents" key

    (*** Step 1.C - Insert new schema in lca of intercepted_schemas ***)
    let insert_interceptor_dcl key (interceptor_dcl,_) (program:program) : program =
        let parents = 
            try
                Hashtbl.find interceptor_parents key 
            with Not_found -> failwith "key %s not found in interceptor_parents" key
        in

        AtomOptionSet.iter (function opt ->
            logger#warning ">> Parent %s" (match opt with | None -> "None" | Some x -> Atom.to_string x)
        ) parents;

        IRUtils.insert_terms_into_lca (List.of_seq (AtomOptionSet.to_seq parents)) [(auto_fplace (auto_plgannot (Component interceptor_dcl)))] program   

    (*************** Step 2 - Bridge handling  ******************)



    (**  Per context and not (per interceptor schema or per interceptor activation) 
        - p_onboard/intercepted_bridges are defined in a per interceptor schema base
    *)
    let generate_bridges place interceptor_name intercepted_schemas p_onboard st_onboard intercepted_bridges =
        assert(Bool.not (InterceptedBridgeSet.is_empty intercepted_bridges));
        let intercepted_schemas = (List.of_seq (Atom.Set.to_seq intercepted_schemas)) in

        (*** b_in ***)
        (* b -> (b_ext = b, b_in, b_in_constructor)*)
        let generated_bridges = Hashtbl.create 16 in
        InterceptedBridgeSet.iter (function (tbridge, b) -> 
            let b_in = Atom.fresh ((Atom.value b)^"_in") in

            (* FIXME default constructor - see whitepaper for improvement *)
            let b_in__constructor = e2_e (CallExpr(
                e2var (Atom.builtin "bridge"),
                []
            )) in

            let b_in__let = auto_fplace (LetStmt (
                mtype_of_ct tbridge.value,
                b_in,
                b_in__constructor
            )) in

            Hashtbl.add generated_bridges b (b, b_in, b_in__let)
        );

        (*** b_onboard ***)
        let b_onboard = Atom.fresh ("b_onboard_"^(Atom.value interceptor_name)) in 
        let b_onboard_mt = b_onboarf_mt_of interceptor_name intercepted_schemas st_onboard in
        let b_onboard_let = auto_fplace (LetStmt (
            b_onboard_mt,
            b_onboard,
            e2_e (CallExpr(
                e2var (Atom.builtin "bridge"),
                [
                    e2var p_onboard
                ]
            ))
        )) in
        
        (generated_bridges, (b_onboard, b_onboard_mt, b_onboard_let))

    (*** Step 2.C - Insert new onboard protocol in lca of intercepted_schemas ***)
    let insert_p_onboard_def key (_, (_,_, p_onboard_def)) (program:program) : program =
            let parents = 
                try 
                    Hashtbl.find interceptor_parents key 
                with Not_found -> failwith "key %s not found in interceptor_parents" key
            in
            IRUtils.insert_terms_into_lca (List.of_seq (AtomOptionSet.to_seq parents)) [auto_fplace (auto_plgannot p_onboard_def)] program   

    (*************** Step 3 - Generate the interceptor factory  ******************)
    (* 
        per context since binding interceptor activation with intercepted_activations, trough b_onboard or b_in, is done inside factory
    *)

    (** 
        @returns a lambda that represents the interceptor factory,
        lambda since the language do not support static method except at toplevel
        and some schemas/types used inside the interceptor factory might not be defined at top-level
        see whitepaper for more details 
    *)
    let generate_interceptor_factory place interceptor (base_interceptor_constructor_params: param list) (generated_bridges, (b_onboard, b_onboard_mt, b_onboard_let))  = 
        (*** Create the factory ***)
        let p_of_i = Atom.fresh "p_of_i" in

        (* return spawn Interceptor( *BaseInterceptor::on_startup_args, b_onboard, b_out_1, b_in_1, ..., b_out_n, b_in_n); *)
        let spawn = 
            {
                c = ce2var interceptor;
                args = 
                    (* *BaseInterceptor::on_startup_args *)
                    List.map (function p -> auto_fplace (VarExpr (snd p.value), fst p.value)) base_interceptor_constructor_params
                    @ [auto_fplace (VarExpr b_onboard, b_onboard_mt)]
                    (* b_out_1, b_in_1, ..., b_out_n, b_in_n *)
                    @ List.flatten (
                        List.map 
                            (function (b_out, b_in, _) -> [
                                e2var b_out; 
                                e2var b_in
                            ])
                            (List.of_seq (Hashtbl.to_seq_values generated_bridges))
                    );
                at = None;
                inline_in = None;
            }
        in


        let body : expr = 
                e2_e (TernaryExpr (
                    e2_e (BinopExpr(
                        e2var p_of_i,
                        Equal,
                        e2_e (OptionExpr None)
                    )),
                    (e2_e (Spawn spawn)),
                    (
                        e2_e (Spawn { spawn with at = Some (e2_e (CallExpr(
                                e2var (Atom.builtin "option_get"),
                                [ e2var p_of_i ]
                            )))}) 
                    )
                ))
        in

        let core_factory = e2_e (LambdaExpr (
            [
                auto_fplace (
                    mtype_of_ct (TOption (mtype_of_ft (TPlace))),
                    p_of_i
                )
            ], 
            body
        )) in


        (***  Wrap core_factory inside function that binds base_interceptor_constructor arguments ***)

        (*    Tail recursive variant - implies to do a List.rev on params before calling make_wraper
        *)
        let rec make_wraper core_factory = function
        | [] -> core_factory
        | {value=(mt, x)}::params -> 
            let res = e2_e (LambdaExpr ( [auto_fplace (mt,x)], core_factory)) in 
            make_wraper res params
        in

        assert((List.of_seq (Hashtbl.to_seq_values generated_bridges)) = []);

        (*** Generate the let ***)
        let factory_targs_wo_p_of_i = 
            (* *BaseInterceptor::on_startup_args *)
            List.map (function p -> fst p.value) base_interceptor_constructor_params
            (* Hidden inside not exposed
            @ [b_onboard_mt]
            (* b_out_1, b_in_1, ..., b_out_n, b_in_n *)
            @ List.flatten (List.map (function (b_out, b_in, b_in_let) -> 
                let mt = match b_in_let.value with
                | LetStmt (mt,_,_) -> mt
                in
                [mt; mt]
            ) (List.of_seq (Hashtbl.to_seq_values generated_bridges)))
            *)
        in 
        let factory_targs = factory_targs_wo_p_of_i @ [mtype_of_ct (TOption (mtype_of_ft (TPlace)))] in

        (* targs -> activation_ref<Interceptor>*) 
        let factory_signature  = mtype_of_fun2 factory_targs (mtype_of_ct (TActivationRef (mtype_of_cvar interceptor))) in

        let factory = Atom.fresh "factory" in
        let local_b_onboard = Atom.fresh (Atom.hint b_onboard) in
        let factory_expr = make_wraper core_factory (List.map (function mt -> auto_fplace (mt, Atom.fresh "arg")) factory_targs_wo_p_of_i) in

        factory, auto_fplace (LetStmt(factory_signature, factory, factory_expr))


    (*************** Step 4 - Ctx elimination  ******************)
    (** 
        Get ride of ``with<Interceptor> ctx() { stmt }``
        see whitepaper for details
    *)

    let ctxelim_rewrite_stmt (program:program) parent_opt place = function 
        | WithContextStmt (anonymous_mod, base_interceptor_name, user_defined_policy, stmts) -> begin 
            (*** Step a - Collect intell on ctx ***)

            (* NB. key_of_ctx is called twice one in prepare and one in rewrite (i.e recompute intercepted_schemas).
                perf: merge rewrite and prepare
                readability: keep them split in two functions => current choice
            *)
            let key = key_of_ctx place base_interceptor_name stmts in
            logger#debug "ctxelim_rewrite: %s" (show_key key);


            let base_interceptor_place, base_interceptor =  get_schema program base_interceptor_name in
            let base_interceptor_constructor : method0 option = get_onstartup base_interceptor in
            let base_interceptor_constructor_params = match base_interceptor_constructor with
                | None -> []
                | Some m -> m.value.args
            in
    
            let intercepted_schemas = intercepted_schemas_of_ctx place base_interceptor_name stmts in (* computed a third time - TODO give it as parameter to ke-of_ctx *)
            let exposed_activations = exposed_activations_of_ctx place base_interceptor_name stmts in
            let intercepted_bridges = intercepted_bridges_of_ctx place base_interceptor_name stmts in

            (* Get interceptor specialized schema for this context
                it has been previously generated by Step 1 
            *)
            let interceptor_assign, (p_onboard, st_onboard, p_def_onboard) = 
                try 
                    Hashtbl.find interceptor_makes key
                with Not_found -> failwith "key %s not found in interceptor_parents" key
            in 
            let interceptor_name = match interceptor_assign.value with
                | ComponentAssign cassign -> cassign.name
            in

            (* TODOC *)
            (* Sanity checks - i.e. detects that interception ctx is not used and raise an error *)
            if Atom.Set.is_empty intercepted_schemas then Error.perror place "Interception context intercepts no component schema !!";
            
            (*** Step b.1 - specialize policy type -> TODO FIXME HOT FIX for Java like target where Interceptor and SpecializedInterceptor are not subtypes ***)
            let user_defined_policy_name = 
                match fst user_defined_policy.value with 
                | AccessExpr(_, {value=VarExpr a, _}) -> a
            in
            
            let specialized_defined_policy = Atom.fresh (Atom.to_string user_defined_policy_name) in
            
            Hashtbl.add to_specialized_defined_policy user_defined_policy_name (
                match Hashtbl.find_opt to_specialized_defined_policy user_defined_policy_name with
                | None -> [(interceptor_name, specialized_defined_policy)]
                | Some set -> (interceptor_name, specialized_defined_policy) :: set
            );

            let user_defined_policy = e2_e (AccessExpr(
                e2_e This,
                e2var specialized_defined_policy
            )) in

            (*** Step b.2 - Forge ctx headers ***)
            let (generated_bridges, (b_onboard, b_onboard_mt, b_onboard_let)) = generate_bridges place interceptor_name intercepted_schemas p_onboard (mtype_of_st st_onboard.value) intercepted_bridges in
            let factory, factory_let = generate_interceptor_factory place interceptor_name base_interceptor_constructor_params (generated_bridges, (b_onboard, b_onboard_mt, b_onboard_let)) in 

            let headers : stmt list = 
                [ b_onboard_let ]
                (* let b_in_i ;*)
                @ ( 
                    List.map (function (_, _, b_in_let) -> b_in_let) (List.of_seq (Hashtbl.to_seq_values generated_bridges))
                )
                @ [ factory_let ]
            in  

            (*** Step c - assign interceptor per activation *)
            (*  spawn A() @ p; 
                ->
                place p_of_a;      
                activation_ref<Interceptor> i_a = make_interceptor(factory, A.schema, p_of_a);
            *)

            (* 
                a -> (schema_of_a, p_of_a_opt, a', i_a) where a' is the new name of a inside the ctx - in order not to leak it and i_a is the identity of the interceptor in charge of a'.
                Whereas a is the proxy of a' for the outside if exposed.
            *)
            let exposed_activations_info = Hashtbl.create 16 in


            (* TODO must be consistent with the fct computing intercepted_activations => dedup code somehow see intercepted_activations_of_stmt *)
            let spawn_selector = function
                | Spawn _ -> true
                | _ -> false
            in

            let stmt_spawn_selector stmt = 
                (* With one or more spawn inside *)

                (*
                    * register headers for this stmt
                    * hydrated exposed_activations_info for each spawn 
                    * rewrite each spawn
                *)
                let spawn_collector _ _ _ = [true] in

                let _, collected_elts, _ = collect_expr_stmt None Atom.Set.empty spawn_selector spawn_collector (auto_fplace stmt) in

                (* A spawn exsists in stmt *)
                List.length collected_elts > 0  
            in

            let stmt_spawn_rewriter parent_opt place stmt = 

                (* TODO must be consistent with the fct computing intercepted_activations => dedup code somehow see intercepted_activations_of_stmt *)
                let spawn_selector = function
                    | Spawn _ -> true
                    | _ -> false
                in

                (*
                    for each spawn (named a for the doc) in stmt
                    N.B spawn can be anonymous
                    * optional let p_of_a
                    * let i_a = ...
                *)
                let stmt_headers = ref [] in

                (*
                    - triggers the onboarding of a in i_a
                *)
                let stmt_footers = ref [] in

                (*
                    * rewrite each spawn
                    * register headers for this stmt
                    * hydrated exposed_activations_info for each spawn 

                    exposed_id = some a if let a = spawn 
                *)
                let spawn_rewriter (a': Atom.atom) (exposed_info: (Atom.atom * Atom.atom) option) parent_opt mt = function
                    | Spawn spawn -> begin 
                        match fst spawn.c.value with
                        | VarCExpr schema_a -> begin 
                            let i_a = Atom.fresh ("i_a") in
                            let p_of_a = Atom.fresh "p_of_a" in

                            begin
                                (* Hydrate exposed_activations_info *)
                                match exposed_info with 
                                | Some (a,a') ->
                                    Hashtbl.add exposed_activations_info 
                                        a 
                                        (
                                            schema_a,
                                            Option.map (function _ -> p_of_a) spawn.at, 
                                            a', 
                                            i_a 
                                        )
                                | None -> ()
                            end;

                            stmt_headers := !stmt_headers @  
                            [
                                (* Store a copy of p_of_a is any *)
                                LetStmt(
                                    mtype_of_ft TPlace,
                                    p_of_a,
                                    match spawn.at with
                                    | Some at -> Option.get spawn.at
                                    | None ->
                                        e2_e (CallExpr (
                                            e2var (Atom.builtin "current_place"),
                                            []
                                        ))
                                );
                                (* Get the interceptor in charge of a *)
                                LetStmt(
                                    mtype_of_ct (TActivationRef (mtype_of_cvar interceptor_name)),
                                    i_a,
                                    e2_e (CallExpr(
                                        user_defined_policy, 
                                        List.map (function x -> auto_fplace (VarExpr (snd x.value), fst x.value) ) base_interceptor_constructor_params
                                        @ [
                                            e2var factory;
                                            schema_to_label fplace (schema_of  spawn.c);
                                            e2var p_of_a
                                        ]
                                    ))
                                ) 
                            ]; 
                            


                            

                            stmt_footers := !stmt_footers @ [
                                ExpressionStmt(
                                    e2_e (CallExpr(
                                        e2_e (AccessExpr(
                                            e2_e This,
                                            e2var (do_onboard_of (Option.get parent_opt) (schema_of  spawn.c) interceptor_name)
                                        )),
                                        [
                                            e2var i_a;
                                            e2var a';
                                            e2var p_of_a;
                                        ]
                                    ))
                                )
                            ];
                            
                            if exposed_info = None then (
                                stmt_headers := !stmt_headers @ [
                                    LetStmt(
                                        mtype_of_ct (TActivationRef mt),
                                        a',
                                        e2_e (Spawn { spawn with
                                            at = Option.map (function _ -> e2var p_of_a) spawn.at
                                        })
                                    )
                                ];
                                VarExpr a'
                            )
                            else
                                (* Need to replace at if exists, in case at expr is not idempotent or costly *)
                                Spawn { spawn with
                                    at = Option.map (function _ -> e2var p_of_a) spawn.at
                                }
                        end 
                        | _ -> Error.perror place "spawn first arg should have been reduce into a cexpr value (i.e. component name)"
                    end
                in

                (* TODO logic duplicated with exposed_activation*)
                match stmt with 
                (* Exposed activations must be identified to hydrate corretly the exposed_activations_info *)
                | LetStmt (mt, a, ({value=Spawn spawn, _} as spawn_e)) ->
                    (* Replace a by a' to ensure that this pass guarantee `` a binder create a unique named variable`` even if we reintroduce a binder for [a] in ctx footer *)
                    (* NB. variable in ctx body will be replaced afterward using expose_activations_info *)
                    let a' = Atom.fresh (Atom.value a) in

                    (* Rewrite args of exposed spawn *)
                    let args = List.map (rewrite_expr_expr parent_opt spawn_selector (spawn_rewriter a' None) ) spawn.args in 

                    let spawn = { spawn with args } in

                    (* Exposed_activation *) 
                    let e = {
                        place = spawn_e.place @ fplace; 
                        value = spawn_rewriter a' (Some (a, a')) parent_opt (snd spawn_e.value) (fst spawn_e.value), (snd spawn_e.value)} in

                    !stmt_headers @ [ LetStmt(mt, a', e)] @ !stmt_footers

                (* Non-exposed activations *)
                | _ -> 
                    let _stmt = (rewrite_expr_stmt parent_opt spawn_selector (spawn_rewriter (Atom.fresh "a") None) (auto_fplace stmt)).value  in
                    !stmt_headers @ [_stmt] @ !stmt_footers
            in 

            (* Rewrite intercepted_activations and hydrate exposed_activations_info *)
            let stmts = List.map (function stmt -> rewrite_stmt_stmt false parent_opt stmt_spawn_selector stmt_spawn_rewriter stmt) stmts in
            let stmts = List.flatten stmts in

            (*  apply renaming a -> a', for exposed_activations, in ctx body.
                N.B. binders have been renamed just before.
            *)
            let stmts = 
                List.map 
                    (
                        rewrite_expr_stmt 
                            parent_opt
                            (function 
                                | VarExpr a -> Hashtbl.find_opt exposed_activations_info a <> None 
                                | _ -> false) 
                            (fun _ mt -> function 
                                | VarExpr a -> 
                                    let (_, _, a', _) = 
                                        try
                                            Hashtbl.find exposed_activations_info a 
                                        with Not_found -> failwith "activation %s marked as exposed but not found in exposed_activation_info" (Atom.to_string a) 
                                    in 
                                    VarExpr a'
                            )
                    )
                    stmts
            in

            (*** Step d - ctx footer ***)
            (*
                Wrap identity of exposed_activations according to ctx option (anonymous or not)

                add at the end  [let a = [InterceptedActivationRef(i, a')] if identity of [a'] should be exposed else [VarExpr i]]
            *)
            let footer_binders = Hashtbl.fold (fun  a (schema_a, _, a', i_a) footers ->
                let mt_a = mtype_of_ct (TActivationRef (mtype_of_cvar schema_a)) in

                let binded_value = e2_e (
                    InterceptedActivationRef (
                        e2var i_a,
                        if anonymous_mod then None
                        else Some (e2var a')
                    )
                ) in

                (*  Preserve name and type for the outside world 
                    see whitepaper for detailed discussion of type and subtyping
                *)
                (auto_fplace (LetStmt( 
                    mt_a, 
                    a,  
                    binded_value
                ))) :: footers
            ) exposed_activations_info [] in


            (*** Update interceptors_info ***)
            if Hashtbl.find_opt interceptors_info interceptor_name = None then
                Hashtbl.add interceptors_info interceptor_name {
                    from_ctx_elim = true;

                    name = interceptor_name;
                    base_interceptor_name = base_interceptor_name;
                    base_interceptor_place; 

                    onboard_info = {st_onboard; b_onboard_mt};
                    inout_bridges_info = List.map (function (b_out, b_int, b_in_let) ->
                        b_out, 
                        b_int,
                        match b_in_let.value with
                        | LetStmt (mt,_,_) -> mt
                    ) (List.of_seq (Hashtbl.to_seq_values generated_bridges));

                    intercepted_schemas;

                    (*** Not hydrated by ctx_elim ***)
                    (*** Hydrated by intercept elim***)
                    this_onboarded_activations = None;
                    this_port_onboard = None;
                    b_onboard_state = None;
                    inout_statebridges_info = None;
                    sessions_info = None;
                };

            let stmts = headers @ stmts @ (List.rev footer_binders) in
            List.map (function stmt -> stmt.value) stmts
        end

    (*************** Step 5 - Add logic to parent of ctx  ******************)
    (* Ctx add do_onboard method for triggering child onboarding *)

    let generate_onboards_of parent_schema intercepted_schema interceptor_schema f_name =
        let l__i_a = Atom.fresh "i_a" in
        let l__a = Atom.fresh "a" in 
        let l__p_of_a = Atom.fresh "p_of_a" in

        let l__s0 = Atom.fresh "s_a" in
        let l__s1 = Atom.fresh "s_b" in
        let l__s2 = Atom.fresh "s_c" in

        let interceptor_info = Hashtbl.find interceptors_info interceptor_schema in 

        Method (auto_fplace {
            annotations = [];
            ret_type = mtype_of_ft TVoid;
            name = f_name;
            args = [
                auto_fplace (mtype_of_cvar interceptor_schema, l__i_a);
                auto_fplace (mtype_of_cvar intercepted_schema, l__a);
                auto_fplace (mtype_of_ft TPlace, l__p_of_a);
            ];
            ghost = false;
            contract_opt = None;
            on_destroy = false;
            on_startup = false;
            body = [
                auto_fplace(LetStmt(
                    mtype_of_st interceptor_info.onboard_info.st_onboard.value,
                    l__s0,
                    e2_e(CallExpr(
                        e2var (Atom.builtin "initiate_session_with"),
                        [
                            e2_e(AccessExpr(
                                e2_e This,
                                e2var (Option.get interceptor_info.this_port_onboard)
                            ));
                            e2var l__i_a
                        ]
                    ))
                ));
                auto_fplace(LetStmt(
                    mtype_of_st interceptor_info.onboard_info.st_onboard.value,
                    l__s1,
                    e2_e(CallExpr(
                        e2var (Atom.builtin "select"),
                        [
                            e2var l__s0;
                            e2_lit (BLabelLit intercepted_schema)
                        ]
                    ))
                ));
                (* ?bool l__s2 = fire(l__s1, ...)?; *)
                auto_fplace(LetStmt(
                    mtype_of_st (STRecv (mtype_of_ft TBool, auto_fplace STEnd)),
                    l__s2,
                    e2_e(CallExpr(
                        e2var (Atom.builtin "fire"),
                        [
                            e2var l__s1;
                            e2_e( BlockExpr(
                                Tuple,
                                [e2var l__a; e2var l__p_of_a]
                            ))
                        ]
                    ))
                ));
                (*TODO failed if onboard refused *)
                auto_fplace(ExpressionStmt(
                    e2_e(CallExpr(
                        e2var (Atom.builtin "recv"),
                        [
                            e2var l__s2;
                        ]
                    ))
                ))
            ]
        })     

    let ctxelim_rewrite_parent program : program =
        let parent_selector (cstruct:component_structure) = 
            (Hashtbl.find_opt do_onboards_of cstruct.name) <> None 
        in

        let parent_rewriter parent_parent_opt place (cstruct:component_structure) : component_structure list = 
            let htbl = Hashtbl.find do_onboards_of cstruct.name in

            let methods = List.map 
                (function ((intercepted_schema, interceptor_schema), f_name) -> 
                    auto_fplace (auto_annote(generate_onboards_of cstruct.name intercepted_schema interceptor_schema f_name))
                )
                (List.of_seq (Hashtbl.to_seq htbl))
            in

            [{cstruct with body = cstruct.body @ methods }]
        in

        rewrite_component_program parent_selector parent_rewriter program 


    (****************** Step 5 specialize policy ************************)
    let specialize_user_defined_policies program = 
        let policy_selector = function 
            | Method m -> Hashtbl.mem to_specialized_defined_policy m.value.name
            | _ -> false
        in
        let policy_rewriter place = function
            | Method m -> begin
                let specialized_ms = List.map 
                    (function (interceptor_name, specialized_policy_name) ->
                        logger#error "Specialized policy %s for %s" (Atom.to_string specialized_policy_name) (Atom.to_string interceptor_name); 
                        (* rename body *)
                        let freevars, _ = free_vars_component_item Atom.Set.empty (auto_fplace (auto_annote(Method m))) in
                        let freetvars, _ = free_tvars_component_item ~flag_tcvar:true  Atom.Set.empty (auto_fplace (auto_annote(Method m))) in

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
                        let body = List.map (rename_stmt ~flag_rename_attribute:true true renaming) m.value.body in


                        Method (auto_fplace {
                            annotations = [];
                            ghost = false;
                            ret_type = mtype_of_ct (TActivationRef (mtype_of_cvar interceptor_name));
                            name = specialized_policy_name; 
                            args = 
                            List.map (map_place(fun _ (mt, x) -> mt, renaming x))
                            ((
                                let rec _update_signature_ret_type place = function
                                | CType {place; value=TArrow(x, {value=CType{value=TActivationRef _}})} -> 
                                    CType {
                                        place = place @ fplace; 
                                        value = TArrow(x, mtype_of_ct (TActivationRef (mtype_of_cvar interceptor_name)))
                                    }
                                | CType{value=TArrow (x, y)} ->
                                    CType{place = place; value = TArrow(x, update_signature_ret_type y)}
                                and update_signature_ret_type mt = map_place _update_signature_ret_type mt
                                in
                                
                                auto_fplace (update_signature_ret_type (fst (List.hd m.value.args).value), (snd (List.hd m.value.args).value))
                            ):: (List.tl m.value.args));
                            body = body;
                            contract_opt = None;
                            on_startup = false;
                            on_destroy = false;
                        })    
                    )
                    (Hashtbl.find to_specialized_defined_policy m.value.name)
                in

                (Method m) :: specialized_ms
            end
            | t -> [t]
        in
        
        rewrite_citem_program policy_selector policy_rewriter program


    (****************** Main CTX Elim ************************)
    let ctxelim_program (program : program) : program= 
        (* Hydrate interceptor_makes/parents 
            NB. could be merged but are split for readability
        *)
        logger#debug "CtxElim: collecting info";
        let _ = collect_stmt_program (function | WithContextStmt _ -> true | _ -> false) ctxelim_prepare_stmt program in

        (* Apply the ctx elimination *)
        logger#debug "CtxElim: applying rewriting";
        let program = rewrite_stmt_program true (function | WithContextStmt _ -> true | _ -> false) (ctxelim_rewrite_stmt program) program in

        let program = ctxelim_rewrite_parent program in

        (* Specialize polices *)
        logger#debug "CtxElim: specialize policies";
        let program =  specialize_user_defined_policies program in


        (* Insert possibly shared definitions between multiple ctx (and parent schemas) *)
        logger#debug "CtxElim: insertion of shared definitions";
        let program = Hashtbl.fold insert_interceptor_dcl interceptor_makes program in
        let program = Hashtbl.fold insert_p_onboard_def interceptor_makes program in

        program

    (*********************************************************)
    let name = "Interception.ContextElimination"
    let displayed_pass_shortdescription = "interception ctx has been eliminated from IR"
    let displayed_ast_name = "interception-ctx-eliminated IR"
    let show_ast = true
    let global_at_most_once_apply = false


    let precondition program = program

    let withcontext_selector = function
        | WithContextStmt _ -> true
        | _ -> false
    let postcondition program = 
        (* Check: no WithContextStmt *)
        ignore (collect_stmt_program withcontext_selector (failure_collector_e2 "WithContextStmt remains in IR") program);

        program
    let apply_program = ctxelim_program
end