open InterceptUtils
open Core
open AstUtils
open IR
open Easy_logging

let logger = Logging.make_logger ("_1_ compspec.Intercept") Debug [];;

let fplace = (Error.forge_place "Intercept.ContextElimination" 0 0) 
let auto_fplace smth = {place = fplace; value=smth}

module Make = struct
    include AstUtils2.Mtype.Make(struct let fplace = fplace end)

    (******************* Shared state of the pass **************************)
    (* 
        State used to deduplicate and place, interceptor specialization i.e. interception make functor
    *)

    (* key for dedup *)
    type interceptor_key = Atom.t * Atom.Set.t

    (* 
        Mapping from key to 
        - the MakeInterceptor stmt used to create the key related interceptor schema
        - the onboarding protocol name
        - the onboarding session type
        - the onboarding protocol def
    *)
    let interceptor_makes : (interceptor_key, (component_dcl * (Atom.atom * session_type * _term))) Hashtbl.t = Hashtbl.create 16 

    (* 
        Store the schemas (called parents) that owns a ctx using the interceptor schema binded to the key
        * key -> parent set
    *)
    let interceptor_parents : (interceptor_key, Atom.Set.t) Hashtbl.t = Hashtbl.create 16

    let show_key (cname, cnames) = 
        Printf.sprintf "Key<%s><%s>" (Atom.show cname) (Atom.Set.show cnames) 




    (*************** Step 0 - gather intell ******************)

    (*** Step 0.A - intercepted_schemas + exposed_actvations ***)


    module InterceptedActivationSet = Set.Make(struct 
        type t = Atom.atom * Atom.atom (* (schema, activation variable) *)
        let compare (a, b) (c, d) = 
            let r1 = Atom.compare a b in
            let r2 = Atom.compare c d in

            if r1 = r2 then 0
            else 
                if r1 = -1 * r2 then r1 - r2 
                else r1 + r2 
    end)

    (*
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
        let collector parent_opt place = function
            | Spawn spawn -> begin 
                match fst spawn.c.value with
                | VarCExpr c -> [c]
                | _ -> Error.error place "spawn first arg should have been reduce into a cexpr value (i.e. component name)"
            end
            | _ -> []
        in 


        let intercepted_schemas = collect_stmt_stmt None selector collector stmt in

        Atom.Set.of_seq (List.to_seq intercepted_schemas)

    (*
        cname - base interceptor name
        stmts - ctx body 
    *)
    let intercepted_schemas_of_ctx place cname stmts : AtomSet.t =
        let intercepted_schemas_sets, _ = List.map (intercepted_schemas_of_stmt place cname) stmts in
        List.fold_left (fun acc set -> Atom.Set.union acc set) AtomSet.empty intercepted_schemas_sets


    (*
        Returns the set of intercepted_schemas
        N.B. 
            - works because binders introduce unique variable names
            - only capture direct intercepted_schemas (see interception whitepaper for details)
    *)
    let intercepted_activations_of_stmt place cname stmt = 
        let selector = function
            (* direct *)
            | Spawn _ -> true
            (* indirect = indirect-call *)
            (* TODO *)

            | _ -> false
        in
        let collector parent_opt place = function
            | Spawn spawn -> begin 
                match fst spawn.c.value with
                | VarCExpr c -> [c]
                | _ -> Error.error place "spawn first arg should have been reduce into a cexpr value (i.e. component name)"
            end
            | _ -> []
        in 


        let intercepted_activations = collect_stmt_stmt None spawn_selector spawn_collector stmt in

        Atom.Set.of_seq (List.to_seq intercepted_activations)

    (*
        cname - base interceptor name
        stmts - ctx body 
    *)
    let intercepted_activations_of_ctx place cname stmts : AtomSet.t =
        let intercepted_activations_sets, _ = List.map (intercepted_activations_of_stmt place cname) stmts in
        List.fold_left (fun acc set -> Atom.Set.union acc set) AtomSet.empty intercepted_activations_sets

    (*
        Returns set of exposed_actvations : (schema, atom) Set - 
        N.B. 
            - works because binders introduce unique variable names
            - only capture direct-simpl exposed activations (see interception whitepaper for details)
    *)
    let exposed_actvations_of_ctx place cname stmt : InterceptedActivationSet.t = 
        let selector = function
            (* direct-simpl*)
            | LetExpr (_, _, {value=Spawn _, _}) -> true

            (* direct-complex*)
            (* TODO not processed neeed control flow - TODOC search spawn or activation in left hand side of a let then search where the activation is spawned then decide*)

            (* no direct exposure since inner binders can leak out ctx stmt *) 
            | ForStmt (_, _, _, _) | IfStmt(_,_,_) | MatchStmt (_,_) | BlockStmt _ | WithContextStmt (_,_,_,_)  -> false 

            (* indirect *)
            (* TODO *)

            | _ -> false
        in
        let collector parent_opt place = function
            | LetExpr (_, x, {value=Spawn spawn, _}) -> begin 
                match fst spawn.c.value with
                | VarCExpr c -> [(c, x)]
                | _ -> Error.error place "spawn first arg should have been reduce into a cexpr value (i.e. component name)"
            end
            | _ -> []
        in 

        let exposed_actvations = collect_stmt_stmt None selector collector stmt in
        InterceptedActivationSet.of_seq (List.of_seq exposed_actvations)

    (*
        cname - base interceptor name
        stmts - ctx body 
    *)
    let exposed_actvations_of_ctx place cname stmts : InterceptedActivationSet.t =
        let exposed_actvations_sets, _ = List.map (exposed_actvations_of_stmt place cname) stmts in
        List.fold_left (fun acc set -> Atom.Set.union acc set) AtomSet.empty exposed_actvations_sets


        (* 
            TODOC
            * Interception egress/ingress communication: Direct/Indirect spawns have only knowledge of intercepted bridges (interceptor <--> internals) - scope mechanism
            * Interception identity: Binders inside inner stmts can not escape and (direct/indirect) spawns in control-flow expression (e.g. ``if (e) {}``) can not be binded  

            exposed_actvations - for handling identity exposure but not for actual msg interception (based on bridge renaming), i.e activation captured by binders (
                direct ``let a = spawn ...`` 
                direct-complex ``let res = (1, spawn ...)``
                indirect-call ``let ... = f()`` where f generate an activation - can be recursive
                indirect-assign ``somemap[key] = spawn ...`` - indirect binded if somewhere let .. = somemap[somekey] or if inherited from ctx parent scope (i.e. defined externaly) and not a binder (i.e. not in exposed_actvations) if there is no ``let ... = somemap[...]`` + not inherited
                    - could have and indirect-assign behind a call (fct_assign(spawn ..., somemap))
            )

            intercepted_schemas - needed to make interceptor <: Internals ; introduce custom logic to avoid building something like union of left/right intercepted_bridges

            relation between both
            * schemas(exposed_actvations) \subset intercepted_schemas
            * intercepted_schemas \not\subset schemas(exposed_actvations) in the general case because all activation not binded spawned in body are part of intercepted components but not exposed_actvations ``e.g. spawn A(...)`` 
        *)

    module InterceptedBridgesSet = Set.Make(struct 
        type t = composed_type * atom (* TBridge, name *)
        let compare (_, b) (_, d) = Atom.compare b d
    end)

    (*
        Returns set of intercepted_bridges : (bridge type:TBridge, atom) Set - 
        N.B. 
            - works because binders introduce unique variable names
            - complet capture
    *)
    let intercepted_bridges_of_ctx place cname stmts : InterceptedActivationSet.t = 
        let fvars = List.amp (function stmt -> snd (free_vars_stmt Atom.Set.empty stmt)) stmts in
        let fvars_htbl = Hashtbl.create 16 in
        List.iter (function (mt, x) -> Hashtbl.add fvars_htbl x mt) fvars;

        let intercepted_bridges = List.map (function (mt,x) ->
            match mt.value with (* FIXME do not work if m.value is an alias of bridge -> us is_subtype or type equal if they handle aliasing *)
            | CType{ value = TBridge _ as tb} -> (Some tb, x)
            | _-> None
        ) in

        List.filter (function opt -> opt <> None) intercepted_bridges
        InterceptedBridgesSet.of_seq (List.of_seq intercepted_bridges)

    (*
        cname - base interceptor name
        stmts - ctx body 
    *)
    let intercepted_bridges_of_ctx place cname stmts : InterceptedActivationSet.t =
        let intercepted_bridges_sets, _ = List.map (intercepted_bridges_of_stmt place cname) stmts in
        List.fold_left (fun acc set -> Atom.Set.union acc set) AtomSet.empty intercepted_bridges_sets



    (*************** Step 1 - Specialized interceptor generation ******************)

    (*** Step 1.A - TODO ***)

    (*  Generates an interceptor key per context 
        key : (cname, set of intercepted_schemas)
    *)
    let key_of_ctx place cname stmts = 
        let intercepted_schemas_sets, _ = List.map (intercepted_schemas_of_stmt place cname) stmts in
        let intercepted_schemas = List.fold_left (fun acc set -> Atom.Set.union acc set) AtomSet.empty intercepted_schemas_sets in 

        let key = (cname, intercepted_schemas) in
        key

    let interceptor_name_of_ctx place cname stmts = 
        Atom.fresh (Atom.value cname)


    (*** Step 1.B - Generation  ***)

    (*
        Generates ``component InterceptorXX = MakeInterceptor(BaseInterceptor, intercepted_schemas)``;
    *)
    let generate_make_schema place cname stmts intercepted_schemas = 
        let interceptor_name = intercepted_schemas_of_ctx place cname stmts in

        auto_fplace (ComponentAssign {
            name = interceptor_name;
            args = []; (* TODO Remove args and create a dedicated type for functor *)
            value = auto_fplace (
                AppCExpr (
                    auto_fplace (VarCExpr (Atom.builtin "MakeInterceptor"), auto_fplace EmptyMainType), 
                    [
                        auto_fplace (VarCExpr cname, auto_fplace EmptyMainType);
                        auto_fplace (AnyExpr 
                            (auto_fplace (BlockExpr(
                                AstUtils.List,
                                List.map (function x -> 
                                    auto_fplace (BoxCExpr (auto_fplace (VarCExpr x, auto_fplace EmptyMainType)), auto_fplace EmptyMainType)
                                ) intercepted_schemas
                            ), auto_fplace EmptyMainType)
                        ), auto_fplace EmptyMainType)
                    ]
                ), auto_fplace EmptyMainType
            );
        })

    let generate_onboard_typedef place cname intercepted_schemas = 
        let onboard_st_branch_of schema = 
            (* ('A': !tuple<activation_ref<A>, place>> ?bool.) *)
            schema, auto_fplace (STSend(
                mtype_of_ct (TTuple [
                    mtype_of_ct (TActivationRef (mtype_of_cvar schema)),
                    mtype_of_ft (TPlace)
                ]),
                auto_fplace (STRecv (mtype_of_ft (TFlatType TBool)))
            )), None
        in
        let st_on_board = STBranch ( List.map onboard_st_branch_of intercepted_schemas) in
        let p_onboard = Atom.fresh ("p_onboard_"^(Atom.value interceptor_name)) in 
        let p_def_onboard = ProtocolDef (p_onboard, mtype_of_st st_onboard) in
        (p_onboard, p_def_onboard)
    (*
        Register each ctx inside shared state (interceptor_parents, interceptor_makes) in order to deduplicate schemas.
    *)
    let ctxelim_prepare_stmt parent_opt place i= function
        | WithContextStmt (anonymous_mod, cname, e, stmts) -> 
            let key = key_of_ctx place cname stmts in
            let intercepted_schemas = intercepted_schemas_of_ctx place cname stmts in

            logger#debug "ctxelim_prepare: %s" (show_key key);

            (* Deduplicate interceptor component type *)
            match Hashtbl.find_opt interceptor_makes key with
            | Some _ -> 
                Hashtbl.add interceptor_parents key (parent_opt::(Hashtbl.find interceptor_parents key));
                [] 
            | None -> begin

                let make_schema_stmt = generate_make_schema place cname stmts intercepted_schemas in
                let (st_onboard, p_def_onboard) = generate_onboard_typedef place cname intercepted_schemas in

                Hashtbl.add interceptor_parents key [parent_opt];
                Hashtbl.add interceptor_makes key (make_schema_stmt, (p_onboard, st_onboard, p_def_onboard));
                []
            end

    (*** Step 1.C - Insert new schema in lca of intercepted_schemas ***)
    let insert_interceptor_dcl key (interceptor_dcl,_) (program:program) : program =
        let parents = Hashtbl.find interceptor_parents key in
        insert_in_ancestor parents (auto_fplace (Component interceptor_dcl)) program   

    (*************** Step 2 - Bridge handling  ******************)

    let mt_internals_of place intercepted_schemas = 
        (* Already checked by main ctx_elim *)
        assert(intercepted_schemas <> []);
        List.fold_left 
            (fun mt schema -> 
                mtype_of_ct (TUnion (mt, mtype_of_cvar schema))
            )
            (mtype_of_cvar (List.hd intercepted_schemas))
            (List.tl intercepted_schemas)


    (*  Per context and not (per interceptor schema or per interceptor activation) 
        - p_onboard/intercepted_bridges are defined in a per interceptor schema base
    *)
    let generate_bridges place interceptor_name intercepted_schemas p_onboard intercepted_bridges =
        assert(intercepted_bridges <> []);
        (*** b_in ***)
        (* b -> (b_ext = b, b_in, b_in_constructor)*)
        let generated_bridges = Hashtbl.create 16 in
        InterceptedBridgesSet.iter (function (tbridge, b) -> 
            let b_in = Atom.fresh ((Atom.value b)^"_in") in

            (* FIXME default constructor - see whitepaper for improvement *)
            let b_in__constructor = auto_fplace (CallExpr(
                auto_fplace (VarExpr (Atom.builtin "bridge"), auto_fplace EmptyMainType)
                []
            )) in

            let b_in__let = LetExpr (
                mtype_of_ct tbridge,
                b_in,
                b_in__constructor
            ) in

            Hashtbl.add generate_bridges b (b, b_in, b_in__let)
        );

        (*** b_onboard ***)
        let b_onboard = Atom.fresh ("b_onboard_"^(Atom.value interceptor_name)) in 
        let b_onboard_let = LetExpr (
            mtype_of_ft (TBridge {
                in_type = mt_internals_of intercepted_schemas;
                out_type = mtype_of_cvar interceptor_name;
                protocol = p_onboard;
            }),
            b_onboard,
            auto_fplace (CallExpr(
                auto_fplace (VarExpr (Atom.builtin "bridge"), auto_fplace EmptyMainType)
                []
            ))
        ) in
        
        (generated_bridges, (b_onboard, b_onboard_let))

    (*** Step 2.C - Insert new onboard protocol in lca of intercepted_schemas ***)
    let insert_p_onboard_def key (_, (_,_, p_onboard_def)) (program:program) : program =
        let parents = Hashtbl.find interceptor_parents key in
        insert_in_ancestor parents (p_onboard_def) program   

    (*************** Step 3 - Generate the interceptor factory  ******************)
    (* 
        per context since binding interceptor activation with intercepted_actvations, trough b_onboard or b_in, is done inside factory
    *)

    (* 
        Returns a lambda that represents the interceptor factory,
        lambda since the language do not support static method except at toplevel
        and some schemas/types used inside the interceptor factory might not be defined at top-level
        see whitepaper for more details 
    *)
    let generate_interceptor_factory place interceptor base_interceptor_constructor_params (generated_bridges, (b_onboard, b_onboard_let))  = 
        (*** Create the factory ***)
        let p_of_i = Atom.fresh "p_of_i" in

        (* return spawn Interceptor( *BaseInterceptor::on_startup_args, b_onboard, b_out_1, b_in_1, ..., b_out_n, b_in_n); *)
        let spawn = 
            Spawn {
                c = auto_fplace(VarCExpr interceptor, auto_fplace EmptyMainType);
                args = 
                    (* *BaseInterceptor::on_startup_args *)
                    List.map snd base_interceptor_constructor_params
                    @ [match b_onboard_let with
                    | LetExpr (mt,_,_) -> mt]
                    (* b_out_1, b_in_1, ..., b_out_n, b_in_n *)
                    @ List.flatten (List.map (function (b_out, b_in, _) -> [b_out; b_in]) (List.of_seq (Hashtbl.to_seq generated_bridges)));
                at = None;
            }
        in


        let body = 
            auto_fplace (IfStmt (
                auto_fplace (BinOpExpr(
                    auto_fplace (VarExpr p_of_i, auto_fplace EmptyMainType),
                    StructuralEqual,
                    auto_fplace (OptionExpr None, auto_fplace EmptyMainType)
                ), auto_fplace EmptyMainType),
                auto_fplace (ReturnStmt spawn),
                Some (auto_fplace (ReturnStmt (
                    { spawn with at = auto_fplace (CallExpr(
                            auto_fplace (VarExpr (Atom.builtin "option_get"), auto_fplace EmptyMainType),
                            [ auto_place (VarExpr p_of_i, auto_fplace EmptyMainType) ]
                        ), auto_fplace EmptyMainType)} 
                )))
            ))
        in

        let core_factory = LambdaExpr (
            p_of_i,
            mtype_of_ct (TOption (mtype_of_ft (TPlace))),
            body
        ) in


        (***  Wrap core_factory inside function that binds base_interceptor_constructor arguments ***)

        (*    Tail recursive variant - implies to do a List.rev on params before calling make_wraper
        *)
        let rec make_wraper core_factory =  function
        | [] -> core_factory
        | (mt, x)::params -> 
            let res = auto_fplace (LambdaExpr (mt, x, core_factory)) in 
            make_wraper res params
        in

        (*** Generate the let ***)
        let factory_signature = 
            (* *BaseInterceptor::on_startup_args *)
            List.map fst base_interceptor_constructor_params
            @ [b_onboard_mt]
            (* b_out_1, b_in_1, ..., b_out_n, b_in_n *)
            @ List.flatten (List.map (function (b_out, b_in, b_in_let) -> 
                let mt = match b_in_let with
                | LetExpr (mt,_,_) -> mt
                in
                [mt; mt]
            ) (List.of_seq (Hashtbl.to_seq generated_bridges)));
        in 
        let factory = Atom.fresh "factory" in
        let factory_expr = make_wraper core_factory (List.rev base_interceptor_constructor_parms) in

        factory, auto_fplace (LetExpr(factory_signature, factory, factory_expr))


    (*************** Step 4 - Ctx elimination  ******************)
    (* 
        Get ride of ``with<Interceptor> ctx() { stmt }``
        see whitepaper for details
    *)

    let ctxelim_rewrite_stmt program place = function 
        | WithContextStmt (anonymous_mod, cname, user_defined_policy, stmts) -> begin 
            (*** Step a - Collect intell on ctx ***)

            (* NB. key_of_ctx is called twice one in prepare and one in rewrite (i.e recompute intercepted_schemas).
                perf: merge rewrite and prepare
                readability: keep them split in two functions => current choice
            *)
            let key = key_of_ctx place cname stmts in
            logger#debug "ctxelim_rewrite: %s" (show_key key);


            let base_interceptor : component_structure = Utils.get_schema program interceptor_name in
            let base_interceptor_constructor : method0 = Utils.get_onstartup base_interceptor.body in

            let intercepted_schemas = intercepted_schemas_of_ctx place cname stmts in (* computed a third time - TODO give it as parameter to ke-of_ctx *)
            let intercepted_activations = intercepted_activations_of_ctx place cname stmts in
            let exposed_activations = exposed_activations_of_ctx place cname stmts in
            let intercepted_bridges = intercepted_bridges_of_ctx place cname stmts in

            (* Get interceptor specialized schema for this context
                it has been previously generated by Step 1 
            *)
            let interceptor_assign, (p_onboard, st_onboard, p_def_onboard) = Hashtbl.find interceptor_makes key in 
            let interceptor_name = match interceptor_assign.value with
                | ComponentAssign cassign -> cassign.name
            in

            (* TODOC *)
            (* Sanity checks - i.e. detects that interception ctx is not used and raise an error *)
            if intercepted_bridges = [] then Error.error place "Interception context intercepts no bridges !!";
            if intercepted_schemas = [] then Error.error place "Interception context intercepts no component schema !!";
            if intercepted_activations = [] then Error.error place "Interception context intercepts no activations !!";

            (*** Step b - Forge ctx headers ***)
            let (generated_bridges, (b_onboard, b_onboard_let)) = generate_bridges place interceptor_name intercepted_schemas p_onboard intercepted_bridges in
            let factory, factory_let = generate_interceptor_factory place interceptor_name base_interceptor_constructor.value.args (generated_bridges, (b_onboard, b_onboard_let)) in 

            let headers : stmt list = 
                [ b_onboard_let ]
                (* let b_in_i ;*)
                @ ( 
                    List.map (function (_, _, b_in_let) -> b_in_let) generate_bridges
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
                let spawn_collector parent_opt place = [] in

                collect_expr_stmt None spawn_selector spawn_collector stmt 
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
                    * rewrite each spawn
                    * register headers for this stmt
                    * hydrated exposed_activations_info for each spawn 

                    exposed_id = some a if let a = spawn 
                *)
                let spawn_rewriter (exposed_info: (Atom.atom * Atom.atom) option) parent_opt place = function
                    | Spawn spawn -> begin 
                        match fst spawn.c.value with
                        | VarCExpr schema_a -> begin 
                            let a,a' = exposed_info in
                            let i_a = Atom.fresh ("i_a") in
                            let p_of_a = Atom.fresh "p_of_a" in

                            begin
                                (* Hydrate exposed_activations_info *)
                                match exposed_id with 
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

                            headers := headers @  
                                (* Store a copy of p_of_a is any *)
                                (if spawn.at <> None then
                                    [LetExpr(
                                        mtype_of_ft TPlace,
                                        p_of_a,
                                        Option.get spawn.at
                                    )]
                                else []) 

                                (* Get the interceptor in charge of a *)
                                @ [ LetExpr(
                                    auto_fplace (TActivationRef (mtype_of_cvar interceptor_name)),
                                    i_a,
                                    auto_fplace (CallExpr(
                                        exposed_activations_info, 
                                        List.map (function x -> snd x.value) base_interceptor_constructor.value.params
                                        @ [
                                            auto_fplace (VarExpr factory, auto_fplace EmptyMainType);
                                            schema_of spawn.c; (* TODO convert to some string ?? *)
                                            auto_fplace (VarExpr p_of_a, auto_fplace EmptyMainType)
                                        ]
                                    ), auto_fplace EmptyMainType)
                                ) ]; 
                            
                            (* Need to replace at if exists, in case at expr is not idempotent or costly *)
                            Spawn { spawn with
                                at = Option.map (function _ -> auto_fplace (VarExpr p_of_a, auto_fplace EmptyMainType)) spawn.at
                            }
                        end 
                        | _ -> Error.error place "spawn first arg should have been reduce into a cexpr value (i.e. component name)"
                    end
                    | _ -> []
                in

                (* TODO logic duplicated with exposed_activation*)
                match smt with 
                (* Exposed activations must be identified to hydrate corretly the exposed_activations_info *)
                | LetExpr (mt, a, {value=Spawn spawn, _} as spawn_e) ->
                    (* Replace a by a' to ensure that this pass guarantee `` a binder create a unique named variable`` even if we reintroduce a binder for [a] in ctx footer *)
                    (* NB. variable in ctx body will be replaced afterward using expose_activations_info *)
                    let a' = Atom.fresh (Atom.value a) in

                    (* Rewrite args of exposed spawn *)
                    let args = List.map (rewrite_expr_stmt false spawn_selector (spawn_rewriter None) ) spawn.args in 

                    let spawn = { spawn with args } in

                    (* Exposed_actiation *) 
                    let e = spawn_rewriter (Some (a, a')) parent_opt spawn_e.place spawn_e in

                    !headers @ [ LetExpr(mt, a', e)]
                (* Non-exposed activations *)
                | _ -> rewrite_expr_stmt false spawn_selector spawn_rewriter stmt
            in 

            (* Rewrite intercepted_activations and hydrate exposed_activations_info *)
            let stmts = List.map (function stmt -> ignore (rewritter_stmt_stmt None stmt_spawn_selector stmt_spawn_rewriter stmt)) stmts in

            let stmts = List.flatten stmts in

            (*  apply renaming a -> a', for exposed_activations, in ctx body.
                N.B. binders have been renamed just before.
            *)
            let stmts = 
                List.map 
                    (
                        rewriter_expr_stmt 
                        (function 
                            | VarExpr a -> Hashtbl.find_opt exposed_actvations_info a <> None 
                            | _ -> false) 
                        (function 
                            | VarExpr _ -> 
                                let (_, a', _) = Hashtbl.find exposed_actvations_info x in 
                                VarExpr a'
                            | _ -> false) 
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

                let binded_value = auto_fplace(
                    InterceptedActivationRef (
                        auto_fplace (VarExpr i_a, auto_fplace EmptyMainType),
                        if anonymous_mod then None
                        else Some (auto_fplace (VarExpr a', auto_fplace EmptyMainPlace))
                    ), mt_a
                ) in

                (*  Preserve name and type for the outside world 
                    see whitepaper for detailed discussion of type and subtyping
                *)
                (auto_fplace (LetExpr( 
                    mt_a, 
                    a,  
                    binded_value
                ))) :: footers
            ) exposed_activations_info [] in

            let stmts = stmts @ (List.rev footer_binders) in
            List.map (function stmt -> stmt.value) stmts
        end



    (****************** Main CTX Elim ************************)
    let ctxelim_program program = 
        (* Hydrate interceptor_makes/parents 
            NB. could be merged but are split for readability
        *)
        let _ = collect_stmt_program (function | WithContextStmt _ -> true | _ -> false) ctxelim_prepare_stmt program in

        (* Apply the ctx elimination *)
        let program = rewrite_stmt_program true (function | WithContextStmt _ -> true | _ -> false) ctxelim_rewrite_stmt program in

        (* Insert possibly shared definitions between multiple ctx (and parent schemas) *)
        let program = Hashtbl.fold insert_interceptor_dcl interceptor_makes program in
        let program = Hashtbl.fold insert_onboard_def interceptor_makes program in

        program

    (*********************************************************)

    let displayed_pass_shortdescription = "interception ctx has been eliminated from IR"
    let displayed_ast_name = "interception-ctx-eliminated IR"
    let show_ast = true


    let precondition program = program

    let withcontext_selector = function
        | WithContextStmt _ -> true
        | _ -> false
    let postcondition program = 
        (* Check: no WithContextStmt *)
        ignore (collect_stmt_program withcontext_selector (failure_collector_ce "WithContextStmt remains in IR") program);

        program
    let apply_program = ctxelim_program
end