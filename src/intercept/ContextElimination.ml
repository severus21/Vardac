open Utils
open Core
open AstUtils
open IR
open Easy_logging

let logger = Logging.make_logger ("_1_ compspec.Intercept") Debug [];;

let fplace = (Error.forge_place "Intercept.ContextElimination" 0 0) 
let auto_fplace smth = {place = fplace; value=smth}
include AstUtils2.Mtype.Make(struct let fplace = fplace end)

(* 
    type key = (base_interceptor_type, intercepted component types)
*)
let interceptor_types : ((Atom.atom * Atom.Set.t), component_dcl) Hashtbl.t = Hashtbl.create 16 

(* key -> parent name list *)
let interceptors_parent : ((Atom.atom * Atom.Set.t), Atom.atom option list) Hashtbl.t = Hashtbl.create 16

let show_key (cname, cnames) = 
    Printf.sprintf "Key<%s><%s>" (Atom.show cname) (Atom.Set.show cnames) 

(*
    returns 
        - list of components type spawned inside the context
        - list of activations spawned inside the context and use in the outerscope


    Overapproximation to detect all activation created in the scope 
        how: based on the presence of a activaiton in the left hand side type of a let
        failure: when the let is not [let ... = spawn]
        overapproximation: because activation already created outside the scope but aliased inside are detected
        unsupported cases (but detected -> trigger an error): let t = tuple(1, spawn ...)

    TODO improve static analysis - control flow based ? + list of activations is not statically decidable due to condition and fct call
*)
let analyze_withcontext place cname stmt = 
    let spawn_selector = function
        | LetExpr (_, _, {value=Spawn _, _}) -> true
        | LetExpr (mt, x, _) -> begin
            (* Overapproximation + failure *)
            let tactivation_selector = function
                | CType {value=TActivationRef _} -> true
                | _ -> false
            in
            let tactivation_collector _ _ = function
                | {value=CType {value=TActivationRef mt_comp}} -> [mt_comp]
            in
            let _,collected_mtypes,_ = collect_type_mtype None Atom.Set.empty tactivation_selector tactivation_collector mt in 

            (* TODO use [collected_mtypes] to replace "spawn of TODO" in the followings *)
            if collected_mtypes = [] then false
            else Error.error place "binder [%s] captucollected_mtypes, and exposes, an uncaptured spawn of TODO - this can be an overapproximation, a spawn hidden in an other function or a unhandled cases (like a spawn hidden in a complex expcollected_mtypession)" (Atom.to_string x)
            
        end
        | ForStmt (_, _, _, _) | IfStmt(_,_,_) | MatchStmt (_,_) | BlockStmt _ | WithContextStmt (_,_,_,_)  -> true (* because binders inside inner stmts can not escape *)
        (* FIXME BlockStmt in cleansing ?? change semantics ??? *)
        | _ -> false
    in
    let spawn_collector parent_opt place = function
        | LetExpr (_, x, {value=Spawn spawn, _}) -> begin 
            match fst spawn.c.value with
            | VarCExpr c -> [(c, x)]
            | _ -> Error.error place "spawn first arg should have been reduce into a cexpr value (i.e. component name)"
        end
        | _ -> []
    in 

    let spawned_activations = collect_stmt_stmt None spawn_selector spawn_collector stmt in

    Atom.Set.of_seq (List.to_seq (List.map fst spawned_activations)), spawned_activations

(* 
    with<Interceptor> ctx() { stmt } 

    component InterceptorXX = MakeInterceptor(Interceptor, spawned_componenet_types)
    rewrite spawn by AnonymousInterceptedSpawn or InterceptedSpawn


    generates one unique interceptor component type per context
*)
(* TODO
    check that nested ctxs are correctly handled => maybe rewrite is not applied recursively
    with ...{
        with ... {

        }
    }
*)
let key_of_ctx place cname stmts = 
    let spawned_component_types_sets, _ = List.split (List.map (analyze_withcontext place cname) stmts) in
    let spawned_component_types_set = 
        match spawned_component_types_sets with
        | [] -> Atom.Set.empty
        | acc::stmts -> List.fold_left (fun acc set -> Atom.Set.union acc set) acc stmts
    in
    let spawned_component_types = List.of_seq (Atom.Set.to_seq spawned_component_types_set) in

    (* Deduplicate interceptor component type *)
    let key = (cname, spawned_component_types_set) in
    key

let ctxelim_prepare_stmt parent_opt place = function
    | WithContextStmt (anonymous_mod, cname, e, stmts) -> 
        let key = key_of_ctx place cname stmts in
        let spawned_component_types = List.of_seq (Atom.Set.to_seq (snd key)) in

        logger#debug "ctxelim_prepare: %s" (show_key key);

        (* Deduplicate interceptor component type *)
        match Hashtbl.find_opt interceptor_types key with
        | Some _ -> 
            Hashtbl.add interceptors_parent key (parent_opt::(Hashtbl.find interceptors_parent key));
            [] 
        | None -> begin
            Hashtbl.add interceptors_parent key [parent_opt];

            (* Step 1 - 
                component InterceptorXX = MakeInterceptor(Interceptor, spawned_componenet_types)
            *)
            let interceptor_name = Atom.fresh (Atom.value cname) in
            let interceptor_dcl = auto_fplace (ComponentAssign {
                name = interceptor_name;
                args = []; (* TODO Remove args *)
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
                                    ) spawned_component_types
                                ), auto_fplace EmptyMainType)
                            ), auto_fplace EmptyMainType)
                        ]
                    ), auto_fplace EmptyMainType
                );
            }) in

            Hashtbl.add interceptor_types key interceptor_dcl;
            []
        end

(*
    with ... {
        let a = spawn ...()
        ...
    }
    => 
    let a' = spawn ... ()
    ...
    let a = [InterceptedActivationRef(i, a')] if identity of [a'] should be exposed else [VarExpr i]

*)
let ctxelim_rewrite_stmt place = function 
    | WithContextStmt (anonymous_mod, cname, e, stmts) -> 
        (* NB. key_of_ctx is called twice one in prepare and one in rewrite.
            perf: merge rewrite and preapre
            readability: keep them split in two functions
        *)
        let key = key_of_ctx place cname stmts in
        logger#debug "ctxelim_rewrite: %s" (show_key key);

        let interceptor_assign = Hashtbl.find interceptor_types key in 
        let interceptor_name = match interceptor_assign.value with
        | ComponentAssign cassign -> cassign.name
        in


        let i = Atom.fresh "interceptor" in
        let mt_interceptor = mtype_of_ct (TActivationRef (mtype_of_cvar interceptor_name)) in

        let spawned_activations = List.flatten (List.map snd (List.map (analyze_withcontext place cname) stmts)) in

        (* Step perf. store things in hashtbl *)
        let htbl = Hashtbl.create 16 in
        List.iter (function (intercepted_name, a) ->
            let a' = Atom.fresh (Atom.value a) in
            Hashtbl.add htbl a (a', intercepted_name) 
        ) (failwith "TODO ctx elim");

        (* Step a. let a = spawn => let a' = spawn *)
        let stmts = List.map 
            (rewrite_stmt_stmt false 
                (function | LetExpr (_,x,_) -> Hashtbl.find_opt htbl x <> None | _ -> false) 
                (function place -> function | LetExpr (mt, x, e) ->
                    let a', _ = Hashtbl.find htbl x in
                    [ LetExpr(mt, a', e) ]
                )
            )
            stmts
        in
        let stmts = List.flatten stmts in

        (* Step b. add at the end  [let a = [InterceptedActivationRef(i, a')] if identity of [a'] should be exposed else [VarExpr i]]*)
        let post_binders = Hashtbl.fold (fun  a (a', intercepted_name) acc ->
            let mt_intercepted = mtype_of_ct (TActivationRef (mtype_of_cvar intercepted_name)) in
            let external_binder = auto_fplace(
                if anonymous_mod then (
                    VarExpr i, mtype_of_ct (TActivationRef (mtype_of_cvar interceptor_name))
                )else(
                    InterceptedActivationRef (
                        auto_fplace (VarExpr i, mt_interceptor), 
                        auto_fplace (VarExpr intercepted_name, mt_intercepted)
                    ), if anonymous_mod then mt_interceptor else mt_intercepted
                )
            ) in
            (auto_fplace (LetExpr( 
                (* Preserved type for the outside world *)
                mtype_of_ct (TActivationRef (mtype_of_cvar intercepted_name)), 
                a, (* Preserve name for the outside world *) 
                external_binder
            ))) :: acc
        ) htbl [] in

        let stmts = stmts @ (List.rev post_binders) in
        List.map (function stmt -> stmt.value) stmts


let insert_interceptor_dcl key interceptor_dcl (program:program) =
    let parents = Hashtbl.find interceptors_parent key in
    let flag_toplevel_parent = List.mem None parents in

    let common_ancestor_name = if flag_toplevel_parent then (
        (* Just need to add it in the toplevel scope *)
        None
    )
    else (
        let parents = List.map Option.get (List.filter (function x -> x <> None) parents) in
        (* Dedup *)
        let parents_set = Atom.Set.of_seq (List.to_seq parents) in

        (* Search for lowest common ancestor *)
        IR_utils.find_lca_program parents_set program
    ) in
 
    let insert_in_ancestor (program: program) : Atom.atom option -> program = function
        | None -> IR_utils.insert_in_terms [auto_fplace (Component interceptor_dcl)] program 
        | Some ca_name ->
            let ancestor_selector = function 
                | Component {value=ComponentStructure cdcl} -> cdcl.name =ca_name 
                | _ -> false
            in
            let ancestor_rewriter place = function
                | Component {place; value=ComponentStructure cdcl} ->
                    let terms_body = List.map (function | {value=Term t} -> t) (List.filter (function |{value=Term _} -> true | _ -> false) cdcl.body) in
                    let remaining_body = List.filter (function |{value=Term _} -> false | _ -> true) cdcl.body in

                    let terms_body = IR_utils.insert_in_terms [auto_fplace (Component interceptor_dcl)] terms_body in 
                    let terms_body = List.map (function t -> {place=t.place; value=Term t}) terms_body in


                    [ 
                        Component {place; value = ComponentStructure {cdcl with
                            body = terms_body @ remaining_body 
                        }} 
                    ]
            in

            rewrite_term_program ancestor_selector ancestor_rewriter program
    in 
    insert_in_ancestor program common_ancestor_name 

let scope_rewriter terms = 
    (* At least one of the term in terms is a WithContextStmt *)

    (* Step 2 - insert the interceptor component type inside the scope i.e. terms *)
    let terms = IR_utils.insert_in_terms 
        (List.map (function c -> auto_fplace (Component c)) (List.of_seq (Hashtbl.to_seq_values interceptor_types)))
        terms
    in


    terms

let ctxelim_program program = 
    (* Step 0 - hydrate the hashtbl 
        NB. step0 and 1 could be merged but are split for readability
    *)
    let _ = collect_stmt_program (function | WithContextStmt _ -> true | _ -> false) ctxelim_prepare_stmt program in

    (* Step 1 - apply the rewrite *)
    let program = rewrite_stmt_program true (function | WithContextStmt _ -> true | _ -> false) ctxelim_rewrite_stmt program in

    let program = Hashtbl.fold insert_interceptor_dcl interceptor_types program in
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
