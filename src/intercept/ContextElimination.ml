open Core
open AstUtils
open IR
open Easy_logging

let logger = Logging.make_logger ("_1_ compspec.Intercept") Debug [];;

let fplace = (Error.forge_place "Intercept.ContextElimination" 0 0) 
let auto_fplace smth = {place = fplace; value=smth}
include AstUtils2.Mtype.Make(struct let fplace = fplace end)


(*
    returns 
        - list of components type spawned inside the context
        - list of activations spawned inside the context and use in the outerscope


        Warning: heuristic only capture the 
            let x = spawn ...; 

        TODO improve static analysis - control flow based ? + list of activations is not statically decidable due to condition and fct call
*)
let analyze_withcontext place cname e stmt = 
    let spawn_selector = function
        | LetExpr (_, _, {value=Spawn _, _}) -> true
        | _ -> false
    in
    let spawn_collector parent_opt place = function
        | LetExpr (_, x, {value=Spawn spawn, _}) -> begin 
            match fst spawn.c.value with
            | VarCExpr c -> [(c, x)]
            | _ -> Error.error place "spawn first arg should have been reduce into a cexpr value (i.e. component name)"
        end
    in 
    let spawned_component_types, spawned_activations = List.split (collect_stmt_stmt None spawn_selector spawn_collector stmt) in

    Atom.Set.of_seq (List.to_seq spawned_component_types), Atom.Set.of_seq (List.to_seq spawned_activations)

(* 
    with<Interceptor> ctx() { stmt } 

    component InterceptorXX = MakeInterceptor(Interceptor, spawned_componenet_types)
    rewrite spawn by AnonymousInterceptedSpawn or InterceptedSpawn
*)
let ctxelim_stmt_ place = function
    | WithContextStmt (anonymous_mod, cname, e, stmt) -> 


        let spawned_component_types_set, spawned_activations_set = analyze_withcontext place cname e stmt in
        let spawned_component_types = List.of_seq (Atom.Set.to_seq spawned_component_types_set) in
        


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
        (*
            Step 2 - 
            with ... {
                a = spawn ...()

            }
            continuation_stmt
            => [VarExpr a] in [continuation_stmt] by
                InterceptedActivationInfo(i, a) if identity of [a] should be exposed NB. literal as type TActivationInfo(I)
                VarExpr i otherwise
                

        *)
        let i = Atom.fresh "interceptor" in
        let mt_interceptor = mtype_of_ct (TActivationInfo (mtype_of_cvar interceptor_name)) in

        let continuation_rewriter =  function
            | VarExpr intercepted_name, _ when Atom.Set.mem intercepted_name spawned_activations_set -> begin
                let mt_intercepted = mtype_of_ct (TActivationInfo (mtype_of_cvar intercepted_name)) in
                if anonymous_mod then (
                    VarExpr i, mtype_of_ct (TActivationInfo (mtype_of_cvar interceptor_name))
                )else(
                    InterceptedActivationInfo (
                        auto_fplace (VarExpr i, mt_interceptor), 
                        auto_fplace (VarExpr intercepted_name, mt_intercepted)
                    ), if anonymous_mod then mt_interceptor else mt_intercepted
                )
            end
        in

        interceptor_dcl, continuation_rewriter 
    | stmt -> failwith "TODO propagation"


let ctxelim_program program = failwith "TODO ctxelim_program" 
    