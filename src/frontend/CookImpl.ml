open Core 
open Core.Error
open Builtin
open Fieldslib
open Easy_logging
open AstUtils

(* The source calculus. *)
module S = Ast_impl
(* The target calculus. *)
module T = Impl 

type env = {
    default_target: string option
}

let fresh_env () : env = {
    default_target = None
}

module Make () = struct
    let logger = Core.Utils.make_log_of "CookImpl" 

    let rec cook_item_impl env place = function 
    | S.MethodImpl mimpl -> env, T.MethodImpl {
        name = mimpl.name;
        body = mimpl.body;
    }
    | S.StateImpl mstate -> env, T.StateImpl {
        name = mstate.name;
        body = mstate.body;
    }
    | S.ComponentHeadersImpl body -> env, T.ComponentHeadersImpl body
    | S.CItemRawImpl body -> env, T.CItemRawImpl body
    and citem_impl env :  S.component_item_impl -> env * T.component_item_impl = map2_place (transparent2_plgannot (cook_item_impl env))

    and cook_component_impl place env (cimpl:S.component_impl) :  env * T.component_impl = 
        let _, items = List.fold_left_map citem_impl env cimpl.body in
        match cimpl.target with 
        | None -> raise (Error.PlacedDeadbranchError (place, "target should have been set or an error should have been reported earlier"))
        | Some target ->
            env, { 
                target = target; 
                name = cimpl.name;
                body = items
            }

    and _cook_term env place = function
    | S.ComponentImpl ({target=None; _} as item) -> begin
        match env.default_target with
        | Some target -> 
            let new_env, item = cook_component_impl place env {item with target = Some target} in
            new_env, [T.ComponentImpl item]
        | None -> Error.perror place "no default target defined and no target assigned to this component" 
    end
    | S.ComponentImpl impl -> 
        let env, item = cook_component_impl place env impl in
        env, [T.ComponentImpl item]
    | S.CurrentDefaultTarget target as term->
        let new_env = { env with default_target = Some target } in
        new_env, [] 
    | S.FunctionImpl timpl -> env, [T.FunctionImpl {
        name = timpl.name;
        body = timpl.body
    }]
    | S.TypeImpl timpl -> env, [T.TypeImpl {
        name = timpl.name;
        body = timpl.body
    }]
    | (S.HeadersImpl body as t) | (S.DependenciesImpl body as t) -> begin
        match env.default_target with
        | Some target -> 
            env, [
                match t with 
                | S.HeadersImpl _ -> T.HeadersImpl {target; body}
                | S.DependenciesImpl _ -> T.DependenciesImpl {target; body}
            ]
        | None -> Error.perror place "no default target defined and no target assigned to headers" 
    end
    and cook_term env :  S.term -> env * T.term list = map2_places (transparent2_plgannots (_cook_term env))

    let cook_program impl_terms =    
        let terms = List.flatten (snd(List.fold_left_map cook_term (fresh_env ()) impl_terms)) in
        terms

        (* TODO post condition no more CurrentDefaultTarget in program*)

    (**********************************************************)
    let name = "CookImpl"
    let displayed_pass_shortdescription = "Impl AST is cooked, IMPL has been generated"
    let displayed_ast_name = "IMPL"
    let global_at_most_once_apply = false


    let show_ast = true 
    let precondition program = program
    let postcondition program = program
    let apply_program = cook_program

end