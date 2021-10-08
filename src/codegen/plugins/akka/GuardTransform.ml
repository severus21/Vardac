open Core
open Utils
open AstUtils
open Easy_logging
open Fieldslib
open Misc

(* FIXME maybe some global transformation*)
let plg_name = "Akka"
let logger = Logging.make_logger ("_1_ compspec.plg."^plg_name) Debug [];;

(* The source calculus. *)
module S = IRI 
(* The target calculus. *)
module T = IRI 

include IRI

module Env = Atom.AtomMap 

(*
    Take a type guard for each timer,
        create a fresh timer for timeout and a fresh timer for lower bound and replace it according the usage.

        "!int{timer x|}!int{|5<x && x<10} => !int{timer x_lb, timer x_hb |}!int{|5<x_lb && x_hb<10}"

    Hyp: no function call inside the guard (or should have been inlined)
*)

type timer_separation = {
    name: variable;
    lb_name: variable;
    hb_name: variable;
}
(* timer t -> (t_lb, t_hb)*)
(* global since name are unique *)
let separation_hbl : (variable, variable * variable) Hashtbl.t = Hashtbl.create 16
let rec timers_of = function
    | {value=UseGlobal _}::headers | {value=UseMetadata _} ::headers-> timers_of headers
    | {value=SetTimer x}::headers -> x::(timers_of headers)

let rec rename_guard_ place : _expr -> _expr = function
(* Rewrite of timer *)
| BinopExpr ({value=VarExpr x; place=p_x}, LessThan, ({value=LitExpr {value=IntLit i}} as e)) -> 
    (* Cook guarantees that everithing is correctly binded *)
    let _, x_hb = Hashtbl.find separation_hbl x in
    BinopExpr ({value=VarExpr x_hb; place=p_x}, LessThan, e)
| BinopExpr ({value=LitExpr {value=IntLit i}} as e, GreaterThan, {value=VarExpr x; place=p_x}) ->
    (* Cook guarantees that everithing is correctly binded *)
    let _, x_hb = Hashtbl.find separation_hbl x in
    BinopExpr (e, GreaterThan, {value=VarExpr x_hb; place=p_x})
| BinopExpr ({value=VarExpr x; place=p_x}, GreaterThan, ({value=LitExpr {value=IntLit i}} as e)) ->
    (* Cook guarantees that everithing is correctly binded *)
    let x_lb, _ = Hashtbl.find separation_hbl x in
    BinopExpr ({value=VarExpr x_lb; place=p_x}, GreaterThan, e)
| BinopExpr ({value=LitExpr {value=IntLit i}} as e, LessThan, {value=VarExpr x; place=p_x}) ->
    (* Cook guarantees that everithing is correctly binded *)
    let x_lb, _ = Hashtbl.find separation_hbl x in
    BinopExpr (e, LessThan, {value=VarExpr x_lb; place=p_x})

(* Just explore *)
| VarExpr _ as e -> e
| AccessExpr _ as e -> e (* can not have the form of a timer expression *)
| BinopExpr (e1, op, e2) -> BinopExpr(rename_guard e1, op, rename_guard e2)
| LambdaExpr _ | CallExpr _ | NewExpr _ | Spawn _ | BoxCExpr _ -> raise (Error.DeadbranchError "LambdaExpr/CallExpr/NewExpr/Spawn/BoxCExpr can not appear inside a guard") (* TODO check it in COook*)
| LitExpr _ as e -> e
| UnopExpr (op, e) ->  UnopExpr (op, rename_guard e)
| This -> This
| OptionExpr e_opt -> OptionExpr (Option.map rename_guard e_opt)
| ResultExpr (e1_opt, e2_opt) -> ResultExpr (
    Option.map rename_guard e1_opt,
    Option.map rename_guard e2_opt
)
| BlockExpr (b, es) -> BlockExpr (
    b,
    List.map rename_guard es
)
| Block2Expr (b, ees) -> Block2Expr (
    b,
    List.map (function (e1, e2) -> rename_guard e1, rename_guard e2) ees
)
and rename_guard guard = {
    place = guard.place;
    value = rename_guard_ guard.place guard.value
}

let rec separate_lb_hb_timers_ place = function
| STSend ({place=p_mt; value=ConstrainedType (mt, (guard_headers, guard_opt))}, st) ->
    let timers = timers_of guard_headers in 
    List.map (function name ->
    begin
    try 
        ignore (Hashtbl.find separation_hbl name)  
    with Not_found -> 
        Hashtbl.add separation_hbl name (
            Atom.fresh (Atom.hint name^"_lb"),
            Atom.fresh (Atom.hint name^"_hb")
        )
    end) timers;

    
    STSend ({place=p_mt; value=ConstrainedType (mt, (guard_headers, Option.map rename_guard guard_opt))}, separate_lb_hb_timers st)
(* TODO other *)
and separate_lb_hb_timers st = {
    place = st.place; 
    value = separate_lb_hb_timers_ st.place st.value;
}   


type timer_entry = {
    base_value: int;
    next_trigger: int option;
}

let flatten_option = function
| None -> None
| Some None -> None
| Some opt -> opt

(* 
    Assumes: t_lb (rep t_hb) is used at most one per guard. 
    Consequence: return the first value
    TODO check it
*)
let rec next_trigger_of_ name place = function
| VarExpr _ | This | LitExpr _ -> None
| AccessExpr _ -> None (* can not have the form of a timer expression *)
| BinopExpr ({value=VarExpr x;}, LessThan, {value=LitExpr {value=IntLit i}}) | BinopExpr ({value=LitExpr {value=IntLit i}} , GreaterThan, {value=VarExpr x;}) when x = name ->
    Some i
| BinopExpr ({value=VarExpr x;}, GreaterThan, {value=LitExpr {value=IntLit i}}) | BinopExpr ({value=LitExpr {value=IntLit i}}, LessThan, {value=VarExpr x;}) when x = name ->
    Some i
| LambdaExpr _ | CallExpr _ | NewExpr _ | Spawn _ | BoxCExpr _ -> raise (Error.DeadbranchError "LambdaExpr/CallExpr/NewExpr/Spawn/BoxCExpr can not appear inside a guard") (* TODO check it in COook*)
| UnopExpr (_,e) -> next_trigger_of name e
| OptionExpr e_opt -> flatten_option (Option.map (next_trigger_of name) e_opt)
| BinopExpr (e1, opt, e2) -> begin
    match (next_trigger_of name) e1 with
    | Some i -> Some i
    | None -> (next_trigger_of name) e2
end
| ResultExpr (e1_opt, e2_opt) -> begin
    match flatten_option (Option.map (next_trigger_of name) e1_opt) with 
    | Some i -> Some i
    | None -> flatten_option (Option.map (next_trigger_of name) e2_opt) 
end
| BlockExpr (b, es) -> failwith "not yet supported in GuardTransform"
| Block2Expr (b, ees) -> failwith "not yet supported in GuardTransform"
and next_trigger_of name e = next_trigger_of_ name e.place e.value

(*
    returns (timers to reset, rewritten guard
    where triggers refers to the current entry.base_value)
*)
let rec rewrite_trigger_ env place = function
(* Rewrite of timer *)
| BinopExpr ({value=VarExpr x; place=p_x}, LessThan, {value=LitExpr {value=IntLit i; place=p_i}; place=p_lit}) as e -> begin 
    (* Cook guarantees that everithing is correctly binded *)
    match Env.find_opt x env with
    | None -> [], e (* Not a timer *)
    | Some entry -> (* A timer *)
    begin 
        let i = i - entry.base_value in
        [x], BinopExpr ({value=VarExpr x; place=p_x}, LessThan, {value=LitExpr {value=IntLit i; place=p_i}; place = p_lit})
    end
end
| BinopExpr ({value=LitExpr {value=IntLit i; place=p_i}; place=p_lit}, GreaterThan, {value=VarExpr x; place=p_x}) as e -> begin
    (* Cook guarantees that everithing is correctly binded *)
    match Env.find_opt x env with
    | None -> [], e (* Not a timer *)
    | Some entry -> (* A timer *)
    begin 
        let i = i - entry.base_value in
        [x], BinopExpr ({value=LitExpr {value=IntLit i; place=p_i}; place =p_lit}, GreaterThan, {value=VarExpr x; place=p_x}) 
    end
end
| BinopExpr ({value=VarExpr x; place=p_x}, GreaterThan, {value=LitExpr {value=IntLit i; place=p_i}; place=p_lit}) as e -> begin
    (* Cook guarantees that everithing is correctly binded *)
    match Env.find_opt x env with
    | None -> [], e (* Not a timer *)
    | Some entry -> (* A timer *)
    begin 
        let i = i - entry.base_value in
        [x], BinopExpr ({value=VarExpr x; place=p_x}, GreaterThan, {value=LitExpr {value=IntLit i; place=p_i}; place=p_lit})
    end
end
| BinopExpr ({value=LitExpr {value=IntLit i; place=p_i}; place=p_lit}, LessThan, {value=VarExpr x; place=p_x}) as e -> begin
    (* Cook guarantees that everithing is correctly binded *)
    match Env.find_opt x env with
    | None -> [], e (* Not a timer *)
    | Some entry -> (* A timer *)
    begin 
        let i = i - entry.base_value in
        [x], BinopExpr ({value=LitExpr {value=IntLit i; place=p_i}; place=p_lit}, LessThan, {value=VarExpr x; place=p_x})
    end
end
(* Just explore *)
| VarExpr _ as e -> [], e
| AccessExpr _ as e -> [], e (* can not have the form of a timer expression *)
| BinopExpr (e1, op, e2) -> 
    let timers1, e1 = rewrite_trigger env e1 in
    let timers2, e2 = rewrite_trigger env e2 in
    timers1@timers2, BinopExpr(e1, op, e2)
| LambdaExpr _ | CallExpr _ | NewExpr _ | Spawn _ | BoxCExpr _ -> raise (Error.DeadbranchError "LambdaExpr/CallExpr/NewExpr/Spawn/BoxCExpr can not appear inside a guard") (* TODO check it in COook*)
| (LitExpr _ as e) | (This as e) -> [], e
| UnopExpr (op, e) ->  
    let timers, e = rewrite_trigger env e in
    timers, UnopExpr (op, e)
| OptionExpr e_opt -> failwith "not yet supported in GuardTransform"
| ResultExpr (e1_opt, e2_opt) -> failwith "not yet supported in GuardTransform"
| BlockExpr (b, es) ->failwith "not yet supported in GuardTransform" 
| Block2Expr (b, ees) -> failwith "not yet supported in GuardTransform" 
and rewrite_trigger env (e:expr) : variable list * expr = 
    let timers, _e = rewrite_trigger_ env e.place e.value in
    timers, { place = e.place; value = _e}

(* After this transformation a time is used for exactly one condition before behing reset 
    "!int{timer x|}!int{reset x | x<5}!{|x<5}"
    + replace all SetTimer with SetFireTimer x, int (int tigger dealy)
*)
let rec reset_timers_ env place = 
let fplace = (Error.forge_place "Akka.GuardTransform.reset_timers" 0 0) in
let auto_place smth = {place = place; value=smth} in
let auto_fplace smth = {place = fplace; value=smth} in

function 
| STSend ({place = place_st; value=ConstrainedType (mt, (guard_headers, guard_opt))}, st) ->
    (* MaJ next triggers of registered timers *)
    let registered_timers = List.of_seq (Env.to_seq env) in
    let new_env0 = List.fold_left (fun env (name, entry) -> 
        let next_trigger_opt = flatten_option (Option.map (next_trigger_of name) guard_opt) in
        let new_entry = { 
            base_value = entry.base_value;
            next_trigger = next_trigger_opt; 
        } in
        Env.add name new_entry env 
    ) env registered_timers in

    (* Reset timers according to the programmer *)
    let timers = timers_of guard_headers in 
    let new_env1 = List.fold_left (fun env name ->
        let next_trigger_opt = flatten_option (Option.map (next_trigger_of name) guard_opt) in
        let new_entry = {
            base_value=0;
            next_trigger=next_trigger_opt; 
        } in
        Env.add name new_entry env 
    ) new_env0 timers in


    match guard_opt with
    | None -> 
        STSend ({place = place_st; value=ConstrainedType (mt, (guard_headers, guard_opt))}, reset_timers new_env1 st)
    | Some guard -> begin
        (* Timer with condition in the guard 
            Should be reset also but base_value should be incremented in order to rewrite update [st] with new trigger value.
        *)
        let timers_to_reset, guard = rewrite_trigger new_env1 guard in 


        (* Dedup timers to reset two sources:
            - user
            - our transformation 
            should appear only once in headers *)
        let set_transform = Atom.Set.of_seq(List.to_seq (timers_to_reset)) in
        let set_user = Atom.Set.of_seq(List.to_seq (timers)) in
        let timers_to_add = List.of_seq(Atom.Set.to_seq(Atom.Set.diff set_transform set_user)) in
        let guard_headers = (List.map (function name -> auto_fplace (SetTimer name)) timers_to_add) @ guard_headers in

        (* Filter out timers that are not use in the remaining *)
        let guard_headers = List.filter (function
            | {value=SetTimer name} -> 
                let entry = Env.find name new_env1 in
                entry.next_trigger <> None 
            | header -> true 
        ) guard_headers in
        (* Replace SetTimer with SetFireTimer *)
        let guard_headers = List.map (function
            | {value=SetTimer name} -> 
                let entry = Env.find name new_env1 in
                auto_fplace (SetFireTimer (name, (Option.get entry.next_trigger) - entry.base_value))
            | header -> header
        ) guard_headers in

        STSend ({place = place_st; value=ConstrainedType (mt, (guard_headers, guard_opt))}, reset_timers new_env1 st)
    end
and reset_timers env st = {
    place = st.place;
    value = reset_timers_ env st.place st.value
}

(* TODO
let rec gtransform_program = 
    *)