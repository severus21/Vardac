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

module Env = Atom.VMap 

(*
    Take a type guard for each timer,
        create a fresh timer for timeout and a fresh timer for lower bound and replace it according the usage.

        "!int{timer x|}!int{|5<x && x<10} => !int{timer x_lb, timer x_hb |}!int{|5<x_lb && x_hb<10}"

    Hyp: no function call inside the guard (or should have been inlined)
*)

type timer_separation = {
    name: expr_variable;
    lb_name: expr_variable;
    hb_name: expr_variable;
}
(* timer t -> (t_lb, t_hb)*)
(* global since name are unique *)
let separation_hbl : (expr_variable, expr_variable * expr_variable) Hashtbl.t = Hashtbl.create 16

let rename_header_ place = 
let auto_place value = {place; value} in
function 
| UseMetadata _ as h -> [ auto_place h ]
| SetTimer x as h-> begin 
    try
        let x_lb, x_hb = Hashtbl.find separation_hbl x in
        [ auto_place (SetTimer x_lb); auto_place(SetTimer x_hb)]
    with Not_found -> [ auto_place h ]
end
let rename_header h = rename_header_ h.place h.value
let rename_headers hs = List.flatten (List.map rename_header hs)

let rec rename_guard_ place : _expr -> _expr = function
(* Rewrite of timer *)
| BinopExpr ({value=(VarExpr x, mt_x); place=p_x}, LessThan, ({value=LitExpr {value=IntLit i}, _} as e)) -> (* x < i*)
    (* Cook guarantees that everithing is correctly binded *)
    let _, x_hb = Hashtbl.find separation_hbl x in
    BinopExpr ({value=(VarExpr x_hb, mt_x); place=p_x}, LessThan, e)
| BinopExpr ({value=LitExpr {value=IntLit i}, _} as e, GreaterThan, {value=VarExpr x, mt_x; place=p_x}) -> (* i > x *)
    (* Cook guarantees that everithing is correctly binded *)
    let _, x_hb = Hashtbl.find separation_hbl x in
    BinopExpr (e, GreaterThan, {value=VarExpr x_hb, mt_x; place=p_x})
| BinopExpr ({value=VarExpr x, mt_x; place=p_x}, GreaterThan, ({value=LitExpr {value=IntLit i}, _} as e)) -> (* x > i *)
    (* Cook guarantees that everithing is correctly binded *)
    let x_lb, _ = Hashtbl.find separation_hbl x in
    BinopExpr ({value=VarExpr x_lb, mt_x; place=p_x}, GreaterThan, e)
| BinopExpr ({value=LitExpr {value=IntLit i}, _} as e, LessThan, {value=VarExpr x, mt_x; place=p_x}) -> (* i < x *)
    (* Cook guarantees that everithing is correctly binded *)
    let x_lb, _ = Hashtbl.find separation_hbl x in
    BinopExpr (e, LessThan, {value=VarExpr x_lb, mt_x; place=p_x})

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
    value = 
        let e, mt = guard.value in 
        (rename_guard_ guard.place e, mt)
}
let rec aux_separate_lb_hb_timers_ac ((guard_headers, guard_opt):applied_constraint)= 
    logger#info "separate_lb_hb_timers_";
    let timers = timers_of_headers guard_headers in 
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

    let guard_headers = rename_headers guard_headers in
    let guard_opt =  Option.map rename_guard guard_opt in
    guard_headers, guard_opt

and aux_separate_lb_hb_timers_branch (label, st, ac_opt) =
    match ac_opt with
    | Some ac ->
        let guard_headers, guard_opt = aux_separate_lb_hb_timers_ac ac in
        (label, separate_lb_hb_timers st, Some (guard_headers, guard_opt))
    | _ -> (label, separate_lb_hb_timers st, ac_opt) 

and separate_lb_hb_timers_ place = function
| STSend ({place=p_mt; value=ConstrainedType(mt, ac)}, st) -> 
    let guard_headers, guard_opt = aux_separate_lb_hb_timers_ac ac in
    STSend ({place=p_mt; value=ConstrainedType (mt, (guard_headers, guard_opt))}, separate_lb_hb_timers st)
| STRecv ({place=p_mt; value=ConstrainedType(mt, ac)}, st) ->
    let guard_headers, guard_opt = aux_separate_lb_hb_timers_ac ac in
    STRecv ({place=p_mt; value=ConstrainedType (mt, (guard_headers, guard_opt))}, separate_lb_hb_timers st)
| STBranch branches -> STBranch (List.map aux_separate_lb_hb_timers_branch branches)
| STSelect branches -> STSelect (List.map aux_separate_lb_hb_timers_branch branches) 
(* TODO other *)

(* Just propagate *)
| STEnd -> STEnd 
| STVar x -> STVar x
| STPolyVar x -> STPolyVar x
| STRec (x, st) -> STRec (x, separate_lb_hb_timers st)
| STRecv (mt, st) -> STRecv (mt, separate_lb_hb_timers st)
| STSend (mt, st) -> STSend (mt, separate_lb_hb_timers st)
and separate_lb_hb_timers st = {
    place = st.place; 
    value = separate_lb_hb_timers_ st.place st.value;
}   

(**************************************************************)

type timer_entry = {
    base_value: int;
    next_trigger: int option;
}
[@@deriving show { with_path = false }]

let print_env env = 
    Format.fprintf Format.std_formatter "Print env\n";
    Error.pp_list 
        "@;" 
        (fun out (x,entry) -> 
            Format.fprintf out "%s -> %s" (Atom.to_string x) (show_timer_entry entry)
        )
        Format.std_formatter
        (List.of_seq(Env.to_seq env)); 
    Format.fprintf Format.std_formatter "\n\n"


let flatten_option = function
| None -> None
| Some None -> None
| Some opt -> opt

(* 
    Assumes: t_lb (rep t_hb) is used at most one per guard. 
    Consequence: return the first value
    TODO check it
*)
let rec next_trigger_of_expr_ name place = 
function
| VarExpr _ | This | LitExpr _ -> None
| AccessExpr _ -> None (* can not have the form of a timer expression *)
| BinopExpr ({value=VarExpr x, _;}, LessThan, {value=LitExpr {value=IntLit i}, _}) | BinopExpr ({value=LitExpr {value=IntLit i}, _} , GreaterThan, {value=VarExpr x, _;}) when x = name ->
    Some i
| BinopExpr ({value=VarExpr x, _;}, GreaterThan, {value=LitExpr {value=IntLit i}, _}) | BinopExpr ({value=LitExpr {value=IntLit i}, _}, LessThan, {value=VarExpr x, _;}) when x = name ->
    Some i
| LambdaExpr _ | CallExpr _ | NewExpr _ | Spawn _ | BoxCExpr _ -> raise (Error.DeadbranchError "LambdaExpr/CallExpr/NewExpr/Spawn/BoxCExpr can not appear inside a guard") (* TODO check it in COook*)
| UnopExpr (_,e) -> next_trigger_of_expr name e
| OptionExpr e_opt -> flatten_option (Option.map (next_trigger_of_expr name) e_opt)
| BinopExpr (e1, opt, e2) -> begin
    match (next_trigger_of_expr name) e1 with
    | Some i -> Some i
    | None -> (next_trigger_of_expr name) e2
end
| ResultExpr (e1_opt, e2_opt) -> begin
    match flatten_option (Option.map (next_trigger_of_expr name) e1_opt) with 
    | Some i -> Some i
    | None -> flatten_option (Option.map (next_trigger_of_expr name) e2_opt) 
end
| BlockExpr (b, es) -> failwith "not yet supported in GuardTransform"
| Block2Expr (b, ees) -> failwith "not yet supported in GuardTransform"
and next_trigger_of_expr name e =           
    next_trigger_of_expr_ name e.place (fst e.value)


and next_trigger_of_st_ name place = function
| STSend ({value=ConstrainedType (_, (_, guard_opt))}, st) | STRecv ({value=ConstrainedType (_, (_, guard_opt))}, st) -> begin
    match flatten_option (Option.map (next_trigger_of_expr name) guard_opt) with
    | Some x -> Some x
    | None -> next_trigger_of_st name st
end
(**)
| STEnd -> None
| STRecv (_, st) | STSend (_, st) -> next_trigger_of_st name st
and next_trigger_of_st name st = next_trigger_of_st_ name st.place st.value

(*
    returns (timers to reset, rewritten guard
    where triggers refers to the current entry.base_value)
*)
let rec rewrite_trigger_ env place = function
(* Rewrite of timer *)
| BinopExpr ({value=VarExpr x, mt_x; place=p_x}, LessThan, {value=LitExpr {value=IntLit i; place=p_i}, mt_i; place=p_lit}) as e -> begin 
    (* Cook guarantees that everithing is correctly binded *)
    match Env.find_opt x env with
    | None -> [], e (* Not a timer *)
    | Some entry -> (* A timer *)
    begin 
        let i = i - entry.base_value in
        [x], BinopExpr ({value=VarExpr x, mt_x; place=p_x}, LessThan, {value=LitExpr {value=IntLit i; place=p_i}, mt_i; place = p_lit})
    end
end
| BinopExpr ({value=LitExpr {value=IntLit i; place=p_i}, mt_i; place=p_lit}, GreaterThan, {value=VarExpr x, mt_x; place=p_x}) as e -> begin
    (* Cook guarantees that everithing is correctly binded *)
    match Env.find_opt x env with
    | None -> [], e (* Not a timer *)
    | Some entry -> (* A timer *)
    begin 
        let i = i - entry.base_value in
        [x], BinopExpr ({value=LitExpr {value=IntLit i; place=p_i}, mt_i; place =p_lit}, GreaterThan, {value=VarExpr x, mt_x; place=p_x}) 
    end
end
| BinopExpr ({value=VarExpr x, mt_x; place=p_x}, GreaterThan, {value=LitExpr {value=IntLit i; place=p_i}, mt_i; place=p_lit}) as e -> begin
    (* Cook guarantees that everithing is correctly binded *)
    match Env.find_opt x env with
    | None -> [], e (* Not a timer *)
    | Some entry -> (* A timer *)
    begin 
        let i = i - entry.base_value in
        [x], BinopExpr ({value=VarExpr x, mt_x; place=p_x}, GreaterThan, {value=LitExpr {value=IntLit i; place=p_i}, mt_i; place=p_lit})
    end
end
| BinopExpr ({value=LitExpr {value=IntLit i; place=p_i}, mt_i; place=p_lit}, LessThan, {value=VarExpr x, mt_x; place=p_x}) as e -> begin
    (* Cook guarantees that everithing is correctly binded *)
    match Env.find_opt x env with
    | None -> [], e (* Not a timer *)
    | Some entry -> (* A timer *)
    begin 
        let i = i - entry.base_value in
        [x], BinopExpr ({value=LitExpr {value=IntLit i; place=p_i}, mt_i; place=p_lit}, LessThan, {value=VarExpr x, mt_x; place=p_x})
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
and rewrite_trigger env (e:expr) : expr_variable list * expr = 
    let timers, _e = rewrite_trigger_ env e.place (fst e.value) in
    timers, { place = e.place; value = _e, snd e.value}

let filter_headers env place (guard_headers : constraint_header list) : constraint_header list =
    let fplace = (Error.forge_place "Akka.GuardTransform.filter_headers" 0 0) in
    let auto_place smth = {place = place; value=smth} in
    let auto_fplace smth = {place = fplace; value=smth} in
    logger#error "filter_headers";
    (* Filter out timers that are not use in the remaining *)
    let guard_headers = List.filter (
    function
        | {value=SetTimer name} -> 
            let entry = Env.find name env in
            entry.next_trigger <> None 
        | _-> true 
    ) guard_headers in

    (* Replace SetTimer with SetFireTimer *)
    let guard_headers = List.map (function
        | {value=SetTimer name} -> 
            let entry = Env.find name env in
            auto_fplace (SetFireTimer (name, (Option.get entry.next_trigger) - entry.base_value))
        | header -> header
    ) guard_headers in

    (* Filter 0 timeout *)
    let guard_headers = List.filter (
    function
        | {value=SetFireTimer (_, 0)} -> false
        | _-> true 
    ) guard_headers in

    guard_headers

(* After this transformation a time is used for exactly one condition before behing reset 
    "!int{timer x|}!int{reset x | x<5}!{|x<5}"
    + replace all SetTimer with SetFireTimer x, int (int trigger delay)
*)
let rec aux_reset_times_ac env place st0 (guard_headers, guard_opt) = 
    let fplace = (Error.forge_place "Akka.GuardTransform.reset_timers" 0 0) in
    let auto_place smth = {place = place; value=smth} in
    let auto_fplace smth = {place = fplace; value=smth} in

    logger#warning "reset <>";
    (* Reset timers according to the programmer *)
    let timers = timers_of_headers guard_headers in 
    let new_env0 = List.fold_left (fun env name ->
        let next_trigger_opt = next_trigger_of_st_ name place st0 in
        let new_entry = {
            base_value=0;
            next_trigger=next_trigger_opt; 
        } in
        Env.add name new_entry env 
    ) env timers in
    
    (* MaJ next triggers of registered timers *)
    (*let registered_timers = List.of_seq (Env.to_seq env) in*)
    let registered_timers = List.of_seq ( (* but not user timers *) 
        Atom.Set.to_seq (Atom.Set.diff
            (Atom.Set.of_seq (Seq.map fst (Env.to_seq env))) 
            (Atom.Set.of_seq (List.to_seq timers)) 
        )
    ) in
    let new_env1, updated_registered_timers = List.fold_left (fun (env, updated_registered_timers) name -> 
        let entry = Env.find name env in
        let next_trigger_opt = next_trigger_of_st_ name place st0 in
        begin
        match next_trigger_opt with
        | None ->         logger#warning "new_env1 next of %s None" (Atom.to_string name);
        | Some _ -> 
        logger#warning "new_env1 next of %s Some" (Atom.to_string name);

        end;

        let new_entry = { 
            base_value = entry.base_value;
            next_trigger = next_trigger_opt; 
        } in
        if entry.next_trigger = new_entry.next_trigger || new_entry.next_trigger = None then
            env, updated_registered_timers
        else
            Env.add name new_entry env, name::updated_registered_timers
    ) (new_env0,[]) registered_timers in

    logger#error "updated_registered %s" (List.fold_left (fun acc x -> acc ^ " " ^(Atom.to_string x)) "" updated_registered_timers); 
    print_env new_env1;

    match guard_opt with
    | None -> 
        let guard_headers = filter_headers new_env1 place guard_headers in
        new_env1, (guard_headers, guard_opt)
    | Some guard -> begin
        (* Update base_value according to base *)
        let new_env2 = List.fold_left ( (* can be optimized*)
            fun (env:timer_entry Env.t) (name, entry) ->
            let updated_based_value_opt = next_trigger_of_expr name guard in
            match updated_based_value_opt with
            | None -> env 
            | Some update_bv ->
                Env.add name {entry with base_value = update_bv} env 
        ) new_env1 (List.of_seq(Env.to_seq new_env1)) in   

        print_env new_env2;


        (* Timer with condition in the guard 
            Should be reset also but base_value should be incremented in order to rewrite update [st] with new trigger value.
        *)
        let timers_to_reset, guard = rewrite_trigger new_env2 guard in 
        logger#error "timer_to_resets %s" (List.fold_left (fun acc x -> acc ^ " " ^(Atom.to_string x)) "" timers_to_reset); 
        
        (* Dedup timers to reset two sources:
            - user
            - our transformation 
            should appear only once in headers *)
        let set_transform = Atom.Set.of_seq(List.to_seq (timers_to_reset)) in
        let set_user = Atom.Set.of_seq(List.to_seq (timers)) in
        let timers_to_add = List.of_seq(Atom.Set.to_seq(Atom.Set.diff set_transform set_user)) in
        logger#error "timer_to_add %s" (List.fold_left (fun acc x -> acc ^ " " ^(Atom.to_string x)) "" timers_to_reset); 
        (* remove non used timers *)
        let timers_to_add = List.filter (function name -> 
            let entry = Env.find name new_env2 in
            entry.next_trigger <> None
        ) timers_to_add in 
        logger#error "timer_to_add %s" (List.fold_left (fun acc x -> acc ^ " " ^(Atom.to_string x)) "" timers_to_reset); 
        let guard_headers = (List.map (function name -> auto_fplace (SetTimer name)) timers_to_add) @ guard_headers in
        
        let guard_headers = filter_headers new_env2 place guard_headers in
        new_env2, (guard_headers, guard_opt)
    end
and aux_reset_times_branch env (label, st, ac_opt) = 
(*  Env are branch specific - i.e. env of one branch can not depend of an other branch
        i.e. timer are not preserved under recursion 

    TODO Future work support the following
    Case 1: already supported  
    µx. {
        l1 {timer x1}: !ping {timer y1} !ping{} - x;
        l2 {timer x2}: ?pong {timer z1} ?pong{} .;
    }

    timers y1 (resp z1) are only defined in one round of branch l1 (resp l2)
        i.e.    - branche1: STvar x => disable y1 
                - branch2: . => disable z1
        and y1 and z1 call are indexed on linear static time from the begining so we can use integer for SetFireTime

    Case 2: TODO
    µx. {
        l1 {timer x1}: !ping {timer y1} !ping{} - x;
        l2 {timer x2, timer_get_or_create x1}: ?pong {timer z1} ?pong{x1>50} .;
    }
    timers x1 (resp x2) should be maintained during recursive call
        e.g. let us take a run composed of two rounds: l1 then l2
        in round l2, x1 should be persisted from one round to an other 
    *)
match ac_opt with
| Some ac ->
    let new_env, new_ac = aux_reset_times_ac env st.place st.value ac in
    (label, reset_timers new_env st, Some new_ac)
| None -> (label, reset_timers env st, ac_opt)

and reset_timers_ env place st0 = 
match st0 with
| STSend ({place = place_st; value=ConstrainedType (mt, ac)}, st) ->
    let new_env, new_ac = aux_reset_times_ac env place st0 ac in
    STSend ({place = place_st; value=ConstrainedType (mt, new_ac)}, reset_timers new_env st)
| STRecv ({place = place_st; value=ConstrainedType (mt, ac)}, st) ->
    let new_env, new_ac = aux_reset_times_ac env place st0 ac in
    STRecv ({place = place_st; value=ConstrainedType (mt, new_ac)}, reset_timers new_env st)
| STBranch branches -> STBranch (List.map (aux_reset_times_branch env) branches)
| STSelect branches -> STSelect (List.map (aux_reset_times_branch env) branches)

(* Just propagate*)
| STEnd -> STEnd 
| STVar x -> STVar x (* TODO FIXME should disable timers *)
| STPolyVar x -> STPolyVar x
| STRec (x, st) -> STRec (x, reset_timers env st)
| STRecv (mt, st) -> STRecv (mt, reset_timers env st)
| STSend (mt, st) -> STSend (mt, reset_timers env st)
and reset_timers env st = {
    place = st.place;
    value = reset_timers_ env st.place st.value
}
and reset_timers_of st = reset_timers Env.empty st

(**************************************************************)


let rec gtransform_mt_ place = function 
| ConstrainedType ({value=SType _}, ac) -> failwith "semantics ????? " 
| SType st -> begin 
    (*logger#info ">>> Before \n %s" (show_session_type st);*)
    let st = separate_lb_hb_timers st in
    let st = reset_timers_of st in
    (*logger#info ">>> After \n %s" (show_session_type st);*)
    SType st
end

(* Identity *)
| CType ct -> CType ct (* Since guard can only apprears in a procotoldef *)
| CompType ct -> CompType ct 
| ConstrainedType (mt, ac) -> ConstrainedType (mt, ac)
and gtransform_mt mt = map_place gtransform_mt_ mt

and gtransform_citem_ place = function
(* protocoldef can only be hidden in subterm *)
| Term t -> Term (gtransform_term t)

(* identity *)
| State s -> State s
| Method s -> Method s
| Contract c -> Contract c
|Inport p ->Inport p
| Outport p -> Outport p
| Include _ -> raise (Error.DeadbranchError "gtransform_citem: Include not supported, include should have been resolved")
and gtransform_citem citem = map_place gtransform_citem_ citem 

and gtransform_cdcl_ place = function
| ComponentStructure cdcl -> ComponentStructure {
    cdcl with
        (* no protocol def in args *)
        body = List.map gtransform_citem cdcl.body
}
| ComponentAssign _ -> failwith "component assign is not supported by guard transform, semantics undefined"
and gtransform_cdcl cdcl = map_place gtransform_cdcl_ cdcl 

and gtransform_term_ place = function
| EmptyTerm -> EmptyTerm
| Comments c -> Comments c
| Stmt stmt -> Stmt stmt
| Component cdcl -> Component (gtransform_cdcl cdcl)
| Function fcdcl -> Function fcdcl
| Typealias _ -> failwith "Typealias is not supported by guardTransform semantics to be defined" 
| Typedef {place=p_p; value=ProtocolDef (x, mt)} ->
    Typedef {place=p_p; value=ProtocolDef (x, gtransform_mt mt)} 
| Typedef _ as t -> t
| Derive _ -> raise (Error.PlacedDeadbranchError (place, "Derivation should have been removed before starting the code generation"))
and gtransform_term term = map_place gtransform_term_ term

and gtransform_program terms = 
    List.map gtransform_term terms