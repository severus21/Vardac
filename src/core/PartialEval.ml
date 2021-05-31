open Error
open Fieldslib
open IR

(* The source calculus. *)
module S = IR 
(* The target calculus. *)
module T = IR 

(* Environment *)
module Env = Atom.AtomMap 

type env = { eempty: string Env.t} [@@deriving fields] 

let fresh_env () = { eempty = Env.empty }

let map_snd f = function (x,y) -> (x, f y)

(*debug only*)
let print_env env =
    let print_keys env = Env.iter (fun x _ -> Printf.printf "%s;" (Atom.atom_to_str Atom.value x)) env in

    print_newline ();
    print_string "Env = {";

    List.iter (
        function (name, l) -> print_string ("\t"^name^"\n\t\t"); print_keys (l env);print_newline (); 
    ) [("empty", eempty)];
    
    print_string "}";
    print_newline ()

let rec peval_place peval_value env ({ AstUtils.place ; AstUtils.value}: 'a AstUtils.placed) = 
    let env, value = peval_value env place value in
    env, {AstUtils.place; AstUtils.value}


let rec peval_mtype env place : _main_type -> env * _main_type = function x -> env, x 
and pe_mtype env: main_type -> env * main_type = peval_place peval_mtype env

(************************************ Expr & Stmt *****************************)
and peval_stmt env place : _stmt -> env * _stmt = function x -> env, x 
and pe_stmt env: stmt -> env * stmt = peval_place peval_stmt env


(************************************ Channels *****************************)
and peval_channel_dcl env place : _channel_dcl -> env * _channel_dcl = function x -> env, x 
and pe_channel_dcl env: channel_dcl -> env * channel_dcl = peval_place peval_channel_dcl env

(************************************ Component *****************************)
and peval_component_item env place : _component_item -> env * _component_item = function x -> env, x
and pe_component_item env: component_item -> env * component_item = peval_place peval_component_item env

and peval_component_dcl env place : _component_dcl -> env * _component_dcl = function  
| ComponentAssign  _ as x -> env, x 
| ComponentStructure cdcl ->
    (* Collect contracts *)
    let collect_contracts env (x:component_item) = 
        match x.value with
        | Contract c -> Env.add c.value.method_name c env 
        | _ -> env 
    in
    let contracts : IR.contract Env.t = List.fold_left collect_contracts Env.empty cdcl.body in (* method_name -> contract *)

    (* Remove contracts from body and pair method with contracts *)
    let rec get_method_name (m: method0) = 
        match m.value with
        | CustomMethod m -> m.name
        | OnDestroy m | OnStartup m -> get_method_name m
    in
    let body = List.filter_map (function (item:component_item) ->
        match item.value with 
        | Contract _ -> None 
        | Method m as x -> begin
            let rec aux (m: method0) = 
                match m.value with
                | CustomMethod _m -> begin
                        let contract : contract = (Env.find _m.name contracts) in
                        { AstUtils.place; value = (CustomMethod { _m with contract_opt = Some contract }) }
                end 
                | OnDestroy m -> { AstUtils.place; value = OnDestroy (aux m) }
                | OnStartup m -> { AstUtils.place; value = OnStartup (aux m) }
            in
            try
                Some { AstUtils.place; value = Method (aux m) }
            with Not_found ->
                Some item 
        end
        | x -> Some item 
        ) cdcl.body in
    
    let new_env, citems = List.fold_left_map pe_component_item env body in 
    new_env, ComponentStructure {cdcl with body = citems }

and pe_component_dcl env: component_dcl -> env * component_dcl = peval_place peval_component_dcl env

(************************************ Program *****************************)
and peval_term env place : _term -> env * _term = function
| Comments c -> env, Comments c
| Stmt stmt -> map_snd (fun x -> Stmt x) (pe_stmt env stmt)
| Channel chan -> map_snd (fun x -> Channel x) (pe_channel_dcl env chan)
| Component comp -> map_snd (fun x -> Component x) (pe_component_dcl env comp)
| Typedef (x, mt_opt) -> begin
    match mt_opt with
    | None -> env, Typedef (x, None)
    | Some mt -> let env, mt = pe_mtype env mt in env, Typedef (x, Some mt)
end
and pe_term env: term -> env * term = peval_place peval_term env

and peval_program (terms: IR.program) : IR.program = 
    (*  Hydrate env, namely:
        -  collect the contract
        And remove contracts from component_item + add contract inside method0 structure *)
    let env, program = List.fold_left_map pe_term (fresh_env ()) terms in
    program

