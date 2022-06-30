open Core
open IR
open Easy_logging
open Utils
open AstUtils

let logger = Logging.make_logger "_1_ vardac" Debug [];;


let rec reduce_component_item place : _component_item -> _component_item = function 
| Contract c -> raise (Error.PlacedDeadbranchError (place, "contract should be paired with method before partial_evaluation, therefore no contract should remains as a component_item"))
| Term t -> Term ((r_term t))
| citem -> citem

and r_component_item citem : component_item = map_place (transparent_plgannot(reduce_component_item)) citem

and reduce_component_dcl place : _component_dcl -> _component_dcl = function  
| ComponentAssign _ as citem -> citem 
| ComponentStructure cdcl ->
    (* Collect contracts *)
    let collect_contracts env (x:component_item) = 
        match x.value.v with
        | Contract c -> 
            Atom.VMap.add c.value.method_name c env 
        | _ -> env 
    in
    let contracts : contract Atom.VMap.t = List.fold_left collect_contracts Atom.VMap.empty cdcl.body in (* method_name -> contract *)

    (* Remove contracts from body and pair method with contracts *)
    let body = List.filter_map (function (item:component_item) ->
        match item.value.v with 
        | Contract _ -> None 
        | Method m -> begin
            let rec aux (m: method0) = 
                let _m = m.value in
                let contract : contract = Atom.VMap.find _m.name contracts in
                { AstUtils.place; value = { _m with contract_opt = Some contract } }
            in
            try
                Some {AstUtils.place; value = {plg_annotations = item.value.plg_annotations; v=Method (aux m) }}
            with Not_found ->
                Some item 
        end
        | x -> Some item 
        ) cdcl.body 
    in
    ComponentStructure {cdcl with body = body }

and r_component_dcl cdcl: component_dcl = map_place (reduce_component_dcl ) cdcl

(************************************ Program *****************************)
and reduce_term place : _term -> _term = function
| Component comp -> Component (r_component_dcl comp)
| t -> t

and r_term t : term = map_place (transparent_plgannot(reduce_term)) t

and reduce_program (terms: IR.program) : IR.program = 
    List.map r_term terms

(**********************************************************)
let name = "Reduce"
let displayed_pass_shortdescription = "IR has been reduced"
let displayed_ast_name = "reduced IR"
let show_ast = true
let global_at_most_once_apply = false

let precondition program = program
let postcondition program = program
let apply_program = reduce_program

