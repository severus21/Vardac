open Core
open IR_common
open BuiltinTypes
open BuiltinDef

(************** Classification *************)
let is_inductive_attr x = Str.string_match re_inductive_attr x 0
let is_tuple_attr x = Str.string_match re_tuple_attr x 0

module BuiltinSet = Set.Make(String)                     
let builtin_htbl = Hashtbl.of_seq (List.to_seq (List.map (function (x, a, b, mt) -> (x, (a,b, mt))) builtin_fcts))

let inductive_htbl = Hashtbl.of_seq (List.to_seq builtin_inductive_types)

let is_builtin_inductive_type ft = 
    Hashtbl.find_opt inductive_htbl ft <> None 

let builtin_expr_env = 
    BuiltinSet.of_list (
        (List.map (function x,_,_,_ -> x) builtin_fcts)
        @ (List.map fst builtin_access)
    )    

let builtin_type_env = BuiltinSet.of_list  builtin_atomic_types    
let builtin_derivation_env = BuiltinSet.of_list  builtin_derivations    
let is_builtin_expr x = 
    try 
        (* whiteliste based selection *)
        let _ = BuiltinSet.find x builtin_expr_env in true 
    with Not_found -> 
        (* Regexp based selection *)
        is_inductive_attr x || is_tuple_attr x

let is_builtin_type x = try let _ = BuiltinSet.find x builtin_type_env in true with Not_found -> false                   
let is_builtin_derivation x = try let _ = BuiltinSet.find x builtin_derivation_env in true with Not_found -> false                   

let is_builtin_component _ = false

(************** Extraction *****************)
let pos_of_tuple_attr x =
    assert(is_tuple_attr x);
    int_of_string (Str.replace_first re_tuple_attr {|\1|} x)
let pos_of_inductive_attr x =
    assert(is_inductive_attr x);
    int_of_string (Str.replace_first re_inductive_attr {|\1|} x)

let sig_of_builtin_inductive_type ft = 
    assert(is_builtin_inductive_type ft);
    Hashtbl.find inductive_htbl ft 


let type_of place x : main_type = 
    assert(is_builtin_expr x);
    let (_, _, make_mt) = 
        try
            Hashtbl.find builtin_htbl x 
        with Not_found -> raise (Error.PlacedDeadbranchError (place, Printf.sprintf "Builtin function [%s] has not a builtin signature" x))
    in
    (* TODO type are incorrect or forall is incorectly used because
    the builtin types introduce free type variable that are outside the scope of their forall quantifier
    
    
    For now this function only return the shape of the type (arrow shape since it used to detect the number of expected args) 
    *)
    make_mt ()


let desc_of x : string = 
    let (_, desc,_) = 
        try
            Hashtbl.find builtin_htbl x 
        with Not_found -> raise (Error.DeadbranchError (Printf.sprintf "Builtin function [%s] has no attached docstring" x))
    in
    desc 