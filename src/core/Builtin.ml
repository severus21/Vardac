let builtin_fcts = [
    "placeof", "abs -> place option", "Give the current place where the abstraction is running. Returns None if the abstraction is not yet placed.";
    "sqrt", "float -> float", "Compute the square root of a float.";
    "none", "unit", "unit term";
    "first", "List<T> -> T", "Return the first element of list, failed if empty";
    "last", "List<T> -> T", "Return the last element of list, failed if empty";
    "initiate_session", "TODO", "TODO";
    "fire", "TODO", "TODO";
    "receive", "TODO", "TODO";
    "switch", "session -> label* sessionTODO", "TODO";
    "remote_endpoint", "session ->TODO", "";
    "ipaddress", "TODO->TODO", "";
    "places", "TODO", "TODO";
    "pick", "TODO", "Random choice in a sequence, failed if empty";
    "children", "TODO", "child of places";
]

let builtin_atomic_types = [
  "unit";
  "int";
  "float";
  "string";
  "bool";
  "place";
  "ipaddress";
  "place_selector"
  ]

module BuiltinSet = 
 Set.Make(String)                     

let builtin_expr_env = BuiltinSet.of_list (List.map (function x,_,_ -> x) builtin_fcts)    
let builtin_type_env = BuiltinSet.of_list  builtin_atomic_types    
let is_builtin_expr x = try let _ = BuiltinSet.find x builtin_expr_env in true with Not_found -> false                   
let is_builtin_type x = try let _ = BuiltinSet.find x builtin_type_env in true with Not_found -> false                   

let is_builtin_component x = false
let is_builtin_channel x = false
