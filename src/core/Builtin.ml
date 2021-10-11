let builtin_fcts = [
    "placeof", "abs -> place option", "Give the current place where the abstraction is running. Returns None if the abstraction is not yet placed.";
    "sqrt", "float -> float", "Compute the square root of a float.";
    "none", "unit", "unit term";
    "first", "Tuple/List<T> -> T", "Return the first element of list, failed if empty";
    "second", "Tuple/List<T> -> T", "Return the second element of list, failed if empty";
    "last", "List<T> -> T", "Return the last element of list, failed if empty";
    "initiate_session", "TODO", "TODO";
    "fire", "TODO", "TODO";
    "receive", "TODO", "TODO";
    "switch", "session -> label* sessionTODO", "TODO";
    "remote_endpoint", "session -> TODO", "";
    "ipaddress", "TODO->TODO", "";
    "places", "TODO", "TODO";
    "pick", "TODO", "Random choice in a sequence, failed if empty";
    "children", "TODO", "child of places";
    "bridge", "() -> Bridge<'A, 'B, 'a>", "create a new bridge with a fresh id";
    "initiate_session_with", "TODO", "TODO";
    "print", "string -> unit", "TODO";
    "dict", "() -> dict", "create a new dict";
    "add2dict", "dict<k,v> -> k -> v -> ()", "add in place";
    "get2dict", "dict<k,v> -> k -> v", "get";
    "remove2dict", "dict<k,v> -> k -> v", "remove and return";
    "sessionid", "session -> id", "TODO";
    "nth", "tuple -> int -> ...", "nème elmt";
    "sleep", "int -> unit", "sleep";
]

let builtin_atomic_types = [
  "unit";
  "int";
  "float";
  "string";
  "bool";
  "place";
  "ipaddress";
  "place_selector";
  "uuid";
  ]

module BuiltinSet = Set.Make(String)                     
let builtin_htbl = Hashtbl.of_seq (List.to_seq (List.map (function (x, a, b) -> (x, (a,b))) builtin_fcts))

let builtin_expr_env = BuiltinSet.of_list (List.map (function x,_,_ -> x) builtin_fcts)    
let builtin_type_env = BuiltinSet.of_list  builtin_atomic_types    
let is_builtin_expr x = try let _ = BuiltinSet.find x builtin_expr_env in true with Not_found -> false                   
let is_builtin_type x = try let _ = BuiltinSet.find x builtin_type_env in true with Not_found -> false                   

let type_of x : IR.main_type = 
  let (sign, _) = Hashtbl.find builtin_htbl x in
  (*match Frontend.Parse.read "Core.Builtin" sign with
  | _ ->*) failwith "it works"

let desc_of x : string = 
  let (_, desc) = Hashtbl.find builtin_htbl x in
  desc 

let is_builtin_component _ = false