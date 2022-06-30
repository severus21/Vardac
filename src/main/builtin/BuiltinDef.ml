open Core
open AstUtils
open IR

open BuiltinSignature

(* FT -> inductive type representation *)
let builtin_inductive_types = [ 
    (TBLabel, [mtype_of_ft TStr]) (* _0_ only *)
]

(********** Regexp selector **********)
let re_tuple_attr = Str.regexp {|^_\([0-9]\)$|}
let re_inductive_attr = Str.regexp {|^_\([0-9]\)_$|}

(********** White list selector **********)
    
let builtin_access : (string * string) list = [
]

(* TODO
encode builtin fcts as type constructor to be able to type check that each plg correctly implements them
*)

(* name, signature string, description, signature () -> .. , neeed a closure to generate fresh types *)
let builtin_fcts : (string * string * string * (unit -> main_type)) list= [
    "bind", "in|ouport -> bridge -> ()", "bind a port with a bridge", t_bind; 
    "bind_in", "in|ouport -> bridge -> ()", "bind a port with a bridge", t_bind; 
    "bind_out", "in|ouport -> bridge -> ()", "bind a port with a bridge", t_bind; 
    "activationsat", "place -> set<activation_ref>", "", t_activationat;
    "add2dict", "dict<k,v> -> k -> v -> ()", "add in place",t_add2dict ;
    "bridge", "() -> Bridge<'A, 'B, 'a>", "create a new bridge with a fresh id",
    fresh_tbridge;
    "string_of_bridge", "bridge -> string", "", t_string_of_bridge;
    "fire", "STSend<'msg, 'continuation> -> 'msg -> 'continuation", "TODO", t_fire
    ;
    "current_place", " unit -> place", "current place", t_current_place;
    "dict", "() -> dict", "create a new dict", t_dict;
    "first", "Tuple<'a, 'b> -> 'a", "Return the first element of list, failed if empty", t_first;
    "get2dict", "dict<k,v> -> k -> v", "get", t_get2dict;
    "initiate_session_with", "TODO", "TODO", 
    t_initiate;
    "listget", " list<T> -> int -> t", "", t_listget;
    "setlength", "set -> int", "", t_setlength; 
    "pick", "dict<k,v> -> v", "Random choice in a sequence, failed if empty", t_select; (*TODO*)
    "placeof", "abs -> place option", "Give the current place where the abstraction is running. Returns None if the abstraction is not yet placed.", t_placeof;
    "place_to_string", "place -> string", "", t_place_to_string;
    "int_to_string", "int -> string", "", t_place_to_string;
    "int_of_string", "string -> int", "", t_int_of_string;
    "long_to_string", "long -> string", "", t_place_to_string;
    "places", "() -> list<place>", "TODO", t_places;
    "print", "string -> unit", "TODO", t_print;
    "receive", "STReceive<'msg,'continuation> -> 'msg * 'continuation", "TODO", t_receive;
    "remove2dict", "dict<k,v> -> k -> v", "remove and return", t_remove2dict;
    "second", "Tuple<'a, 'b> -> 'b", "Return the second element of list, failed if empty", t_second;
    "select_places", "label -> (place -> bool) -> place", "", t_select;
    "sleep", "int -> unit", "sleep", t_sleep;
    "select", "st -> label -> st", "select", t_select;
    "option_get", "option<'a> -> 'a", "", t_select; (*TODO*)
    "session_from", "session -> activation<>", "get the initiater of the session", t_select; (*TODO*)
    "session_to_2_", "session -> activation<>", "TODO", t_session_to_2_;


    "forge_activation_ref", "() -> activation_ref", "", t_forge_activation_ref;
    "one_hop_activation_ref", "activation_ref -> activation_ref -> activation_ref", "parent -> hidden -> new activation ref", t_one_hop_activation_ref;
    "activationid", "activation_ref -> activation_id", "TODO", t_select; (*TODO*)
    "sessionid", "'st -> int", "Return the id of the session", t_sessionid;
    "ip", "place -> string", "TODO", t_ip;
    "port", "place -> int", "TODO", t_select;

    "is_ok", "result<..,..> -> bool", "TODO", t_is_ok;
    "get_ok", "result<..., error> -> ...", "TODO", t_get_ok;
    "is_err", "result<..,..> -> bool", "TODO", t_is_ok;
    "get_err", "result<..., error> -> err", "TODO", t_get_err;

    "long_of_int", "int -> long", "...", t_long_of_int;

    "exit", "() -> ()", "run in guardian, terminate the guardian and all activations", t_exit;
    "debug", "string -> ()", "TODO", t_debug;
    "info", "string -> ()", "TODO", t_debug;
    
    "aget", "array<item> -> int -> item", "...", t_aget;
    "asize", "array<item> -> int", "...", t_asize;

    "leftactivations", "...", "discover activations interconnecting by the bridge", t_leftactivations;
    "rightactivations", "...", "discover activations interconnecting by the bridge", t_rightactivations;
    "leftregister", "...", "...", t_leftregister;
    "rightregister", "...", "...", t_leftregister;

    "list2array", "array<item> -> list<item>", "...", t_list2array; 
    "append", "list<item> -> item -> ()", "...", t_list_append;

    "range", "int -> int -> range", "", t_range;
    "time", "() -> int", "miliseconds", t_time; 

    (* Not expose to user, but should be implemented by the underlying plugin *)
    "__get_intermediate_port", "session -> port", "TODO", t___get_intermediate_port;

    "bridgeof", "port | inport -> ", "TODO", t_bridgeof;
    "bridgeof_in", "port | inport -> ", "TODO", t_bridgeof;
    "bridgeof_out", "port | inport -> ", "TODO", t_bridgeof;
    (*


    "sqrt", "float -> float", "Compute the square root of a float.";
    "last", "List<T> -> T", "Return the last element of list, failed if empty";
    "switch", "session -> label* sessionTODO", "TODO";
    "remote_endpoint", "session -> TODO", "";
    "ipaddress", "TODO->TODO", "";
    "children", "TODO", "child of places";
    *)
]

let builtin_derivations = [
    "rpc"
]

(****************************************)