(* TODO clean builtin and write type information + descrption*)
open AstUtils
open IR
open Easy_logging

let logger = Logging.make_logger "_1_ compspec.Builtin" Debug [];;
let fplace = (Error.forge_place "Builtin.*" 0 0)
let auto_fplace smth = {place = fplace; value=smth}

(* load mtype_of_... *)
include AstUtils2.Mtype.Make(struct let fplace = fplace end)

let quantify labels make : main_type =
    let vars = List.map Atom.fresh labels in
    let rec aux = function
    | [] -> make vars 
    | x::xs -> mtype_of_ct (TForall(x, aux xs))
    in
    aux vars
(* FIXME TODO can we mutualize functions with type reconstruction ???*)
let fresh_tbridge () = 
    let a = Atom.fresh "a" in
    let b = Atom.fresh "b" in
    let p = Atom.fresh "p" in
    
    (*quantify ["a"; "b"; "p"]
    ( function [a; b; p] ->
        mtype_of_ct (TArrow(
            mtype_poly_of_var p,
            mtype_of_ct (TBridge {
                in_type  = mtype_poly_of_var a;
                out_type = mtype_poly_of_var b;
                protocol = mtype_poly_of_var p
            })
        ))
    )*)
    mtype_of_ct (TArrow(
        mtype_of_ft TWildcard,
        mtype_of_ct (TBridge {
            in_type  = mtype_of_ft TWildcard;
            out_type = mtype_of_ft TWildcard;
            protocol = mtype_of_st STEnd
        })
    ))


let t_fire () = 
    (*quantify 
        ["msg"; "continuation"] 
        (function [msg; continuation] -> 
            mtype_of_ct(TArrow(
                mtype_of_st(STSend(
                    mtype_poly_of_var msg, auto_fplace(STPolyVar continuation) 
                )),
                mtype_of_ct(TArrow(
                    mtype_poly_of_var msg,
                    mtype_poly_of_svar continuation
                ))
            ))
        )
    *)
    mtype_of_ct(TArrow(
        mtype_of_st(STSend(
            mtype_of_ft TWildcard, auto_fplace STEnd 
        )),
        mtype_of_ct(TArrow(
            mtype_of_ft TWildcard,
            mtype_of_ft TWildcard
        ))
    ))
let t_receive () = 
    (*quantify 
        ["msg"; "continuation"] 
        (function [msg; continuation] -> 
            mtype_of_ct(TArrow(
                mtype_of_st(STSend(
                    mtype_poly_of_var msg, auto_fplace(STPolyVar continuation) 
                )),
                mtype_of_ct(TArrow(
                    fresh_tbridge (), (* Temporary needed until we use control flow information to retrieve statically the bridge of a session - because we need it when doing the Rewrite pass *)
                    mtype_of_ct(TTuple[
                        mtype_poly_of_var msg;
                        mtype_poly_of_svar continuation
                    ])
                ))
            ))
        )*)
    mtype_of_ct(TArrow(
        mtype_of_st(STRecv(
            mtype_of_ft TWildcard, auto_fplace STEnd 
        )),
        mtype_of_ct(TArrow(
            mtype_of_ft TWildcard,
            mtype_of_ft TWildcard
        ))
    ))

let t_initiate () =
    let a = Atom.fresh "A" in
    let b = Atom.fresh "B" in
    
    (*mtype_of_ct (TArrow(
        fresh_outport (),
        mtype_of_ct(TArrow(
            mtype_of_ct(TForall(a, mtype_poly_of_cvar b)), 
            mtype_of_ct(TActivationRef (
                mtype_of_ct(TForall(b, mtype_poly_of_cvar b))
            ))
        ))
    ))*)

    mtype_of_ct(TArrow(
        mtype_of_ft TWildcard,
        mtype_of_ct(TArrow(
            mtype_of_ft TWildcard,
            mtype_of_ft TWildcard
        ))
    ))

let t_print () = 
    mtype_of_ct(TArrow(
        mtype_of_ft TStr,
        mtype_of_ft TVoid
    ))

let t_first () = 
    (*quantify ["a"; "b"] (function [a; b] -> 
        mtype_of_ct(TArrow(
            mtype_of_ct (TTuple [mtype_poly_of_var a; mtype_poly_of_var b]),
            mtype_poly_of_var a 
        ))
    )*)

    mtype_of_ct(TArrow(
        mtype_of_ft TWildcard,
        mtype_of_ft TWildcard
    ))

let t_second () = 
    (*quantify ["a"; "b"] (function [a; b] -> 
        mtype_of_ct(TArrow(
            mtype_of_ct (TTuple [mtype_poly_of_var a; mtype_poly_of_var b]),
            mtype_poly_of_var b
        ))
    )*)
    mtype_of_ct(TArrow(
        mtype_of_ft TWildcard,
        mtype_of_ft TWildcard
    ))

let t_current_place () =
    mtype_of_ct(TArrow(
        mtype_of_ft TVoid,
        mtype_of_ft TPlace
    ))
let t_places () =
    mtype_of_ct(TArrow(
        mtype_of_ft TVoid,
        mtype_of_ct (TList (mtype_of_ft TPlace))
    ))
let t_place_to_string () = 
    mtype_of_ct(TArrow(
        mtype_of_ft TPlace,
        mtype_of_ft TStr
    ))

let t_listget () =
    (*quantify ["a"] (function [a] ->
        mtype_of_ct(TArrow(
            mtype_of_ct (TList (mtype_poly_of_var a)),
            mtype_of_ct(TArrow(
                mtype_of_ft TInt,
                mtype_of_ft TPlace
            ))
        ))
    )*)
    mtype_of_ct(TArrow(
        mtype_of_ft TWildcard,
        mtype_of_ct(TArrow(
            mtype_of_ft TWildcard,
            mtype_of_ft TWildcard
        ))
    ))

let t_placeof () = 
    (*mtype_of_ct(TArrow(
        quantify ["A"] (function  [a] -> mtype_of_ct (TActivationRef (mtype_poly_of_cvar a))),
        mtype_of_ft TPlace
    ))
    *)
    mtype_of_ct(TArrow(
        mtype_of_ft TWildcard,
        mtype_of_ft TWildcard
    ))
let t_select () = 
    (*mtype_of_ct(TArrow(
        quantify ["vp"] (function[vp] -> mtype_of_ct (TVPlace (mtype_poly_of_var vp))),
        mtype_of_ct (TArrow(
            mtype_of_ft TPlace,
            mtype_of_ft TBool
        ))
    ))*)
    mtype_of_ct(TArrow(
        mtype_of_ft TWildcard,
        mtype_of_ct(TArrow(
            mtype_of_ft TWildcard,
            mtype_of_ft TWildcard
        ))
    ))

let t_activationat () =
    (*mtype_of_ct(TArrow(
        mtype_of_ft TPlace,
        mtype_of_ct (TSet (
            mtype_of_ct (TActivationRef(mtype_of_ft TWildcard))
        ))
    ))*)

    mtype_of_ct(TArrow(
        mtype_of_ft TWildcard,
        mtype_of_ft TWildcard
    ))

let t_sleep () = 
    mtype_of_ct(TArrow(
        mtype_of_ft TInt,
        mtype_of_ft TVoid
    ))
let t_add2dict () = 
   (* quantify ["k"; "v"] (function [k;v] ->
        mtype_of_ct(TArrow(
            mtype_of_ct (TDict (mtype_poly_of_var k, mtype_poly_of_var v)),
            mtype_of_ct(TArrow(
                mtype_poly_of_var k,
                mtype_of_ct(TArrow(
                    mtype_poly_of_var v,
                    mtype_of_ft TVoid
                ))
            ))
        ))    
    )
*)
    mtype_of_ct(TArrow(
        mtype_of_ft TWildcard,
        mtype_of_ct(TArrow(
            mtype_of_ft TWildcard,
            mtype_of_ct(TArrow(
                mtype_of_ft TWildcard,
                mtype_of_ft TWildcard
            ))
        ))

    ))
let t_get2dict () = 
    (*quantify ["k"; "v"] (function [k;v] ->
        mtype_of_ct(TArrow(
            mtype_of_ct (TDict (mtype_poly_of_var k, mtype_poly_of_var v)),
            mtype_of_ct(TArrow(
                mtype_poly_of_var k,
                mtype_poly_of_var v
            ))
        ))    
    )
*)
    mtype_of_ct(TArrow(
        mtype_of_ft TWildcard,
        mtype_of_ct(TArrow(
            mtype_of_ft TWildcard,
            mtype_of_ft TWildcard
        ))

    ))
let t_remove2dict () = 
    (*quantify ["k"; "v"] (function [k;v] ->
        mtype_of_ct(TArrow(
            mtype_of_ct (TDict (mtype_poly_of_var k, mtype_poly_of_var v)),
            mtype_of_ct(TArrow(
                mtype_poly_of_var k,
                mtype_of_ft TVoid
            ))
        ))    
    ))*)
    mtype_of_ct(TArrow(
        mtype_of_ft TWildcard,
        mtype_of_ct(TArrow(
            mtype_of_ft TWildcard,
            mtype_of_ct(TArrow(
                mtype_of_ft TWildcard,
                mtype_of_ft TWildcard
            ))
        ))

    ))
let t_dict () = 
    (*mtype_of_ct(TArrow(
        mtype_of_ft TVoid,
        quantify ["k"; "v"] (function [k;v] ->
            mtype_of_ct (TDict (mtype_poly_of_var k, mtype_poly_of_var v))
        )
    ))*)

    mtype_of_ct(TArrow(
        mtype_of_ft TWildcard,
        mtype_of_ft TWildcard
    ))

let t_string_of_bridge () = 
    mtype_of_ct(TArrow(
        fresh_tbridge (),
        mtype_of_ft TStr 
    ))


let t_sessionid () =
    mtype_of_ct (TArrow(
        quantify ["st"] (function [st] -> mtype_poly_of_svar st),
        mtype_of_ft TInt
    ))

let re_tuple_attr = Str.regexp "^_\([0-9]\)$"
let re_inductive_attr = Str.regexp "^_\([0-9]\)_$"
let is_inductive_attr x = Str.string_match re_inductive_attr x 0
let is_tuple_attr x = Str.string_match re_tuple_attr x 0
let pos_of_tuple_attr x =
    assert(is_tuple_attr x);
    int_of_string (Str.replace_first re_tuple_attr "\1" x)
let pos_of_inductive_attr x =
    assert(is_inductive_attr x);
    int_of_string (Str.replace_first re_inductive_attr "\1" x)


(*TODO use re_.. for builtin_access *)
    
let builtin_access : (string * string) list = [
    "_0_", "access part of user inductive type";
    "_1_", "...";
    "_3_", "...";
    "_4_", "...";
    (* Session attributes *)
    (* TODO session_from/session_to/session_to_2_ => syntaxic sugar s.from s.to s.to_2_*)

    "_0", "access part of a tuple";
    "_1", "...";
]

(* name, signature string, description, signature () -> .. , neeed a closure to generate fresh types *)
let builtin_fcts : (string * string * string * (unit -> main_type)) list= [
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
    "pick", "dict<k,v> -> v", "Random choice in a sequence, failed if empty", t_select; (*TODO*)
    "placeof", "abs -> place option", "Give the current place where the abstraction is running. Returns None if the abstraction is not yet placed.", t_placeof;
    "place_to_string", "place -> string", "", t_place_to_string;
    "places", "() -> list<place>", "TODO", t_places;
    "print", "string -> unit", "TODO", t_print;
    "receive", "STReceive<'msg,'continuation> -> 'msg * 'continuation", "TODO", t_receive;
    "remove2dict", "dict<k,v> -> k -> v", "remove and return", t_remove2dict;
    "second", "Tuple<'a, 'b> -> 'b", "Return the second element of list, failed if empty", t_second;
    "select_places", "label -> (place -> bool) -> place", "", t_select;
    "sessionid", "'st -> int", "Return the id of the session", t_sessionid;
    "sleep", "int -> unit", "sleep", t_sleep;
    "select", "st -> label -> st", "select", t_select;
    "option_get", "option<'a> -> 'a", "", t_select; (*TODO*)
    "session_from", "session -> activation<>", "get the initiater of the session", t_select; (*TODO*)
    "session_to_2_", "session -> activation<>", "TODO", t_select; (*TODO*)
    "activationid", "activation_ref -> activation_id", "TODO", t_select; (*TODO*)
    "sessionid", "'st -> session_id", "TODO", t_select; (*TODO*)
    "ip", "place -> string", "TODO", t_select;
    "port", "place -> int", "TODO", t_select;
    (*


    "sqrt", "float -> float", "Compute the square root of a float.";
    "last", "List<T> -> T", "Return the last element of list, failed if empty";
    "switch", "session -> label* sessionTODO", "TODO";
    "remote_endpoint", "session -> TODO", "";
    "ipaddress", "TODO->TODO", "";
    "children", "TODO", "child of places";
    *)
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


let builtin_derivations = [
    "rpc"
]

module BuiltinSet = Set.Make(String)                     
let builtin_htbl = Hashtbl.of_seq (List.to_seq (List.map (function (x, a, b, mt) -> (x, (a,b, mt))) builtin_fcts))

let builtin_expr_env = BuiltinSet.of_list ((List.map (function x,_,_,_ -> x) builtin_fcts)@(List.map fst builtin_access))    
let builtin_type_env = BuiltinSet.of_list  builtin_atomic_types    
let builtin_derivation_env = BuiltinSet.of_list  builtin_derivations    
let is_builtin_expr x = try let _ = BuiltinSet.find x builtin_expr_env in true with Not_found -> false                   
let is_builtin_type x = try let _ = BuiltinSet.find x builtin_type_env in true with Not_found -> false                   
let is_builtin_derivation x = try let _ = BuiltinSet.find x builtin_derivation_env in true with Not_found -> false                   

let type_of place x : IR.main_type = 
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

let is_builtin_component _ = false