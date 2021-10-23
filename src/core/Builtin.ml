(* TODO clean builtin and write type information + descrption*)
open AstUtils
open IR
let fplace = (Error.forge_place "Builtin.*" 0 0)
let auto_fplace smth = {place = fplace; value=smth}

let mtype_of_var x = 
    auto_fplace(CType(auto_fplace (TPolyVar x)))
let mtype_of_svar x = 
    auto_fplace(SType(auto_fplace (STPolyVar x)))
let mtype_of_cvar x = 
    auto_fplace(CompType(auto_fplace (TPolyCVar x)))
let mtype_of_ct ct = 
    auto_fplace(CType(auto_fplace ct))
let mtype_of_st st = 
    auto_fplace(SType(auto_fplace st))
let mtype_of_ft ft = 
    mtype_of_ct (TFlatType ft)

(* FIXME TODO can we mutualize functions with type reconstruction ???*)
let fresh_tbridge () = 
    let a = Atom.fresh "a" in
    let b = Atom.fresh "b" in
    let p = Atom.fresh "p" in
    mtype_of_ct (TArrow(
        mtype_of_ft TVoid,
        mtype_of_ct(TForall(
            a, 
            mtype_of_ct(TForall (
                b, 
                mtype_of_ct(TForall(
                    p, 
                    mtype_of_ct (TBridge {
                        in_type  = mtype_of_var a;
                        out_type = mtype_of_var b;
                        protocol = mtype_of_var p
                    })
                ))
            ))
        ))
    ))

let quantify labels make : main_type =
    let vars = List.map Atom.fresh labels in
    let rec aux = function
    | [] -> make vars 
    | x::xs -> mtype_of_ct (TForall(x, aux xs))
    in
    aux vars

let t_fire () = 
    quantify 
        ["msg"; "continuation"] 
        (function [msg; continuation] -> 
            mtype_of_ct(TArrow(
                mtype_of_st(STSend(
                    mtype_of_var msg, auto_fplace(STPolyVar continuation) 
                )),
                mtype_of_ct(TArrow(
                    mtype_of_var msg,
                    mtype_of_svar continuation
                ))
            ))
        )
let t_receive () = 
    quantify 
        ["msg"; "continuation"] 
        (function [msg; continuation] -> 
            mtype_of_ct(TArrow(
                mtype_of_st(STSend(
                    mtype_of_var msg, auto_fplace(STPolyVar continuation) 
                )),
                mtype_of_ct(TTuple[
                    mtype_of_var msg;
                    mtype_of_svar continuation
                ])
            ))
        )

let t_initiate () =
    let a = Atom.fresh "A" in
    let b = Atom.fresh "B" in
    
    mtype_of_ct (TArrow(
        fresh_tbridge (),
        mtype_of_ct(TArrow(
            mtype_of_ct(TForall(a, mtype_of_cvar b)), 
            mtype_of_ct(TActivationInfo (
                mtype_of_ct(TForall(b, mtype_of_cvar b))
            ))
        ))
    ))

let t_print () = 
    mtype_of_ct(TArrow(
        mtype_of_ft TStr,
        mtype_of_ft TVoid
    ))

let t_first () = 
    quantify ["a"; "b"] (function [a; b] -> 
        mtype_of_ct(TArrow(
            mtype_of_ct (TTuple [mtype_of_var a; mtype_of_var b]),
            mtype_of_var a 
        ))
    )
let t_second () = 
    quantify ["a"; "b"] (function [a; b] -> 
        mtype_of_ct(TArrow(
            mtype_of_ct (TTuple [mtype_of_var a; mtype_of_var b]),
            mtype_of_var b
        ))
    )

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
    quantify ["a"] (function [a] ->
        mtype_of_ct(TArrow(
            mtype_of_ct (TList (mtype_of_var a)),
            mtype_of_ct(TArrow(
                mtype_of_ft TInt,
                mtype_of_ft TPlace
            ))
        ))
    )

let t_placeof () = 
    mtype_of_ct(TArrow(
        quantify ["A"] (function  [a] -> mtype_of_ct (TActivationInfo (mtype_of_cvar a))),
        mtype_of_ft TPlace
    ))
let t_select () = 
    mtype_of_ct(TArrow(
        quantify ["vp"] (function[vp] -> mtype_of_ct (TVPlace (mtype_of_var vp))),
        mtype_of_ct (TArrow(
            mtype_of_ft TPlace,
            mtype_of_ft TBool
        ))
    ))

let t_activationat () =
    mtype_of_ct(TArrow(
        mtype_of_ft TPlace,
        mtype_of_ct (TSet (
            mtype_of_ct (TActivationInfo(mtype_of_ft TWildcard))
        ))
    ))

let t_sleep () = 
    mtype_of_ct(TArrow(
        mtype_of_ft TInt,
        mtype_of_ft TVoid
    ))
let t_add2dict () = 
    quantify ["k"; "v"] (function [k;v] ->
        mtype_of_ct(TArrow(
            mtype_of_ct (TDict (mtype_of_var k, mtype_of_var v)),
            mtype_of_ct(TArrow(
                mtype_of_var k,
                mtype_of_ct(TArrow(
                    mtype_of_var v,
                    mtype_of_ft TVoid
                ))
            ))
        ))    
    )
let t_get2dict () = 
    quantify ["k"; "v"] (function [k;v] ->
        mtype_of_ct(TArrow(
            mtype_of_ct (TDict (mtype_of_var k, mtype_of_var v)),
            mtype_of_ct(TArrow(
                mtype_of_var k,
                mtype_of_var v
            ))
        ))    
    )
let t_remove2dict () = 
    quantify ["k"; "v"] (function [k;v] ->
        mtype_of_ct(TArrow(
            mtype_of_ct (TDict (mtype_of_var k, mtype_of_var v)),
            mtype_of_ct(TArrow(
                mtype_of_var k,
                mtype_of_ft TVoid
            ))
        ))    
    )
let t_dict () = 
    mtype_of_ct(TArrow(
        mtype_of_ft TVoid,
        quantify ["k"; "v"] (function [k;v] ->
            mtype_of_ct (TDict (mtype_of_var k, mtype_of_var v))
        )
    ))

let t_nth () = (* TODO can not work - number of elmt not knwon*)
    mtype_of_ct(TArrow(
        mtype_of_ct (TTuple [mtype_of_ft TWildcard]),
        mtype_of_ft TWildcard
    ))

let t_sessionid () =
    mtype_of_ct (TArrow(
        quantify ["st"] (function [st] -> mtype_of_svar st),
        mtype_of_ft TInt
    ))
        
(* name, signature string, description, signature () -> .. , neeed a closure to generate fresh types *)
let builtin_fcts : (string * string * string * (unit -> main_type)) list= [
    "activationsat", "place -> set<activation_info>", "", t_activationat;
    "add2dict", "dict<k,v> -> k -> v -> ()", "add in place",t_add2dict ;
    "bridge", "() -> Bridge<'A, 'B, 'a>", "create a new bridge with a fresh id",
    fresh_tbridge;
    "fire", "STSend<'msg, 'continuation> -> 'msg -> 'continuation", "TODO", t_fire
    ;
    "current_place", " unit -> place", "current place", t_current_place;
    "dict", "() -> dict", "create a new dict", t_dict;
    "first", "Tuple<'a, 'b> -> 'a", "Return the first element of list, failed if empty", t_first;
    "get2dict", "dict<k,v> -> k -> v", "get", t_get2dict;
    "initiate_session_with", "TODO", "TODO", 
    t_initiate;
    "listget", " list<T> -> int -> t", "", t_listget;
    "nth", "tuple -> int -> ...", "nème elmt", t_nth;
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
    (*


    "sqrt", "float -> float", "Compute the square root of a float.";
    "none", "unit", "unit term";
    "last", "List<T> -> T", "Return the last element of list, failed if empty";
    "switch", "session -> label* sessionTODO", "TODO";
    "remote_endpoint", "session -> TODO", "";
    "ipaddress", "TODO->TODO", "";
    "pick", "TODO", "Random choice in a sequence, failed if empty";
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

module BuiltinSet = Set.Make(String)                     
let builtin_htbl = Hashtbl.of_seq (List.to_seq (List.map (function (x, a, b, mt) -> (x, (a,b, mt))) builtin_fcts))

let builtin_expr_env = BuiltinSet.of_list (List.map (function x,_,_,_ -> x) builtin_fcts)    
let builtin_type_env = BuiltinSet.of_list  builtin_atomic_types    
let is_builtin_expr x = try let _ = BuiltinSet.find x builtin_expr_env in true with Not_found -> false                   
let is_builtin_type x = try let _ = BuiltinSet.find x builtin_type_env in true with Not_found -> false                   

let type_of x : IR.main_type = 
    let (_, _, make_mt) = Hashtbl.find builtin_htbl x in
    make_mt ()

let desc_of x : string = 
  let (_, desc,_) = Hashtbl.find builtin_htbl x in
  desc 

let is_builtin_component _ = false