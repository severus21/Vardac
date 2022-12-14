open Core
open AstUtils
open IR
open Easy_logging

open BuiltinTypes

let logger = Core.Utils.make_log_of "BuiltinSignature"
let fplace = (Error.forge_place "BuiltinSignature.*" 0 0)
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
    (*let a = Atom.fresh "a" in
    let b = Atom.fresh "b" in
    let p = Atom.fresh "p" in
    *)
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

let t_protocol_of () =

    mtype_of_ct (TArrow(
        mtype_of_ct (TBridge {
            in_type  = mtype_of_ft TWildcard;
            out_type = mtype_of_ft TWildcard;
            protocol = mtype_of_st STEnd
        }),
        mtype_of_ft TWildcard
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
            mtype_of_ft TBottom, auto_fplace STEnd (* see t_recive for TBottom *)
        )),
        mtype_of_ct(TArrow(
            mtype_of_ft TBottom,
            mtype_of_ct (TResult (
                mtype_of_st STWildcard,
                builtin_mt_error
            ))
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
            (* TBottom should be wildcard or better a quantified universal type
            for now it is a TBottom to be translated to java "Object" 
            FIXME TODO when mgu 
            *)
            mtype_of_ft TBottom, auto_fplace STEnd 
        )),
        mtype_of_ct(TTuple
            [
                mtype_of_ft TBottom; (* Same as previous TBottom*)
                mtype_of_st STWildcard
            ]
        )
    ))


let t_session_to_2_ () =
    mtype_of_ct (TArrow(
        mtype_of_st STBottom,
        mtype_of_ct (TActivationRef (mtype_of_ft TWildcard))
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
let t_initiate_inlined () =
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
            mtype_of_ct(TArrow(
                mtype_of_ft TWildcard,
                mtype_of_ft TWildcard
            ))
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
        mtype_of_ft TUnit,
        mtype_of_ft TPlace
    ))

let t_ip () = 
    mtype_of_ct (TArrow(
        mtype_of_ft TPlace,
        mtype_of_ft TStr
    ))

let t_places () =
    mtype_of_ct(TArrow(
        mtype_of_ft TUnit,
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

let t_aget () =
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

let t_aput () =
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
            mtype_of_ct(TArrow(
                mtype_of_ft TWildcard,
                mtype_of_ft TVoid
            ))
        ))
    ))

let t_asize () =
    mtype_of_ct(TArrow(
        mtype_of_ft TWildcard,
        mtype_of_ft TInt
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
        mtype_of_ct(TArrow(
            mtype_of_st STWildcard,
            mtype_of_ct(TArrow(
                mtype_of_ft TBLabel,
                mtype_of_ct ( TResult(
                    mtype_of_st STWildcard,
                    builtin_mt_error
                ))
            ))
        ))

let t_is_init_stage () = 
    mtype_of_ct(TArrow(
        mtype_of_st STWildcard,
        mtype_of_ft TBool
    ))

let t_session_from () =
    mtype_of_ct(TArrow(
        mtype_of_st STWildcard,
        mtype_of_ct (TActivationRef(mtype_of_ft TWildcard))
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
let t_exist2dict () = 
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
            mtype_of_ft TBool 
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
            mtype_of_ft TVoid
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
        mtype_of_ft TUnit,
        mtype_of_ft TWildcard
    ))

let t_string_of_bridge () = 
    mtype_of_ct(TArrow(
        fresh_tbridge (),
        mtype_of_ft TStr 
    ))


let t_sessionid () =
    mtype_of_ct (TArrow(
        mtype_of_st STBottom,
        mtype_of_ft TInt
    ))

let t___get_intermediate_port () =
    mtype_of_ct (TArrow(
        mtype_of_st STBottom,
        mtype_of_ct (TArrow(
            mtype_of_ct (TOption (mtype_of_ft TStr)),
            mtype_of_ft TInt
        ))
    ))

let t_bind () =
    mtype_of_ct (TArrow (
        mtype_of_ct (TUnion (
            mtype_of_ct (TOutport (mtype_of_st STWildcard)),
            mtype_of_ct (TInport (mtype_of_st STWildcard))
        )),
        mtype_of_ct (TArrow (
            fresh_tbridge (),
            mtype_of_ft TVoid
        ))
    ))

let t_bind_inlined () =
    mtype_of_ct (TArrow (
        mtype_of_ct (TUnion (
            mtype_of_ct (TOutport (mtype_of_st STWildcard)),
            mtype_of_ct (TInport (mtype_of_st STWildcard))
        )),
        mtype_of_ct (TArrow (
            fresh_tbridge (),
            mtype_of_ct (TArrow (
                mtype_of_ct (TActivationRef(mtype_of_ft TWildcard)),
                mtype_of_ft TVoid
            ))
        ))
    ))

let t_is_none () =
    mtype_of_ct (TArrow (
        mtype_of_ct (TOption (
            mtype_of_ft TWildcard
        )),
        mtype_of_ft TBool 
    ))
let t_is_ok () =
    mtype_of_ct (TArrow (
        mtype_of_ct (TResult (
            mtype_of_ft TWildcard,
            builtin_mt_error
        )),
        mtype_of_ft TBool 
    ))
let t_get_ok () =
    mtype_of_ct (TArrow (
        mtype_of_ct (TResult (
            mtype_of_ft TWildcard,
            builtin_mt_error
        )),
        mtype_of_ft TWildcard 
    ))

let t_get_err () =
    mtype_of_ct (TArrow (
        mtype_of_ct (TResult (
            mtype_of_ft TWildcard,
            builtin_mt_error
        )),
        builtin_mt_error 
    ))

let t_exit () =
    mtype_of_ct (TArrow (
        mtype_of_ft TUnit,
        mtype_of_ft TVoid 
    ))

let t_debug () =
    mtype_of_ct (TArrow (
        mtype_of_ft TStr,
        mtype_of_ft TVoid 
    ))

let t_bridgeof () =
    mtype_of_ct (TArrow (
        mtype_of_ct (TUnion (
            mtype_of_ct (TOutport (mtype_of_st STWildcard)),
            mtype_of_ct (TInport (mtype_of_st STWildcard))
        )),
        mtype_of_ct (TBridge {
            in_type  = mtype_of_ft TWildcard;
            out_type = mtype_of_ft TWildcard;
            protocol = mtype_of_st STEnd
        })
    ))

let t_setlength () =
    mtype_of_ct (TArrow (
        mtype_of_ct (TSet (mtype_of_ct (TActivationRef(mtype_of_ft TWildcard)))),
        mtype_of_ft TInt
    ))
let t_leftactivations () =
    mtype_of_ct (TArrow (
        mtype_of_ct (TBridge {
            in_type  = mtype_of_ft TWildcard;
            out_type = mtype_of_ft TWildcard;
            protocol = mtype_of_st STEnd
        }),
        mtype_of_ct (TResult (
            mtype_of_ct (TSet (mtype_of_ct (TActivationRef(mtype_of_ft TBottom)))),
            builtin_mt_error
        ))
    ))
let t_rightactivations () = t_leftactivations ()

let t_leftregister () =
    mtype_of_ct (TArrow (
        mtype_of_ct (TBridge {
            in_type  = mtype_of_ft TWildcard;
            out_type = mtype_of_ft TWildcard;
            protocol = mtype_of_st STEnd
        }),
        mtype_of_ct (TArrow (
            mtype_of_ct (TActivationRef(mtype_of_ft TBottom)),
            mtype_of_ct (TResult (
                mtype_of_ft TBool,
                builtin_mt_error
            ))
        ))
    ))

let t_range () = 
    mtype_of_ct (TArrow (
        mtype_of_ft TInt,
        mtype_of_ct (TArrow (
            mtype_of_ft TInt,
            mtype_of_ft TRange
        ))
    ))
let t_time () = 
    mtype_of_ct (TArrow (
        mtype_of_ft TUnit,
        mtype_of_ft TInt
    ))
let t_nano_time () = 
    mtype_of_ct (TArrow (
        mtype_of_ft TUnit,
        mtype_of_ft TLong
    ))

let t_long_of_int () = 
    mtype_of_ct (TArrow (
        mtype_of_ft TInt,
        mtype_of_ft TLong
    ))

let t_int_of_string () =
    mtype_of_ct (TArrow (
        mtype_of_ft TStr,
        mtype_of_ft TInt
    ))

let t_list2array () =
    mtype_of_ct (TArrow (
        mtype_of_ct (TList (mtype_of_ft TWildcard)),
        mtype_of_ct (TArray (mtype_of_ft TWildcard))
    ))

let t_list_append () =
    mtype_of_ct (TArrow (
        mtype_of_ct (TList (mtype_of_ft TWildcard)),
        mtype_of_ct (TArrow (
            mtype_of_ft TWildcard,
            mtype_of_ft TVoid
        ))
    ))

let t_forge_activation_ref () =
    mtype_of_ct (TArrow (
        mtype_of_ft TUnit,
        mtype_of_ct (TActivationRef (mtype_of_ft TWildcard))
    ))
let t_current_activation () =
    mtype_of_ct (TArrow (
        mtype_of_ft TUnit,
        mtype_of_ct (TActivationRef (mtype_of_ft TWildcard))
    ))

let t_activationid () =
    mtype_of_ct (TArrow (
        mtype_of_ct (TActivationRef (mtype_of_ft TWildcard)),
        mtype_of_ft TActivationID
    ))
let t_schema_of () =
    mtype_of_ct (TArrow (
        mtype_of_ct (TActivationRef (mtype_of_ft TWildcard)),
        mtype_of_ft TStr 
    ))

let t_option_get () =
    mtype_of_ct (TArrow (
        mtype_of_ct (TOption (mtype_of_ft TWildcard)),
        mtype_of_ft TWildcard
    ))

let t_one_hop_activation_ref () =
    mtype_of_ct (TArrow (
        mtype_of_ct (TActivationRef (mtype_of_ft TWildcard)),
        mtype_of_ct (TArrow (
            mtype_of_ct (TActivationRef (mtype_of_ft TWildcard)),
            mtype_of_ct (TActivationRef (mtype_of_ft TWildcard))
        ))
    ))

let t_register_activation_at () = 
    mtype_of_ct( TArrow(
        mtype_of_ct (TActivationRef (mtype_of_ft TWildcard)),
        mtype_of_ct (TArrow (
            mtype_of_ft TPlace,
            mtype_of_ft TVoid
        ))
    ))

let t_pick () =
    mtype_of_ct(TArrow(
        mtype_of_ct(TDict (mtype_of_ft TWildcard, mtype_of_ft TWildcard)),
        mtype_of_ft TWildcard
    ))

let t_future () =
    mtype_of_ct(TArrow(
        mtype_of_ft TUnit,
        mtype_of_ct(TFuture(mtype_of_ft TWildcard))
    ))
let t_complete_future () =
    mtype_of_ct(TArrow(
        mtype_of_ct(TFuture(mtype_of_ft TWildcard)),
        mtype_of_ct(TArrow(
            mtype_of_ft TWildcard,
            mtype_of_ft TVoid
        ))
    ))

let t_wait_future () =
    mtype_of_ct(TArrow(
        mtype_of_ct(TFuture(mtype_of_ft TWildcard)),
        mtype_of_ct(TArrow(
            mtype_of_ft TInt,
            mtype_of_ct (TResult (
                mtype_of_st STWildcard,
                builtin_mt_error
            ))
        ))
    ))