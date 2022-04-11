open Core
open Utils
open AstUtils
open Easy_logging
open Fieldslib
open Misc

let plg_name = "Akka"
let logger = Logging.make_logger ("_1_ compspec.plg."^plg_name) Debug [];;

(* The source calculus. *)
module S = IR 
(* The target calculus. *)
module T = Ast 

(*
TODO refactor en
encode_no_arg, one_arg 
encode try encode according to arg number in order to mutualised the error messages
*)

(*
    @param mt_name - type of the right-hand side variable
*)
let encode_builtin_access place e name =
    assert(Core.Builtin.is_builtin_expr name);
    let auto_place t = {place; value=t} in 

    match name with
    | _ when Builtin.is_tuple_attr name -> begin
        let i = Builtin.pos_of_tuple_attr name in

        (* Vavr state at _1 and not _0 *)
        let i = i+1 in

        T.AccessExpr (
            e, 
            auto_place (
                T.VarExpr (Atom.builtin (Printf.sprintf "_%d" i)), 
                auto_place T.TUnknown
            )
        )
    end
    | _ when Builtin.is_inductive_attr name -> begin
        let i = Builtin.pos_of_inductive_attr name in

        T.CallExpr (
            auto_place (T.AccessExpr (
                e, 
                auto_place (
                    T.VarExpr (Atom.builtin (Printf.sprintf "_%d_" i)), 
                    auto_place T.TUnknown
                )
            ), auto_place T.TUnknown),
            []
        )
    end
    | _ -> failwith (Printf.sprintf "Unsupported builtin access in Akka:  %s" name) 

let encode_builtin_fct place name (args:T.expr list) =
    assert(Core.Builtin.is_builtin_expr name);
    let auto_place t = {place; value=t} in 
    match name with
    (* TODO Remove string and used typed constructor in order to ensure that this file is uptodate with the Core.Builtin.builtin_fcts*)
    | "activationid" -> begin
        match args with
        | [a] -> T.CallExpr(
                auto_place (T.AccessExpr(
                    a,
                    auto_place(T.VarExpr (Atom.builtin "activationId"), auto_place T.TUnknown)
            ), auto_place T.TUnknown),
            []
        )
    end
    | "add2dict" -> begin 
        (* empty dict *)
        match args with
        | [dict; k; v] -> T.CallExpr( 
            auto_place (T.AccessExpr (
                dict, 
                auto_place(T.VarExpr (Atom.builtin "put"), auto_place T.TUnknown)
            ), auto_place T.TUnknown),
            [ k; v ]
        ) 
        | _ -> Error.error place "add2dict takes three arguments"
    end
    | "remove2dict" -> begin 
        (* empty dict *)
        match args with
        | [dict; k] -> T.CallExpr( 
            auto_place(T.AccessExpr (
                dict, 
                auto_place( T.VarExpr (Atom.builtin "remove"), auto_place T.TUnknown)
            ), auto_place T.TUnknown),
            [ k ]
        ) 
        | _ -> Error.error place "remove2dict takes two arguments"
    end
    | "get2dict" -> begin 
        (* empty dict *)
        match args with
        | [dict; k] -> T.CallExpr( 
            auto_place (T.AccessExpr (
                dict, 
                auto_place (T.VarExpr (Atom.builtin "get"), auto_place T.TUnknown)
            ), auto_place T.TUnknown),
            [ k ]
        ) 
        | _ -> Error.error place "get2dict takes two arguments"
    end
    | "ip" -> begin
        match args with
        | [place] -> T.CallExpr( 
            auto_place (T.AccessExpr (
                place, 
                auto_place (T.VarExpr (Atom.builtin "getHost"), auto_place T.TUnknown)
            ), auto_place T.TUnknown),
            []
        ) 
        | _ -> Error.error place "ip takes one argument"
    end
    | "port" -> begin
        match args with
        | [place] -> T.CallExpr( 
            auto_place (T.AccessExpr (
                place, 
                auto_place (T.VarExpr (Atom.builtin "getPort"), auto_place T.TUnknown)
            ), auto_place T.TUnknown),
            []
        ) 
        | _ -> Error.error place "port takes one argument"
    end
    | "dict" -> begin 
        (* empty dict *)
        match args with
        | [] -> T.NewExpr( 
            auto_place (T.VarExpr (Atom.builtin "HashMap"), auto_place T.TUnknown
            ), 
            []
        ) 
        | _ -> Error.error place "dict takes no arguments"
        end
    | "fire" -> begin 
        match args with
        | [ session; msg ] -> T.CallExpr( 
            auto_place (T.AccessExpr (
                session, 
                auto_place (T.VarExpr (Atom.builtin "fire"), auto_place T.TUnknown)
            ), auto_place T.TUnknown),
            [ 
                msg; 
                e_get_context place;
                e_this_timers place;
                e_this_frozen_sessions place;
                e_this_dead_sessions place;
            ]
        ) 
        | _ -> Error.error place "fire must take two arguments : place(session, message)"
        end
    | "select" -> begin 
        match args with
        | [ session; label ] -> T.CallExpr( 
            auto_place (T.AccessExpr (
                session, 
                auto_place (T.VarExpr (Atom.builtin "select"), auto_place T.TUnknown)
            ), auto_place T.TUnknown),
            [ 
                label; 
                e_get_context place;
                e_this_timers place;
                e_this_frozen_sessions place;
                e_this_dead_sessions place;
            ]
        ) 
        | _ -> Error.error place "select must take two arguments : place(session, message)"
        end
    | "first" -> begin
        match args with
        | [ tuple ] ->  T.AccessExpr (
            tuple, 
            auto_place (T.VarExpr (Atom.builtin "_1"), auto_place T.TUnknown)
            )
        | _ -> Error.error place "first must take one argument"
    end
    | "second" -> begin
        match args with
        | [ tuple ] ->  T.AccessExpr (
            tuple, 
            auto_place (T.VarExpr (Atom.builtin "_2"), auto_place T.TUnknown)
            )
        | _ -> Error.error place "second must take one argument"
    end
    | "listget" -> begin
        (* Vavr state at _1 and not _0 *)
        match args with
        | [ l; n]-> T.CallExpr(
            auto_place(T.AccessExpr (
                l, 
                auto_place (T.VarExpr (Atom.builtin "get"), auto_place T.TUnknown)
            ), auto_place T.TUnknown),
            [n]
        )
        | _ -> Error.error place "listget must take two argument"
    end
    | "sessionid" -> begin
        match args with
        | [ s ] ->  T.CallExpr(
            auto_place (T.AccessExpr (
                s, 
                auto_place (T.VarExpr (Atom.builtin "get_id"), auto_place T.TUnknown)
            ), auto_place T.TUnknown),
            []
        )
        | _ -> Error.error place "sessionid must take one argument"
    end
    |"string_of_bridge" -> begin
        match args with
        | [ b ] ->  T.CallExpr(
            auto_place (T.AccessExpr (b, auto_place (T.VarExpr (Atom.builtin "toString"), auto_place T.TUnknown)), auto_place T.TUnknown),
            []
        )
        | _ -> Error.error place "string_of_bridge must take one argument"
    end
    | "activationsat" -> begin
        (* TODO rename activationsat or componentsat in order to have same name in java and ocaml code*)
        match args with
        | [at] -> T.CallExpr(
            e_lg4dc_componentsat place,
            [
                e_get_context place;
                e_this_guardian place;
                at
            ]
        )
        | _ -> Error.error place "activationsat must take one argument" 
    end
    | "placeof" -> begin
        match args with
        | [activation] -> T.CallExpr(
            e_lg4dc_placeof place,
            [ e_get_context place; activation ]
        )
        | _ -> Error.error place "placeof must take one argument" 
    end
    | "places" -> begin
        match args with
        | [] -> fst (e_lg4dc_places place).value
        | _ -> Error.error place "places must take no argument" 
    end
    | "current_place" -> begin
        match args with
        | [] -> fst (e_lg4dc_current_place place).value
        | _ -> Error.error place "current_place must take no argument" 
    end
    | "select_places" -> begin
        match args with
        | [vp; predicate] -> fst (e_lg4dc_select_places place vp predicate).value
        | _ -> Error.error place "select_place must take two arguments" 
    end
    | "print" -> begin
        match args with
        | [ msg ] -> T.CallExpr (
                auto_place (T.VarExpr (Atom.builtin "System.out.println"), auto_place T.TUnknown),
                [msg]
            )
        | _ -> Error.error place "print must take one argument" 
    end
    | "place_to_string" -> begin
        match args with
        | [p] -> T.CallExpr(
            auto_place(T.AccessExpr (
                p,
                auto_place(T.RawExpr "toString", auto_place T.TUnknown)
            ), auto_place T.TUnknown),
            []
        )
        | _ -> Error.error place "places must take one argument"
    end
    | "initiate_session_with" -> begin
        (* TODO i need to get the name of the type of the protocol 
        Maybe some thing like protocol is a value and we bind a protocol type with it.
        *)
        match args with
        | [ outport; right ] ->  
            T.CallExpr (
                auto_place (T.AccessExpr (outport, auto_place (T.VarExpr (Atom.builtin "initiate_session_with"), auto_place T.TUnknown)), auto_place T.TUnknown),
                [
                    auto_place(T.CastExpr(
                        auto_place (T.TVar (Atom.builtin "ActivationRef")), (* TODO can we move cast elsewhere ?*)
                        Misc.e_get_self_activation place (Misc.e_get_context place)
                    ), auto_place T.TUnknown);
                    right;
                    Misc.e_none place ; (* FIXME interception should change this*)
                ]
            )
        | _ -> Error.error place "first must take one argument"
    end
    | "pick" -> begin
        match args with 
        | [ dict ] ->
            T.CallExpr(
                auto_place (T.VarExpr (Atom.builtin "com.lg4dc.Utils.pick"), auto_place T.TUnknown),
                [ dict ]
            )
        | _ -> Error.error place "pick must take one argument"
    end
    | "option_get" -> begin
        match args with 
        | [ opt ] ->
            T.CallExpr(
                auto_place (T.AccessExpr( 
                    opt,
                    auto_place (T.VarExpr (Atom.builtin "get"), auto_place T.TUnknown)
                ), auto_place T.TUnknown),
                [ ]
            )
        | _ -> Error.error place "option_get must take one argument"
    end
    | "sleep" -> Error.error place "In Akka, sleep must be convertible to a statement"
    | _ -> 
        Error.error place "Akka.Finish do not yet support builtin function %s" name

let is_stmt_builtin = function
| "sleep" -> true
| _ -> false

let encode_builtin_fct_as_stmt place name (args:T.expr list) =
    assert(Core.Builtin.is_builtin_expr name);
    let auto_place t = {place; value=t} in 
    match name with
    | "sleep" -> begin
        (* FIXME/TODO create a protocol to pause an actor and resume it after some time *)
        logger#warning "using sleep in Akka will block a thread - not only the actor";
        match args with
        | [ duration ] -> 
            let e = Atom.fresh "e" in
            T.TryStmt(
                auto_place(T.ExpressionStmt(
                    auto_place(T.CallExpr (
                        auto_place (T.RawExpr "Thread.sleep", auto_place T.TUnknown), 
                        [duration]
                    ), auto_place T.TUnknown)
                )),
                [
                    (
                        auto_place(T.Atomic "Exception"), 
                        e, 
                        auto_place(T.ExpressionStmt(auto_place(T.RawExpr "System.out.println(e)", auto_place T.TUnknown)))
                    );
                ]
            )
        | _ -> Error.error place "print must take one argument" 
    end
let encode_list place es = 
    let auto_place smth = {place; value=smth} in
    T.CallExpr(
        auto_place(T.VarExpr(Atom.builtin "List.of"), auto_place T.TUnknown),
        es
    )
let encode_tuple place es = 
    let auto_place smth = {place; value=smth} in
    T.CallExpr(
        auto_place(T.VarExpr(Atom.builtin "Tuple.of"), auto_place T.TUnknown),
        es
    )