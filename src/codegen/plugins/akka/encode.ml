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

let encode_builtin_fct place name (args:T.expr list) =
    assert(Core.Builtin.is_builtin_expr name);
    let auto_place t = {place; value=t} in 
    match name with
    (* TODO Remove string and used typed constructor in order to ensure that this file is uptodate with the Core.Builtin.builtin_fcts*)
    | "add2dict" -> begin 
        (* empty dict *)
        match args with
        | [dict; k; v] -> T.CallExpr( 
            {
                place;
                value = T.AccessExpr (dict, {place; value = T.VarExpr (Atom.fresh_builtin "put")})
            },
            [ k; v ]
        ) 
        | _ -> Error.error place "add2dict takes three arguments"
    end
    | "remove2dict" -> begin 
        (* empty dict *)
        match args with
        | [dict; k] -> T.CallExpr( 
            {
                place;
                value = T.AccessExpr (dict, {place; value = T.VarExpr (Atom.fresh_builtin "remove")})
            },
            [ k ]
        ) 
        | _ -> Error.error place "remove2dict takes two arguments"
    end
    | "get2dict" -> begin 
        (* empty dict *)
        match args with
        | [dict; k] -> T.CallExpr( 
            {
                place;
                value = T.AccessExpr (dict, {place; value = T.VarExpr (Atom.fresh_builtin "get")})
            },
            [ k ]
        ) 
        | _ -> Error.error place "get2dict takes two arguments"
    end
    | "dict" -> begin 
        (* empty dict *)
        match args with
        | [] -> T.NewExpr( 
            {
                place;
                value = T.VarExpr (Atom.fresh_builtin "HashMap")
            },
            []
        ) 
        | _ -> Error.error place "dict takes no arguments"
        end
    | "fire" -> begin 
        match args with
        | [ session; msg ] -> T.CallExpr( 
            {
                place;
                value = T.AccessExpr (session, {place; value = T.VarExpr (Atom.fresh_builtin "fire")})
            },
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
    | "first" -> begin
        match args with
        | [ tuple ] ->  T.AccessExpr (tuple, {place; value = T.VarExpr (Atom.fresh_builtin "_1")})
        | _ -> Error.error place "first must take one argument"
    end
    | "second" -> begin
        match args with
        | [ tuple ] ->  T.AccessExpr (tuple, {place; value = T.VarExpr (Atom.fresh_builtin "_2")})
        | _ -> Error.error place "second must take one argument"
    end
    | "nth" -> begin
        (* Vavr state at _1 and not _0 *)
        match args with
        | [ {value=LitExpr{value=IntLit i}}; tuple ]->  T.AccessExpr (tuple, {place; value = T.VarExpr (Atom.fresh_builtin (Printf.sprintf "_%d"(i+1)))})
        | _ -> Error.error place "nth must take two argument"
    end
    | "listget" -> begin
        (* Vavr state at _1 and not _0 *)
        match args with
        | [ l; n]-> T.CallExpr(
            auto_place(T.AccessExpr (l, {place; value = T.VarExpr (Atom.fresh_builtin "get")})),
            [n]
        )
        | _ -> Error.error place "nth must take two argument"
    end
    | "sessionid" -> begin
        match args with
        | [ s ] ->  T.CallExpr(
            { place; value = T.AccessExpr (s, {place; value = T.VarExpr (Atom.fresh_builtin "get_id")})},
            []
        )
        | _ -> Error.error place "sessionid must take one argument"
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
    | "places" -> begin
        match args with
        | [] -> (e_lg4dc_places place).value
        | _ -> Error.error place "places must take no argument" 
    end
    | "current_place" -> begin
        match args with
        | [] -> (e_lg4dc_current_place place).value
        | _ -> Error.error place "current_place must take no argument" 
    end
    | "select_places" -> begin
        match args with
        | [vp; predicate] -> (e_lg4dc_select_places place vp predicate).value
        | _ -> Error.error place "select_place must take two arguments" 
    end
    | "print" -> begin
        match args with
        | [ msg ] -> T.CallExpr (({place; value=T.VarExpr (Atom.fresh_builtin "System.out.println")}), [msg])
        | _ -> Error.error place "print must take one argument" 
    end
    | "place_to_string" -> begin
        match args with
        | [p] -> T.CallExpr(
            auto_place(T.AccessExpr (
                p,
                auto_place(T.RawExpr "toString")
            )),
            []
        )
        | _ -> Error.error place "places must take one argument"
    end
    | "initiate_session_with" -> begin
        (* TODO i need to get the name of the type of the protocol 
        Maybe some thing like protocol is a value and we bind a protocol type with it.
        *)
        match args with
        | [ bridge; right ] ->  
            T.CallExpr (
                auto_place (T.AccessExpr (bridge, auto_place (T.VarExpr (Atom.fresh_builtin "initiate_session_with")))),
                [
                    auto_place(T.CastExpr(
                        auto_place (T.TVar (Atom.fresh_builtin "ActorRef")),
                        Misc.e_get_self place (Misc.e_get_context place)
                    ));
                    right;
                ]
            )
        | _ -> Error.error place "first must take one argument"
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
            let e = Atom.fresh_builtin "e" in
            T.TryStmt(
                auto_place(T.ExpressionStmt(auto_place(T.CallExpr (({place; value=T.RawExpr "Thread.sleep"}), [duration])))),
                [
                    (auto_place(T.Atomic "Exception"), e, auto_place(T.ExpressionStmt(auto_place(T.RawExpr "System.out.println(e)"))));
                ]
            )
        | _ -> Error.error place "print must take one argument" 
    end
let encode_list place es = 
    let auto_place smth = {place; value=smth} in
    T.CallExpr(
        auto_place(T.VarExpr(Atom.fresh_builtin "List.of")),
        es
    )
let encode_tuple place es = 
    let auto_place smth = {place; value=smth} in
    T.CallExpr(
        auto_place(T.VarExpr(Atom.fresh_builtin "Tuple.of")),
        es
    )