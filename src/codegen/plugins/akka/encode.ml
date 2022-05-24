open Core
open Utils
open AstUtils
open Easy_logging
open Fieldslib
open Misc

let plg_name = "Akka"
let logger = Logging.make_logger ("_1_ compspec.plg."^plg_name) Debug [];;
let fplace = (Error.forge_place ("plg."^plg_name^".Encode") 0 0) 
include Ast.AstUtil2.Make(struct let fplace = fplace end)

(* The source calculus. *)
module S = IR 
(* The target calculus. *)
module T = Ast 

(*
TODO refactor en
encode_no_arg, one_arg 
encode try encode according to arg number in order to mutualised the error messages
*)


let encode_builtin_type place name = 
    assert(Builtin.is_builtin_type name);
    let auto_place t = {place; value=t} in 
    
    match name with
    | "error" -> T.TVar (Atom.builtin "com.lg4dc.Error")
    | _ -> failwith (Printf.sprintf "Unsupported builtin access in Akka:  %s" name) 


(*
    @param mt_name - type of the right-hand side variable
*)
let rec _encode_builtin_access place e name =
    assert(Builtin.is_builtin_expr name);
    let auto_place t = {place; value=t} in 

    match name with
    | _ when Builtin.is_tuple_attr name -> begin
        let i = Builtin.pos_of_tuple_attr name in

        (* Vavr state at _1 and not _0 *)
        let i = i+1 in

        T.AccessExpr (
            e, 
            e2var (Atom.builtin (Printf.sprintf "_%d" i))
        )
    end
    | _ when Builtin.is_inductive_attr name -> begin
        let i = Builtin.pos_of_inductive_attr name in

        T.CallExpr (
            e2_e (T.AccessExpr (
                e, 
                e2var (Atom.builtin (Printf.sprintf "_%d_" i))
            )),
            []
        )
    end
    | "__get_intermediate_port" ->
        T.AccessExpr (
            e, 
            e2var (Atom.builtin "get_intermediate_port")
        )
    | _ -> failwith (Printf.sprintf "Unsupported builtin access in Akka:  %s" name) 
and encode_builtin_access place e name = 
    let auto_place t = {place; value=t} in 

    if PlgBuiltin.is_builtin_expr name then 
        T.AccessExpr(e, e2var (Atom.builtin name))
    else _encode_builtin_access place e name

let encode_builtin_fct place name (args:T.expr list) =
    assert(Builtin.is_builtin_expr name);
    let auto_place t = {place; value=t} in 
    match name with
    (* TODO Remove string and used typed constructor in order to ensure that this file is uptodate with the Builtin.builtin_fcts*)
    | "activationid" -> begin
        match args with
        | [a] -> T.CallExpr(
                e2_e (T.AccessExpr(
                    a,
                    e2var (Atom.builtin "activationId")
            )),
            []
        )
    end
    | "bind_in" -> begin
        match args with
        | [port; bridge] -> 
            T.CallExpr(
                e2_e (T.AccessExpr(
                    e2_e T.This,
                    e2var (Atom.builtin "bind_in")
                )),
                [ 
                    port; 
                    bridge 
                ]
            )
    end
    | "bind_out" -> begin
        match args with
        | [port; bridge] -> 
            T.CallExpr(
                e2_e (T.AccessExpr(
                    e2_e T.This,
                    e2var (Atom.builtin "bind_out")
                )),
                [ 
                    port; 
                    bridge 
                ]
            )
    end
    | "is_ok" -> begin
        match args with
        | [e] -> 
            T.CallExpr(
                e2_e (T.AccessExpr(
                    e,
                    e2var (Atom.builtin "isRight")
                )),
                []
            )
    end
    | "is_err" -> begin
        match args with
        | [e] -> 
            T.CallExpr(
                e2_e (T.AccessExpr(
                    e,
                    e2var (Atom.builtin "isLeft")
                )),
                []
            )
    end
    | "add2dict" -> begin 
        (* empty dict *)
        match args with
        | [dict; k; v] -> T.CallExpr( 
            e2_e (T.AccessExpr (
                dict, 
                e2var (Atom.builtin "put")
            )),
            [ k; v ]
        ) 
        | _ -> Error.perror place "add2dict takes three arguments"
    end
    | "remove2dict" -> begin 
        (* empty dict *)
        match args with
        | [dict; k] -> T.CallExpr( 
            e2_e (T.AccessExpr (
                dict, 
                e2var (Atom.builtin "remove")
            )),
            [ k ]
        ) 
        | _ -> Error.perror place "remove2dict takes two arguments"
    end
    | "get2dict" -> begin 
        (* empty dict *)
        match args with
        | [dict; k] -> T.CallExpr( 
            e2_e (T.AccessExpr (
                dict, 
                e2var (Atom.builtin "get")
            )),
            [ k ]
        ) 
        | _ -> Error.perror place "get2dict takes two arguments"
    end
    | "ip" -> begin
        match args with
        | [place] -> T.CallExpr( 
            e2_e (T.AccessExpr (
                place, 
                e2var (Atom.builtin "getHost")
            )),
            []
        ) 
        | _ -> Error.perror place "ip takes one argument"
    end
    | "port" -> begin
        match args with
        | [place] -> T.CallExpr( 
            e2_e (T.AccessExpr (
                place, 
                e2var (Atom.builtin "getPort")
            )),
            []
        ) 
        | _ -> Error.perror place "port takes one argument"
    end
    | "dict" -> begin 
        (* empty dict *)
        match args with
        | [] -> T.NewExpr( 
            e2var (Atom.builtin "HashMap"), 
            []
        ) 
        | _ -> Error.perror place "dict takes no arguments"
        end
    | "fire" -> begin 
        match args with
        | [ session; msg ] -> T.CallExpr( 
            e2_e (T.AccessExpr (
                session, 
                e2var (Atom.builtin "fire")
            )),
            [ 
                msg; 
                e_get_context place;
                e_this_timers place;
                e_this_frozen_sessions place;
                e_this_dead_sessions place;
            ]
        ) 
        | _ -> Error.perror place "fire must take two arguments : place(session, message)"
        end
    | "select" -> begin 
        match args with
        | [ session; label ] -> T.CallExpr( 
            e2_e (T.AccessExpr (
                session, 
                e2var (Atom.builtin "select")
            )),
            [ 
                label; 
                e_get_context place;
                e_this_timers place;
                e_this_frozen_sessions place;
                e_this_dead_sessions place;
            ]
        ) 
        | _ -> Error.perror place "select must take two arguments : place(session, message)"
        end
    | "first" -> begin
        match args with
        | [ tuple ] ->  T.AccessExpr (
            tuple, 
            e2var (Atom.builtin "_1")
            )
        | _ -> Error.perror place "first must take one argument"
    end
    | "second" -> begin
        match args with
        | [ tuple ] ->  T.AccessExpr (
            tuple, 
            e2var (Atom.builtin "_2")
            )
        | _ -> Error.perror place "second must take one argument"
    end
    | "listget" -> begin
        (* Vavr state at _1 and not _0 *)
        match args with
        | [ l; n]-> T.CallExpr(
            e2_e (T.AccessExpr (
                l, 
                e2var (Atom.builtin "get")
            )),
            [n]
        )
        | _ -> Error.perror place "listget must take two argument"
    end
    | "sessionid" -> begin
        match args with
        | [ s ] ->  T.CallExpr(
            e2_e (T.AccessExpr (
                s, 
                e2var (Atom.builtin "get_id")
            )),
            []
        )
        | _ -> Error.perror place "sessionid must take one argument"
    end
    |"string_of_bridge" -> begin
        match args with
        | [ b ] ->  T.CallExpr(
            e2_e (T.AccessExpr (b, e2var (Atom.builtin "toString"))),
            []
        )
        | _ -> Error.perror place "string_of_bridge must take one argument"
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
        | _ -> Error.perror place "activationsat must take one argument" 
    end
    | "placeof" -> begin
        match args with
        | [activation] -> T.CallExpr(
            e_lg4dc_placeof place,
            [ e_get_context place; activation ]
        )
        | _ -> Error.perror place "placeof must take one argument" 
    end
    | "places" -> begin
        match args with
        | [] -> fst (e_lg4dc_places place).value
        | _ -> Error.perror place "places must take no argument" 
    end
    | "current_place" -> begin
        match args with
        | [] -> fst (e_lg4dc_current_place place).value
        | _ -> Error.perror place "current_place must take no argument" 
    end
    | "select_places" -> begin
        match args with
        | [vp; predicate] -> fst (e_lg4dc_select_places place vp predicate).value
        | _ -> Error.perror place "select_place must take two arguments" 
    end
    | "print" -> begin
        match args with
        | [ msg ] -> T.CallExpr (
                e2var (Atom.builtin "System.out.println"),
                [msg]
            )
        | _ -> Error.perror place "print must take one argument" 
    end
    | "place_to_string" | "int_to_string" -> begin
        match args with
        | [p] -> T.CallExpr(
            e2_e (T.AccessExpr (
                p,
                e2_e (T.RawExpr "toString")
            )),
            []
        )
        | _ -> Error.perror place "X_to_string takes one arg"
    end
    | "initiate_session_with" -> begin
        (* TODO i need to get the name of the type of the protocol 
        Maybe some thing like protocol is a value and we bind a protocol type with it.
        *)
        match args with
        | [ outport; right ] ->  
            T.CallExpr (
                e2_e (T.AccessExpr (outport, e2var (Atom.builtin "initiate_session_with"))),
                [
                    e2_e (T.CastExpr(
                        auto_place (T.TVar (Atom.builtin "ActivationRef")), (* TODO can we move cast elsewhere ?*)
                        Misc.e_get_self_activation place (Misc.e_get_context place)
                    ));
                    right;
                    Misc.e_none place ; (* FIXME interception should change this*)
                ]
            )
        | _ -> Error.perror place "first must take one argument"
    end
    | "pick" -> begin
        match args with 
        | [ dict ] ->
            T.CallExpr(
                e2var (Atom.builtin "com.lg4dc.Utils.pick"),
                [ dict ]
            )
        | _ -> Error.perror place "pick must take one argument"
    end
    | "debug" -> begin
        match args with 
        | [ str ] ->
            T.CallExpr(
                e2var (Atom.builtin "getContext().getLog().debug"),
                [ str ]
            )
        | _ -> Error.perror place "debug must take one argument"
    end
    | "option_get" -> begin
        match args with 
        | [ opt ] ->
            T.CallExpr(
                e2_e (T.AccessExpr( 
                    opt,
                    e2var (Atom.builtin "get")
                )),
                [ ]
            )
        | _ -> Error.perror place "option_get must take one argument"
    end
    | "sleep" -> Error.perror place "In Akka, sleep must be convertible to a statement"
    | _ -> 
        Error.perror place "Akka.Finish do not yet support builtin function %s" name

let is_stmt_builtin = function
| "sleep" -> true
| "exit" -> true
| _ -> false

let encode_builtin_fct_as_stmt place name (args:T.expr list) =
    assert(Builtin.is_builtin_expr name);
    let auto_place t = {place; value=t} in 
    match name with
    | "exit" -> begin
        match args with
        | [ _ ] -> T.RawStmt {|
        //try {
            ActorSystem system = getContext().getSystem();
        //    CompletableFuture future = system.getWhenTerminated().toCompletableFuture();
            system.terminate();
        //    future.get(30, java.util.concurrent.TimeUnit.SECONDS);
        //} catch (InterruptedException | ExecutionException | java.util.concurrent.TimeoutException e) {
        //    System.out.println(e);
        //}
    |}
    end
    | "sleep" -> begin
        (* FIXME/TODO create a protocol to pause an actor and resume it after some time *)
        logger#warning "using sleep in Akka will block a thread - not only the actor";
        match args with
        | [ duration ] -> 
            let e = Atom.fresh "e" in
            T.TryStmt(
                auto_place(T.ExpressionStmt(
                    e2_e (T.CallExpr (
                        e2_e (T.RawExpr "Thread.sleep"), 
                        [duration]
                    ))
                )),
                [
                    (
                        auto_place(T.Atomic "Exception"), 
                        e, 
                        auto_place(T.ExpressionStmt(e2_e (
                            T.CallExpr(
                                e2_e (T.RawExpr "System.out.println"),
                                [e2var e]
                            ))))
                    );
                ]
            )
        | _ -> Error.perror place "print must take one argument" 
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