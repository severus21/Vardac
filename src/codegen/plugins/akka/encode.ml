open Core
open Utils
open AstUtils
open Easy_logging
open Fieldslib
open Misc

let plg_name = "Akka"
let logger = make_log_of "Akka" 
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
    | "error" -> T.TVar (Atom.builtin "com.varda.Error")
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

let encode_builtin_fct_0 parent_opt place name =
    let auto_place t = {place; value=t} in 
    match name with
    | "dict" -> 
        T.NewExpr( 
            e2var (Atom.builtin "HashMap"), 
            []
        ) 
    | "places" -> 
        fst (e_varda_places place).value
    | "current_place" ->
         fst (e_varda_current_place place).value
    | "time" ->
        T.RawExpr "System.currentTimeMillis()"
    | "nano_time" ->
        T.RawExpr "System.nanoTime()"
    | "current_activation" ->
        fst (Misc.e_get_self_activation place (Misc.e_get_context place)).value
    | "forge_activation_ref" ->
        T.RawExpr "new ActivationRef()"
    | "future" -> 
        T.RawExpr "new CompletableFuture()"
    | _ -> Error.perror place "%s takes zero argument" name

let encode_builtin_fct_1 parent_opt place name a =
    let auto_place t = {place; value=t} in 
    match name with
    | "activationid" ->
        T.CallExpr(
                e2_e (T.AccessExpr(
                    a,
                    e2var (Atom.builtin "activationId")
            )),
            []
        )
    | "schemaof" ->
        T.CallExpr(
                e2_e (T.AccessExpr(
                    a,
                    e2var (Atom.builtin "getComponentSchema")
            )),
            []
        )
    | "asize" ->
        T.CallExpr(
            e2_e(T.AccessExpr(
                a,
                e2_e (T.RawExpr "size")
            )),
            []
        )
    | "is_none" ->
        T.CallExpr(
            e2_e (T.AccessExpr(
                a,
                e2var (Atom.builtin "isEmpty")
            )),
            []
        )
    | "is_ok" ->
        T.CallExpr(
            e2_e (T.AccessExpr(
                a,
                e2var (Atom.builtin "isRight")
            )),
            []
        )
    | "is_err" ->
        T.CallExpr(
            e2_e (T.AccessExpr(
                a,
                e2var (Atom.builtin "isLeft")
            )),
            []
        )
    | "ip" ->
        T.CallExpr( 
            e2_e (T.AccessExpr (
                a, 
                e2var (Atom.builtin "getHost")
            )),
            []
        ) 
    | "port" -> 
        T.CallExpr( 
            e2_e (T.AccessExpr (
                a, 
                e2var (Atom.builtin "getPort")
            )),
            []
        ) 
    | "print" -> 
        T.CallExpr (
            e2var (Atom.builtin "System.out.println"),
            [ a ]
        )
    | "place_to_string" | "int_to_string" | "long_to_string" -> 
        T.CallExpr(
            e2_e (T.AccessExpr (
                a,
                e2_e (T.RawExpr "toString")
            )),
            []
        )
    | "int_of_string" ->
        T.CallExpr(
            e2_e (T.RawExpr "Integer.parseInt"),
            [ a ]
        )
    | "first" -> 
        T.AccessExpr (
            a, 
            e2var (Atom.builtin "_1")
        )
    | "second" -> 
        T.AccessExpr (
            a, 
            e2var (Atom.builtin "_2")
        )
    | "sessionid" -> 
        T.CallExpr(
            e2_e (T.AccessExpr (
                a, 
                e2var (Atom.builtin "get_id")
            )),
            []
        )
    |"string_of_bridge" -> 
        T.CallExpr(
            e2_e (T.AccessExpr (a, e2var (Atom.builtin "toString"))),
            []
        )
    | "activationsat" ->
        (* TODO rename activationsat or componentsat in order to have same name in java and ocaml code*)
        T.CallExpr(
            e_varda_componentsat place,
            [
                e_get_context place;
                a
            ]
        )
    | "placeof" -> 
        T.CallExpr(
            e_varda_placeof place,
            [ e_get_context place; a ]
        )
    | "pick" ->
        T.CallExpr(
            e2var (Atom.builtin "com.varda.Utils.pick"),
            [ a ]
        )
    | "debug" -> 
        T.CallExpr(
            e2var (Atom.builtin "getContext().getLog().debug"),
            [ a ]
        )
    | "info" -> 
        T.CallExpr(
            e2var (Atom.builtin "getContext().getLog().info"),
            [ a ]
        )
    | "option_get" -> 
        T.CallExpr(
            e2_e (T.AccessExpr( 
                a,
                e2var (Atom.builtin "get")
            )),
            [ ]
        )
    | "get_ok" -> 
        T.CallExpr(
            e2_e (T.AccessExpr( 
                a,
                e2var (Atom.builtin "get")
            )),
            [ ]
        )
    | "get_err" -> 
        T.CallExpr(
            e2_e (T.AccessExpr( 
                a,
                e2var (Atom.builtin "getLeft")
            )),
            [ ]
        )
    | "setlength" -> 
        T.CastExpr(
            auto_place (T.Atomic "Integer"),
            e2_e (T.CallExpr(
                e2_e (T.AccessExpr( 
                    a,
                    e2_e (T.RawExpr "size")
                )),
                [ ]
            ))
        )
    | "leftactivations" ->
        T.CallExpr(
            e2_e (T.AccessExpr(
                a,
                e2_e (T.RawExpr "leftActivations")
            )),
            [ 
                e_get_context place;
                e_this_guardian (this_actor parent_opt) place;
            ]
        )
    | "rightactivations" ->
        T.CallExpr(
            e2_e (T.AccessExpr(
                a,
                e2_e (T.RawExpr "rightActivations")
            )),
            [ 
                e_get_context place;
                e_this_guardian (this_actor parent_opt) place;
            ]
        )
    | "bridgeof_in" | "bridgeof_out" ->
        T.AccessExpr(
            a,
            e2var (Atom.builtin "bridge")
        )
    | "long_of_int" ->
        T.CallExpr(
            e2_e (T.RawExpr "Long.valueOf"),
            [ a ]
        )
    | "is_init_stage" ->
        T.AccessExpr(
            a,
            e2_e (T.RawExpr "init_stage"))
    |"protocolof" ->
        T.AccessExpr(
            a,
            e2_e (T.RawExpr "protocol"))
    |"session_from" ->
        T.AccessExpr(
            a,
            e2_e (T.RawExpr "left"))
    |"session_to" ->
        T.AccessExpr(
            a,
            e2_e (T.RawExpr "right"))
    | "session_to_2_" ->
            T.AccessExpr(
                a,
                e2_e (T.RawExpr "hidden_right")
            )
    | "list2array" -> begin
        match (snd a.value).value with
        | T.TList mt -> 
            T.NewExpr(
                e2_e (T.RawExpr "ArrayList"),
                [ a ]
            )
        | _ -> raise (Error.PlacedDeadbranchError (a.place, "should be of type: list<?>")) 
    end
    | _ -> Error.perror place "%s with one argument is undefined" name

let encode_builtin_fct_2 parent_opt place name a b =
    let auto_place t = {place; value=t} in 
    match name with
    | "bind_in" ->
        T.CallExpr(
            e2_e (T.AccessExpr(
                e2_e (this_actor parent_opt),
                e2var (Atom.builtin "bind_in")
            )),
            [ 
                a; 
                b 
            ]
        )
    | "bind_out" ->
        T.CallExpr(
            e2_e (T.AccessExpr(
                e2_e (this_actor parent_opt),
                e2var (Atom.builtin "bind_out")
            )),
            [ 
                a; 
                b 
            ]
        )
    | "remove2dict" -> 
        T.CallExpr( 
            e2_e (T.AccessExpr (
                a, 
                e2var (Atom.builtin "remove")
            )),
            [ b ]
        ) 
    | "get2dict" ->
        T.CallExpr( 
            e2_e (T.AccessExpr (
                a, 
                e2var (Atom.builtin "get")
            )),
            [ b ]
        ) 
    | "exist2dict" ->
        T.CallExpr( 
            e2_e (T.AccessExpr (
                a, 
                e2var (Atom.builtin "containsKey")
            )),
            [ b ]
        ) 
    | "fire" -> 
        T.CallExpr( 
            e2_e (T.AccessExpr (
                a, 
                e2var (Atom.builtin "fire")
            )),
            [ 
                b; 
                e_get_context place;
                e_this_timers (this_actor parent_opt) place;
                e_this_frozen_sessions (this_actor parent_opt) place;
                e_this_dead_sessions (this_actor parent_opt) place;
            ]
        ) 
    | "leftregister" ->
        T.CallExpr(
            e2_e (T.AccessExpr(
                a,
                e2_e (T.RawExpr "letRegister")
            )),
            [ 
                e_get_context place;
                e_this_guardian (this_actor parent_opt) place;
                b;
            ]
        )
    | "rightregister" ->
        T.CallExpr(
            e2_e (T.AccessExpr(
                a,
                e2_e (T.RawExpr "letRegister")
            )),
            [ 
                e_get_context place;
                e_this_guardian (this_actor parent_opt) place;
                b;
            ]
        )    
        | "register_activation_at" -> 
        T.CallExpr( 
            e2_e (T.RawExpr "PlaceDiscovery.register"),
            [ 
                e_get_context place;
                e2_e (T.ClassOf (match (snd a.value).value with
                    | T.TActivationRef t -> t
                    | _ ->
                        raise (Error.PlacedDeadbranchError (b.place, "Not typed with activation_ref"))
                ));
                b;
                a;
            ]
        ) 
    | "select" -> 
        T.CallExpr( 
            e2_e (T.AccessExpr (
                a, 
                e2var (Atom.builtin "select")
            )),
            [ 
                b; 
                e_get_context place;
                e_this_timers (this_actor parent_opt) place;
                e_this_frozen_sessions (this_actor parent_opt) place;
                e_this_dead_sessions (this_actor parent_opt) place;
            ]
        ) 
    | "select_local" -> 
        T.CallExpr( 
            e2_e (T.AccessExpr (
                a, 
                e2var (Atom.builtin "apply_select")
            )),
            [ 
                b; 
                e_get_context place;
            ]
        ) 
    | "listget" ->
        T.CallExpr(
            e2_e (T.AccessExpr (
                a, 
                e2var (Atom.builtin "get")
            )),
            [ b ]
        )
    | "aget" -> begin
        match (snd a.value).value with
        | T.TArray mt -> 
            T.CastExpr(
                mt,
                e2_e (T.CallExpr(
                    e2_e(T.AccessExpr(
                        a,
                        e2var (Atom.builtin "get")
                    )),
                    [ b]
                ))
            )
        | _ -> raise (Error.PlacedDeadbranchError (a.place, "should be of type: array<?>"))
    end
    | "append" -> 
        T.CallExpr(
            e2_e (T.AccessExpr(
                a,
                e2_e (T.RawExpr "add")
            )),
            [ b ]
        )
    | "range" ->
        T.NewExpr(
            e2_e (T.RawExpr "IntegerRange"),
            [ a; b ]
        )
    | "select_places" ->
        fst (e_varda_select_places place a b).value
    | "initiate_session_with" ->
        (* TODO i need to get the name of the type of the protocol 
        Maybe some thing like protocol is a value and we bind a protocol type with it.
        *)
        T.CallExpr (
            e2_e (T.AccessExpr (a, e2var (Atom.builtin "initiate_session_with"))),
            [
                e2_e (T.CastExpr(
                    auto_place (T.TVar (Atom.builtin "ActivationRef")), (* TODO can we move cast elsewhere ?*)
                    Misc.e_get_self_activation place (Misc.e_get_context place)
                ));
                b;
                Misc.e_none place ; (* FIXME interception should change this*)
            ]
        )
    | "one_hop_activation_ref" ->
        T.NewExpr(
            e2_e (T.RawExpr "ActivationRef"),
            [a; b]
        )
    | "complete_future" ->
        T.CallExpr(
            e2_e (T.AccessExpr(a, e2_e(T.RawExpr "complete"))),
            [ b]
        )
    | _ -> Error.perror place "%s takes two arguments" name

let encode_builtin_fct_3 parent_opt place name a b c =
    let auto_place t = {place; value=t} in 
    match name with
    | "add2dict" -> 
        T.CallExpr( 
            e2_e (T.AccessExpr (
                a, 
                e2var (Atom.builtin "put")
            )),
            [ b; c ]
        ) 
    | "aput" -> begin
        match (snd a.value).value with
        | T.TArray _ -> 
            T.CallExpr(
                e2_e(T.AccessExpr(
                    a,
                    e2var (Atom.builtin "set")
                )),
                [ b; c]
            )
        | _ -> raise (Error.PlacedDeadbranchError (a.place, "should be of type: array<?>"))
    end
    | "bind_in_inlined" ->
        T.CallExpr(
            e2_e (T.AccessExpr(
                e2_e (this_actor parent_opt),
                e2var (Atom.builtin "bind_in")
            )),
            [ 
                a; 
                b;
                c 
            ]
        )
    | "bind_out_inlined" ->
        T.CallExpr(
            e2_e (T.AccessExpr(
                e2_e (this_actor parent_opt),
                e2var (Atom.builtin "bind_out")
            )),
            [ 
                a; 
                b;
                c
            ]
        )
    | "initiate_session_with_inlined" ->
        (* TODO i need to get the name of the type of the protocol 
        Maybe some thing like protocol is a value and we bind a protocol type with it.
        *)
        T.CallExpr (
            e2_e (T.AccessExpr (a, e2var (Atom.builtin "initiate_session_with"))),
            [
                e2_e (T.CastExpr(
                    auto_place (T.TVar (Atom.builtin "ActivationRef")), (* TODO can we move cast elsewhere ?*)
                    Misc.e_get_self_activation place (Misc.e_get_context place)
                ));
                b;
                c;
            ]
        )
    | _ -> Error.perror place "%s takes three arguments" name
    

let is_stmt_builtin = function
| "sleep" -> true
| "exit" -> true
| "wait_future" -> true
| _ -> false

let encode_builtin_fct parent_opt place name (args:T.expr list) =
    assert(Builtin.is_builtin_expr name);
    let auto_place t = {place; value=t} in 
    match name with
    (* TODO Remove string and used typed constructor in order to ensure that this file is uptodate with the Builtin.builtin_fcts*)
    | name when is_stmt_builtin name-> Error.perror place "In Akka, %s must be convertible to a statement" name
    | _ -> begin
        match args with
        | [] -> encode_builtin_fct_0 parent_opt place name 
        | [a] -> encode_builtin_fct_1 parent_opt place name a
        | [a;b] -> encode_builtin_fct_2 parent_opt place name a b
        | [a;b;c] -> encode_builtin_fct_3 parent_opt place name a b c
        | _ -> Error.perror place "Akka.Finish do not yet support builtin function %s" name
    end


let encode_builtin_fct_as_stmt with_return place name (args:T.expr list) =
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
    | "wait_future" -> begin
        match args with
        | [ f; timeout ] -> 

            let e = Atom.fresh "e" in
            let inner = e2_e ( T.CallExpr(
                        e2_e (T.AccessExpr(f, e2_e(T.RawExpr "get"))),
                        [
                            timeout;
                            e2_e (T.RawExpr "TimeUnit.MILLISECONDS")
                        ]
                    )) in
            let core = T.TryStmt(
                (
                    if with_return then 
                        auto_fplace (T.ReturnStmt inner)
                    else
                        auto_place(T.ExpressionStmt inner)
                ),
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
            ) in

            if with_return then
                T.BlockStmt [
                    auto_fplace core;
                    auto_fplace (T.ReturnStmt (e2_e (T.RawExpr "null")));
                ]
            else core
        | _ -> Error.perror place "wait_future must take two arguments" 
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