open Core
open Utils
open AstUtils
open Easy_logging
open Fieldslib
open Misc

let plg_name = "Akka"
let logger = Logging.make_logger ("_1_ compspec.plg."^plg_name) Debug [];;

(* The source calculus. *)
module S = IRI 
(* The target calculus. *)
module T = Ast 


let encode_builtin_fct place name (args:T.expr list) =
    assert(Core.Builtin.is_builtin_expr name);
    let auto_place t = {place; value=t} in 
    match name with
    (* TODO Remove string and used typed constructor in order to ensure that this file is uptodate with the Core.Builtin.builtin_fcts*)
    | "fire" -> begin 
        match args with
        | [ session; msg ] -> T.CallExpr( 
            {
                place;
                value = T.AccessExpr (session, {place; value = T.VarExpr (Atom.fresh_builtin "fire")})
            },
            [ 
                msg; 
                e_get_context place
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
    | "print" -> begin
        match args with
        | [ msg ] -> T.CallExpr (({place; value=T.VarExpr (Atom.fresh_builtin "System.out.println")}), [msg])
        | _ -> Error.error place "print must take one argument" 
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
                    right 
                ]
            )
        | _ -> Error.error place "first must take one argument"
    end
    | _ -> 
        Error.error place "Akka.Finish do not yet support builtin function %s" name