open Core
open Utils
open AstUtils
open Easy_logging
open Fieldslib

let plg_name = "Akka"
let logger = Logging.make_logger ("_1_ compspec.plg."^plg_name) Debug [];;

(* The source calculus. *)
module S = IRI 
(* The target calculus. *)
module T = Ast 


let encode_builtin_fct place name (args:T.expr list) =
    assert(Core.Builtin.is_builtin_expr name);
    match name with
    (* TODO Remove string and used typed constructor in order to ensure that this file is uptodate with the Core.Builtin.builtin_fcts*)
    | "fire" -> begin 
        match args with
        | [ session; msg ] -> T.CallExpr( 
            {
                place;
                value = T.AccessExpr (session, {place; value = T.VarExpr (Atom.fresh_builtin "fire")})
            },
            [ msg ]
        ) 
        | _ -> Error.error place "fire must take two arguments : place(session, message)"
        end
    | "first" -> begin
        match args with
        | [ tuple ] ->  T.AccessExpr (tuple, {place; value = T.VarExpr (Atom.fresh_builtin "_1")})
        | _ -> Error.error place "first must take one argument"
    end
    | "initiate_session" -> begin
        (* TODO i need to get the name of the type of the protocol 
        Maybe some thing like protocol is a value and we bind a protocol type with it.
        *)
        match args with
        | [ tuple ] ->  T.AccessExpr (tuple, {place; value = T.VarExpr (Atom.fresh_builtin "_1")})
        | _ -> Error.error place "first must take one argument"
    end
    | _ -> 
        Error.error place "Akka.Finish do not yet support builtin function %s" name