open Utils
open Error
open Easy_logging
open Fieldslib

module S = IR
module T = IRD
(*
let logger = Logging.make_logger "_1_ compspec.frontend" Debug [];;

(* atom -> debruijn *)
module Env = Atom.VMap

type env = Env.t
let fresh_env () = Env.empty

let rec a2d_var_expr env place x : Debruijn.t=
    if Atom.is_builtin x then DeBruijn.DeBuiltin x
    else Env.find x env
and a2d_var_type env place x = x
and a2d_var_component env place x = x

(************************************ Types **********************************)

(* notation debruin uniquement pour les expressions le reste reste en atom*)
and a2d_composed_type env ct = env, ct
and _a2d_session_type env place = function
| S.STEnd -> T.STEnd
| S.STVar x -> T.STVar x
| S.STSend (mt, st) -> 
    let mt = a2d_main_type env mt in
    let st = a2d_session_type env st in
    T.STSend (mt, st)
| S.Recv (mt, st) -> 
    let mt = a2d_main_type env mt in
    let st = a2d_session_type env st in
    T.STRecv (mt, st)
| S.STRec (x, st) -> 
    let st = a2d_session_type env st in
    T.STRecv (x, st)
| S.STInline x -> S.STInline x 

and a2d_session_type env = a2d_place _a2d_session_type env 

and _a2d_component_type env place = function
| S.CompTUid x -> T.CompTUid x
and a2d_component_type env = a2d_place _a2d_component_type env 

and _a2d_main_type env place = function
| S.CType ct -> T.CType (a2d_composed_type env ct)
| S.SType st -> T.SType (a2d_session_type env st)
| S.CompType ct -> T.CompType (a2d_composed_type env ct)
| S.ConstrainedType (mt, guard) -> T.ConstrainedType (
    a2d_main_type env mt, 
    a2d_applied_constraint env mt)
and a2d_main_type env = a2d_place _a2d_main_type env 

(******************************** Constraints ********************************)



*)