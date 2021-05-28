open Core
open Core.Error
open Core.AstUtils

(* variable : string => Atom.atom + check env *)
(* The source calculus. *)
module S = RawTarget 
(* The target calculus. *)
module T = Target 

(* Environments map strings to atoms. *)
module Env = Map.Make(String)

(* [bind env x] creates a fresh atom [a] and extends the environment [env]
   with a mapping of [x] to [a]. *)
let bind env x =
    let a = Atom.fresh x in
    Env.add x a env, a

let rec cook_target env {Core.AstUtils.place; Core.AstUtils.value} =
match value with 
| S.Target {name; codegen} -> 
    let env, name = bind env name in
    env, {place; value=T.Target {name; codegen}} 
                
let cook_targets (targets : S.targets) : T.targets =
   snd (List.fold_left_map cook_target Env.empty targets)  
