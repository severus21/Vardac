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

type env = unit
let fresh_env () = ()

let rec cook_target env (target:S.target) : env * T.target =
    env, {
        place = target.place; 
        value = {
            T.name = target.value.name; 
            codegen = target.value.codegen
        }
    } 
                
let cook_targets (targets : S.targets) : T.targets =
   snd (List.fold_left_map cook_target (fresh_env ()) targets)  
