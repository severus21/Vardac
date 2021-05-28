
(* cook the different pieces (ast, place ast) into the IR 
    - translate string identifier to variable while preserving architect notation
*)

open Core
open Core.Error
open Core.AstUtils

(* The source calculus. *)
module S = Ast 
(* The target calculus. *)
module T = IR 

(* Environments map strings to atoms. *)
module Env = Map.Make(String)

(* [bind env x] creates a fresh atom [a] and extends the environment [env]
   with a mapping of [x] to [a]. *)
let bind env x =
  let a = Atom.fresh x in
  Env.add x a env, a

let rec cook_vplace env {Core.AstUtils.place; Core.AstUtils.value} = 
match value with
| S.VPlaceVar x -> begin           
    try
        env, {place; value=T.VPlaceVar (Env.find x env)}
    with Not_found ->
        error place "Unbound place variable: %s" x
end
| S.VPlaceDcl vp -> 
    let env, children = List.fold_left_map cook_vplace env vp.children in 
    let env, name = bind env vp.name in
    env, {place; value=T.VPlaceDcl {  name; 
                        nbr_instances=Cook.cook_expression vp.nbr_instances;
                        features=vp.features;
                        children } }
                
let cook_vplaces vplaces : T.vplaces =
   snd (List.fold_left_map cook_vplace Env.empty vplaces)  
   
