open Core
open IR


type tconstraint = 
| Equality of main_type * main_type
| Or of tconstraint list
[@@deriving show { with_path = false }]
val mgu_solver : Error.place -> tconstraint list -> (Atom.t * main_type) list

(** subtype S T returns true if S <: T, returns false otherwise *)
val is_subtype : main_type -> main_type -> bool

val is_suffix_st : session_type -> session_type -> bool

(* try to unify the forall i.e. find the substution *)
val is_instance : main_type -> main_type -> bool

val fct_sign : main_type list -> main_type -> main_type

(* From a signature return (arg type list, ret_type) *)
val inv_fct_sign : main_type -> main_type list * main_type
