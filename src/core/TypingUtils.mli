open IR

(* Dans type inference*)
(*context already gamma do we need more yes*)
type context = {
    ectx : main_type Atom.VMap.t; (* Typing context for expressions *) 
    tctx : main_type Atom.VMap.t; (* Contexts of types*)
    cctx : main_type Atom.VMap.t; (* Typing context for components *)
    self : component_variable option
}

val print_context : context -> unit
val fresh_context : unit -> context

val typeof_var_expr : context -> expr_variable -> main_type
val typeof_var_cexpr : context -> component_variable -> main_type
val defof_tvar : context -> type_variable -> main_type
val register_expr_type : context -> expr_variable -> main_type -> context
val register_cexpr_type : context -> component_variable -> main_type -> context
val register_self : context -> component_variable -> context
val register_def_type : context -> _typedef -> context
val register_type : context -> type_variable -> main_type -> context

type tconstraint = 
| Equality of main_type * main_type
| Or of tconstraint list
[@@deriving show { with_path = false }]
val mgu_solver : Error.place -> tconstraint list -> (Atom.t * main_type) list

(* subtype S T returns true if S <: T, returns false otherwise *)
val is_subtype : main_type -> main_type -> bool

(* try to unify the forall i.e. find the substution *)
val is_instance : main_type -> main_type -> bool
