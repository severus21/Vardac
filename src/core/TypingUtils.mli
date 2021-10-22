open IR

(* Dans type inference*)
(*context already gamma do we need more yes*)
type context = {
    ectx : main_type Atom.VMap.t; (* Typing context for expressions *) 
    tctx : main_type Atom.VMap.t; (* Contexts of types*)
    cctx : main_type Atom.VMap.t; (* Typing context for components *)
    self : component_variable option
}
val fresh_context : unit -> context

val typeof_var_expr : context -> expr_variable -> main_type
val typeof_var_cexpr : context -> component_variable -> main_type
val register_expr_type : context -> expr_variable -> main_type -> context
val register_cexpr_type : context -> component_variable -> main_type -> context
val register_self : context -> component_variable -> context
val register_def_type : context -> _typedef -> context
val register_type : context -> type_variable -> main_type -> context
