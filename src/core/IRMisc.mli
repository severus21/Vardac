open IR


val aid_of : Error.place -> expr -> expr
val schema_of : component_expr -> Atom.atom
val schema_to_label : Error.place -> Atom.atom -> expr

val dual : session_type -> session_type
val unfold_st_star : session_type -> session_type
val stages_of_st : session_type -> _session_type list
val msgcont_of_st: session_type -> main_type * session_type 

val e_param_of : string -> Atom.atom * expr