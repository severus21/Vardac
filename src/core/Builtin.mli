val builtin_fcts : (string * string * string) list
val builtin_atomic_types : string list  

val is_builtin_component : string -> bool
val is_builtin_expr : string -> bool
val is_builtin_type : string -> bool

val type_of : string -> IR.main_type
val desc_of : string -> string

