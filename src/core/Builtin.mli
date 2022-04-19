open IR

val builtin_fcts : (string * string * string * (unit -> IR.main_type)) list
val builtin_atomic_types : string list  

val re_tuple_attr : Str.regexp 
val re_inductive_attr : Str.regexp 
val is_tuple_attr : string -> bool
val is_inductive_attr : string -> bool
val pos_of_tuple_attr : string -> int
val pos_of_inductive_attr : string -> int

val is_builtin_component : string -> bool
val is_builtin_expr : string -> bool
val is_builtin_type : string -> bool
val is_builtin_derivation : string -> bool
val is_builtin_inductive_type : IR.flat_type -> bool 
val sig_of_builtin_inductive_type : IR.flat_type -> IR.main_type list 

val type_of : Error.place -> string -> IR.main_type
val desc_of : string -> string

(* *)
val builtin_mt_error: main_type

