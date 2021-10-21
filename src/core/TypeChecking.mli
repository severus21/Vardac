open IR
open TypingUtils
(*
(* subtype S T returns true if S <: T, returns false otherwise *)
val subtype : main_type -> main_type -> bool

(* raise error if type errors *)
val check_expr : expr -> unit



val typeof_expr : expr -> main_type
(* TODO val typeof_component : component_dcl -> main_type*)

(* _e, EmptyMainType -> _e, typeof_expr e *)
val tannot_expr : expr -> expr
val tannot_program : program -> expr
*)