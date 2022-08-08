(** Atom define variable representation in Varda intermediate states. Each binder introduce a unique name. We do not use DeBruijn notations to simplify maintaining globl information in between compilation transformation. *)

(* from https://gitlab.inria.fr/fpottier/alphaLib*)
(* TEMPORARY document *)

(** An atom is a pair of a unique integer identity and a (not necessarily
   unique) string. *)
type atom

and t = atom
  [@@deriving show, hash]

val identity: atom -> int
val hint: atom -> string
val value: atom -> string
val is_builtin: atom -> bool

(* Producing fresh atoms. *)

val fresh: string -> atom
val builtin: string -> atom
val copy: atom -> atom
val copy_upper: atom -> atom
val craft: int -> string -> string -> bool -> atom

val refresh_hint : atom -> string -> atom
val refresh_value : atom -> string -> atom

(* Comparison of atoms. *)

val compare_atom: atom -> atom -> int (* for ppx_compare, alias of compare *)
val equal_atom: atom -> atom -> bool (* for ppx_equal, alias of equal *)
val equal: atom -> atom -> bool
val compare: atom -> atom -> int
val hash: atom -> int

(* Sets. *)

module Set2 : sig
  include Set.S with type elt = atom * atom

  val to_list: t -> elt list
  val of_list: elt list -> t
end
module Set : sig
  include Set.S with type elt = atom

  val to_list: t -> elt list
  val of_list: elt list -> t

(* TODO NOT yet included in TVariable
  val disjoint: t -> t -> bool

  val union_many: ('a -> t) -> 'a list -> t

  (* Disjoint union. *)

  exception NonDisjointUnion of atom
  val disjoint_union: t -> t -> t

  val handle_NonDisjointUnion: ('a -> unit) -> 'a -> bool

  (* Sets of atoms form monoids under union and disjoint union. *)

  class ['z] union_monoid : object
    constraint 'z = t
    method zero: 'z
    method plus: 'z -> 'z -> 'z
  end

  class ['z] disjoint_union_monoid : object
    constraint 'z = t
    method zero: 'z
    method plus: 'z -> 'z -> 'z
  end
*)
  val show: t -> string
  val print: out_channel -> t -> unit
end

(* Maps. *)
module VMap : sig
  include Map.S with type key = atom

  val domain: 'a t -> Set.t
  val codomain: ('a -> Set.t) -> 'a t -> Set.t

  val pp : ('a->'c) -> Ppx_deriving_runtime.Format.formatter -> 'b t -> unit (*Ppx_deriving_runtime.Format.formatter -> 'a t -> Ppx_deriving_runtime.unit*)
  val show : 'a t -> Ppx_deriving_runtime.string

  val to_list: 'a t -> (key * 'a) list
  val of_list: (key * 'a) list -> 'a t

  val hash_fold_t : 'a Ppx_hash_lib.Std.Hash.folder -> 'b t Ppx_hash_lib.Std.Hash.folder

end

module AtomsMap : sig
  include Map.S with type key = atom list
end

type renaming =
  atom VMap.t

(* Printing helper *)
val output_atom : out_channel -> (atom -> string) -> atom -> unit

val to_string : atom -> string
val p_to_string : (atom -> string) -> atom -> string

val deduplicate : atom list -> atom list

val show_list : string -> atom list -> string
val pp_list : string -> (Format.formatter -> 'a -> unit) -> Format.formatter -> atom list -> unit 

module AtomVariable : (AstUtils.TVariable with type t = atom and module Set = Set)