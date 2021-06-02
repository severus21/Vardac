(* from https://gitlab.inria.fr/fpottier/alphaLib*)
(* TEMPORARY document *)

(* An atom is a pair of a unique integer identity and a (not necessarily
   unique) string. *)

type atom

and t = atom
  [@@deriving show]

val identity: atom -> int
val hint: atom -> string
val value: atom -> string
val builtin: atom -> bool

(* Producing fresh atoms. *)

val fresh: string -> atom
val fresh_builtin: string -> atom
val copy: atom -> atom
val copy_upper: atom -> atom

(* Comparison of atoms. *)

val equal: atom -> atom -> bool
val compare: atom -> atom -> int
val hash: atom -> int

(* Sets. *)

module Set : sig
  include Set.S with type elt = atom

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

  val show: t -> string
  val print: out_channel -> t -> unit
end

(* Maps. *)
module AtomMap : sig

  include Map.S with type key = atom

  val domain: 'a t -> Set.t
  val codomain: ('a -> Set.t) -> 'a t -> Set.t

end

module AtomsMap : sig
  include Map.S with type key = atom list
end

type renaming =
  atom AtomMap.t

(* Printing helper *)
val output_atom : out_channel -> (atom -> string) -> atom -> unit

val atom_to_str : atom -> string
val p_atom_to_str : (atom -> string) -> atom -> string
