type debruijn = 
| DeBruijn of int * Atom.atom 
| DeBuiltin of Atom.atom 

and t = debruijn
  [@@deriving show]

(* Comparison of debruijns. *)

val equal: debruijn -> debruijn -> bool
val compare: debruijn -> debruijn -> int
val hash: debruijn -> int

(* Sets. *)

module Set : sig
  include Set.S with type elt = debruijn
(* NOT yet included in TVariable
  val disjoint: t -> t -> bool

  val union_many: ('a -> t) -> 'a list -> t

  (* Disjoint union. *)

  exception NonDisjointUnion of debruijn
  val disjoint_union: t -> t -> t

  val handle_NonDisjointUnion: ('a -> unit) -> 'a -> bool

  (* Sets of debruijns form monoids under union and disjoint union. *)

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
  include Map.S with type key = debruijn

  val domain: 'a t -> Set.t
  val codomain: ('a -> Set.t) -> 'a t -> Set.t

end

module DebruijnsMap : sig
  include Map.S with type key = debruijn list
end

type renaming =
  debruijn VMap.t

(* Printing helper *)
val output_debruijn : out_channel -> (debruijn -> string) -> debruijn -> unit

val to_string : debruijn -> string
val p_to_string : (debruijn -> string) -> debruijn -> string


module DebruijnVariable : (AstUtils.TVariable with type t = debruijn and module Set = Set)