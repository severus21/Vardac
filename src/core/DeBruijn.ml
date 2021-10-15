
(* -------------------------------------------------------------------------- *)


(* Builtin are n*)
type var = { 
    identity: int; (* De Bruijn indices *)
    hint: string; 
}

and debruijn = 
| DeBruijn of var
| DeBuiltin of string

and t = debruijn
  [@@deriving show { with_path = false }]

let identity a =
  a.identity
let hint a =
  a.hint
let is_builtin = function
| DeBruijn _ -> false
| DeBuiltin _ -> true
(* -------------------------------------------------------------------------- *)

(* Comparison of atoms. *)

let equal = function 
| (DeBruijn a, DeBruijn b) -> a.identity = b.identity
| (DeBuiltin a, DeBuiltin b) -> a = b
| _ -> false

let compare a b =
  (* Identities are always positive numbers (see [allocate] above)
     so I believe overflow is impossible here. *)
match (a,b) with
| (DeBruijn a, DeBruijn b) -> a.identity - b.identity
| (DeBuiltin a, DeBuiltin b) -> String.compare a b
| DeBruijn a, DeBuiltin b -> -1 - a.identity - Int.abs (String.compare a.hint b)
| DeBuiltin a, DeBruijn b -> 1 + b.identity + Int.abs (String.compare a b.hint)

let hash = function  
| DeBruijn a -> Hashtbl.hash a.identity
| DeBuiltin a -> Hashtbl.hash a
(* -------------------------------------------------------------------------- *)

(* A scratch buffer for printing. *)

let scratch =
  Buffer.create 1024

(* [print_separated_sequence] prints a sequence of elements into the [scratch]
   buffer. The sequence is given by the higher-order iterator [iter], applied
   to the collection [xs]. The separator is the string [sep]. Each element is
   transformed to a string by the function [show]. *)

let print_separated_sequence show sep iter xs : unit =
  let first = ref true in
  iter (fun x ->
    if !first then begin
      Buffer.add_string scratch (show x);
      first := false
    end
    else begin
      Buffer.add_string scratch sep;
      Buffer.add_string scratch (show x)
    end
  ) xs

(* -------------------------------------------------------------------------- *)

(* Sets and maps. *)

module Order = struct
  type t = debruijn 
  let compare = compare
end

module Set = struct

  include Set.Make(Order)

  (* A disjointness test. *)

  let disjoint xs ys =
    is_empty (inter xs ys)

  (* Iterated union. *)

  let union_many (f : 'a -> t) (xs : 'a list) : t =
    List.fold_left (fun accu x ->
      union accu (f x)
      ) empty xs

  (* Disjoint union. *)

  exception NonDisjointUnion of debruijn 

  let disjoint_union xs ys =
    match choose (inter xs ys) with
    | exception Not_found ->
        (* The intersection of [xs] and [ys] is empty. Return their union. *)
        union xs ys
    | x ->
        (* The intersection contains [x]. Raise an exception. *)
        raise (NonDisjointUnion x)

  let handle_NonDisjointUnion f x =
    try
      f x; true
    with NonDisjointUnion a ->
      Printf.eprintf "NonDisjointUnion: %s\n%!" (show a);
      false

  (* Sets of atoms form a monoid under union. *)

  class ['z] union_monoid = object
    method zero: 'z = empty
    method plus: 'z -> 'z -> 'z = union
  end

  (* Sets of atoms form a monoid under disjoint union. *)

  class ['z] disjoint_union_monoid = object
    method zero: 'z = empty
    method plus: 'z -> 'z -> 'z = disjoint_union
  end

  (* These printing functions should be used for debugging purposes only. *)

  let print_to_scratch xs =
    Buffer.clear scratch;
    Buffer.add_string scratch "{";
    print_separated_sequence show ", " iter xs;
    Buffer.add_string scratch "}"

  let show xs =
    print_to_scratch xs;
    let result = Buffer.contents scratch in
    Buffer.reset scratch;
    result

  let print oc xs =
    print_to_scratch xs;
    Buffer.output_buffer oc scratch;
    Buffer.reset scratch

end

module VMap = struct

  include Map.Make(Order)

  (* This is O(nlog n), whereas in principle O(n) is possible.
     The abstraction barrier in OCaml's [Set] module hinders us. *)
  let domain m =
    fold (fun a _ accu -> Set.add a accu) m Set.empty

  let codomain f m =
    fold (fun _ v accu -> Set.union (f v) accu) m Set.empty

end

type renaming = debruijn VMap.t

(* Printing *)
let output_debruijn out builtin_eval = function
| DeBruijn x -> Printf.fprintf out "%s%d" (hint x) (identity x)
| DeBuiltin x -> Printf.fprintf out "%s" x

let to_string = function 
| DeBruijn x -> (hint x)^(string_of_int (identity x))
| DeBuiltin x -> x 

let p_to_string builtin_eval = function
| DeBruijn _ as x -> to_string x
| DeBuiltin _ as x -> (builtin_eval x)

(* Sets and maps. *)

module DebruijnsOrder = struct
  type t = debruijn list
  let compare xs ys= 
    let lxs = List.length xs in
    let lys = List.length ys in

    if lxs <> lys then 
      lxs - lys
    else (
      List.fold_left (fun acc (x,y) -> acc + compare x y) 0 (List.combine xs ys)
    )
end

module DebruijnsMap = struct
  include Map.Make(DebruijnsOrder)
end

module DebruijnVariable = struct 
  type t = debruijn 
  [@@deriving show { with_path = false }]

  module Set = Set
  module VMap = VMap

  let to_string = to_string
  let p_to_string = p_to_string
  let is_builtin = is_builtin
end