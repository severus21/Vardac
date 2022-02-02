(** Adding code placement information to AST element (for debugging)*)
type 'a placed = {
  (* printer without place annoatation *)
  place: Error.place;[@opaque]
  (*  printer with place annotation:
  place: Error.place;[@printer fun fmt ->let pp_pos fmt ({pos_fname=n1; pos_lnum=l1; pos_bol=b1; pos_cnum=c1}:Lexing.position) = fprintf fmt "{pos_fname=%s; pos_lnum=%d; pos_bol=%d; pos_cnum=%d}" n1 l1 b1 c1 in let pp_place fmt (pos1, pos2) = fprintf fmt "(%a,%a)" pp_pos pos1 pp_pos pos2 in fprintf fmt "%a" pp_place  ]*)
  value: 'a
} [@@deriving show { with_path = false }]


(* Printing a syntax tree in an intermediate language (for debugging). *)

let print_delimiter () =
  Printf.eprintf "----------------------------------------";
  Printf.eprintf "----------------------------------------\n"

let dump ?(print=(Config.debug ()))(phase : string) (show : 'term -> string) (t : 'term) =
  if print then begin
    print_delimiter();
    Printf.eprintf "%s:\n\n%s\n\n%!" phase (show t)
  end;
  t

(* Define how variable are represented *)
module type TVariable = sig
    type t 
    (* Deriving *)
    val show :  t -> Ppx_deriving_runtime.string
    val pp : Ppx_deriving_runtime.Format.formatter -> t -> Ppx_deriving_runtime.unit

    (* Pretty printing *)
    val to_string : t -> string
    val p_to_string : (t -> string) -> t -> string
    module Set : sig
        include Set.S with type elt = t 
        val show: t -> string
        val print: out_channel -> t -> unit

        val to_list: t -> elt list
    end

    module VMap : sig
        include Map.S with type key = t 
    end

    val is_builtin : t -> bool
end

module type IRParams = sig
    module Variable : TVariable 
end

type _comments =
    | BlockComment of string
    | DocComment of string
    | LineComment of string
and comments = _comments placed
[@@deriving show { with_path = false }]

type flat_type = 
    (** Literal types *)
    | TActivationID (* can not be created by programmer -> no constructor *)
    | TBool
    | TInt
    | TUUID
    | TFloat 
    | TStr
    | TLabel
    | TBLabel (* label for non deterministic choices (STBranch/STSelect) only *)
    | TVoid
    | TPlace
    | TSessionID (* can not be created by programmer -> no constructor *)
    | TTimer
    | TWildcard
[@@deriving show { with_path = false }]

type unop = 
    | Not 
    | UnpackResult

and binop =
    (* Boolean *)
    | And    
    | Or 

    (* Numeric *)
    | Plus
    | Minus
    | Mult
    | Divide

    (** Comparison *)
    | StructuralEqual (*(e.g. like equals in Java or = in Ocaml)*)
    | Equal 
    | GreaterThanEqual
    | LessThanEqual
    | GreaterThan
    | LessThan

    (* Iterators *)
    | In

and block = 
    | Block (* tODO FIXME is used, if not get ride of this*)
    | List 
    | Tuple
    | Set
and block2 =
    | Dict
[@@deriving show { with_path = false }]


let rec map0_place (fct:Error.place -> 'a -> 'b) ({place; value}:'a placed) : 'b  = 
    fct place value 
let rec map_place (fct:Error.place -> 'a -> 'b) ({place; value}:'a placed) : 'b placed = 
    { place; value = fct place value }

let rec map2_place (fct:Error.place -> 'a -> 'b * 'c) ({place; value}:'a placed) : 'b * 'c placed = 
    let env, value = fct place value in
    env, { place; value = value }

let map_places (fct:Error.place -> 'a -> 'b list) ({place; value}:'a placed) : ('b placed list)= 
    let values = fct place value in
    List.map (function v -> { place; value = v}) values

let map2_places (fct:Error.place -> 'a -> 'b * 'c list) ({place; value}:'a placed) : 'b * ('c placed list)= 
    let env, values = fct place value in
    env, List.map (function v -> { place; value = v}) values
