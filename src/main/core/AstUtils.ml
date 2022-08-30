open Ppx_hash_lib.Std
open Hash.Builtin
open Ppx_compare_lib.Builtin

(** Adding code placement information to AST element (for debugging)*)
type 'a placed = {
  (* printer without place annoatation *)
  place: Error.place;[@opaque][@hash.ignore][@compare.ignore][@equal.ignore]
  (*  printer with place annotation:
  place: Error.place;[@printer fun fmt ->let pp_pos fmt ({pos_fname=n1; pos_lnum=l1; pos_bol=b1; pos_cnum=c1}:Lexing.position) = fprintf fmt "{pos_fname=%s; pos_lnum=%d; pos_bol=%d; pos_cnum=%d}" n1 l1 b1 c1 in let pp_place fmt (pos1, pos2) = fprintf fmt "(%a,%a)" pp_pos pos1 pp_pos pos2 in fprintf fmt "%a" pp_place  ]*)
  value: 'a
} [@@deriving show { with_path = false }, yojson, hash, compare, equal]


(* Printing a syntax tree in an intermediate language (for debugging). *)

let print_delimiter () =
  Printf.eprintf "----------------------------------------";
  Printf.eprintf "----------------------------------------\n"

let dump ?(print=(Config.debug ())) ?(json=(Config.json ())) (phase : string) (show : 'term -> string) (to_json : 'term -> Yojson.Safe.t) (t : 'term) =
  if print then ( 
        print_delimiter();
        Printf.eprintf "%s:\n\n%s\n\n%!" phase (show t)
    );  
    if json <> "" then(
        Yojson.Safe.to_file json (to_json t)
    );

    t

let dump_selected (name:string) (phase:string) (show:'term -> string) (to_json : 'term -> Yojson.Safe.t) (t:'term) =
    if Config.debug () then 
        match Config.debug_selector() with
        | None -> dump phase show to_json t
        | Some selected_passes ->
            if List.mem name selected_passes then
                dump phase show to_json t
            else
                t 
    else t

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
[@@deriving show { with_path = false }, yojson, hash, compare, equal]

(** Literal types *)
type flat_type = 
    | TActivationID (** can not be created by programmer -> no constructor *)
    | TBool
    | TInt
    | TLong
    | TUUID
    | TFloat 
    | TRange
    | TStr
    | TLabel
    | TBLabel (** label for non deterministic choices (STBranch/STSelect) only *)
    | TVoid (* literal of type void exists *)
    | TUnit (* not literal exists *)
    | TPlace
    | TSessionID (** can not be created by programmer -> no constructor *)
    | TTimer
    | TWildcard (** e.g. ? *)
    | TBottom (** e.g. object in Java *)
[@@deriving show { with_path = false }, yojson, hash, compare, equal]

type unop = 
    | Not 
    | UnpackOrPropagateResult

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
    | NotEqual
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
    | Array
and block2 =
    | Dict
[@@deriving show { with_path = false }, yojson, hash, compare, equal]


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

type plg_annotation = string (* parsed by plg logic *)

and 'a plg_annotated = {
    plg_annotations: plg_annotation list;
    v: 'a;
}
[@@deriving show { with_path = false }, yojson, hash, compare, equal]


let transparent0_plgannot f place {plg_annotations; v} =
    f place v
let transparent_plgannot f place {plg_annotations; v} =
    {plg_annotations; v = f place v}
let transparent_plgannots f place {plg_annotations; v} =
    List.map (function v -> {plg_annotations; v}) (f place v)
let transparent2_plgannot f place {plg_annotations; v} =
    let env, tmp = f place v in
    env, {plg_annotations; v = tmp}
let transparent2_plgannots ?(f_annot=Fun.id) f place {plg_annotations; v} =
    let env, tmp = f place v in
    env, List.map (function v -> {plg_annotations = f_annot plg_annotations; v}) tmp

let map0_plgannot f place {plg_annotations; v} =
    f place plg_annotations v
let map_plgannot f place {plg_annotations; v} =
    {plg_annotations; v = f place plg_annotations  v}
let map_plgannots f place {plg_annotations; v} =
    List.map (function v -> {plg_annotations; v}) (f place plg_annotations v)
let map2_plgannot f place {plg_annotations; v} =
    let env, tmp = f place plg_annotations  v in
    env, {plg_annotations; v = tmp}
let map2_plgannots ?(f_annot=Fun.id) f place {plg_annotations; v} =
    let env, tmp = f place plg_annotations v in
    env, List.map (function v -> {plg_annotations = f_annot plg_annotations; v}) tmp

let auto_plgannot v = {plg_annotations=[]; v}