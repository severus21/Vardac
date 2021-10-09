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