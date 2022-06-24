{

open Lexing
open Core.Error
open Tokens

open Ast 

let string_of_chars chars = 
    String.of_seq (List.to_seq chars)
    (* or, FIXME i don't know what is the best in term of performances
    let buf = Buffer.create (List.length chars) in
    List.iter (Buffer.add_char buf) chars;
    Buffer.contents buf
    *)
let chars_of_string s =
    let rec exp i l =
        if i < 0 then l else exp (i - 1) (s.[i] :: l) in
    exp (String.length s - 1) []

let any_regexp = Str.regexp "^<[a-zA-Z][a-zA-Z0-9]*>$" 
let any_rec_regexp = Str.regexp "^<<[a-zA-Z][a-zA-Z0-9]**>>$"
let node_regexp = Str.regexp "^[a-zA-Z][a-zA-Z0-9]*$" 

let parse_label_node place = function
    | "*" -> Core.Label.AnyNode None 
    | "**" -> Core.Label.AnyRecNode None 
    | x when (Str.string_match any_regexp x 0) ->
        Core.Label.AnyNode (Some (String.sub x 1 ((String.length x)-2)))
    | x when (Str.string_match any_rec_regexp x 0) -> 
        Core.Label.AnyRecNode (Some (String.sub x 2 ((String.length x)-4)))
    | x when (Str.string_match node_regexp x 0) ->  Core.Label.StringNode x
    | _ -> raise (SyntaxError place)

let keywords = Hashtbl.create 128 
let () = List.iter (fun (s,t) -> Hashtbl.add keywords s t)
[
    "override", OVERRIDE;

    "implements", IMPLEMENTS;
    "extends", EXTENDS;
]
}

(* --------------------------- Regular expressions -------------------------- *)

let newline =
  ('\010' | '\013' | "\013\010")

let whitespace =
  [ ' ' '\t' ]

let lowercase =
  ['a'-'z' '\223'-'\246' '\248'-'\255' '_']

let uppercase =
  ['A'-'Z' '\192'-'\214' '\216'-'\222']

let identchar =
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '0'-'9']

let str_character = uppercase | lowercase | [':' '.' ';' '/' '-' ' ' '\n' '<' '>' '{' '}' '_'] |['0'-'9']
 

let digit =
  ['0'-'9']

let frac = '.' digit*

let exp = ['e' 'E'] ['-' '+']? digit+

let float = "-"? digit+ (frac|exp|frac exp)

(* -------------------------------- Keywords ------------------------------- *)

(* The lexer. *)

rule entry = parse
| ""
    { EMPTYTOKEN}

(* binders *)

| "("
    { LPAREN }
| ")"
    { RPAREN }

| (lowercase identchar *) as s
{ try Hashtbl.find keywords s with Not_found -> LID s }
| (uppercase identchar *) as s
{ try Hashtbl.find keywords s with Not_found -> UID s }
| "\""(str_character* as str)"\"" 
    { STRLITERAL str }
| newline
    { new_line lexbuf; entry lexbuf }
| whitespace+
    { entry lexbuf }
| eof
    { EOF }
| _ as c
    { perror (place lexbuf) "unexpected character: '%c'." c }