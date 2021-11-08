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
    (* type *)
    "event", EVENT;
    "of", OF;
    "protocol", PROTOCOL;
    "type", TYPE;
    "vplacedef", VPLACEDEF;

    (* binders *)
    "in", IN;

    (* Control-flow *)
    "break", BREAK;
    "case", CASE;
    "continue", CONTINUE;
    "else", ELSE;
    "exist", EXIT;
    "for", FOR;
    "if", IF;
    "match", MATCH;
    "return", RETURN;

    (* component *)
    "component", COMPONENT;
    "contract", CONTRACT;
    "expecting", EXPECTING;
    "method", METHOD;
    "mutation", MUTATION;
    "on", ON;
    "port", PORT;
    "sig", SIG;
    "signature", SIGNATURE;
    "timer", TIMER;

    (* Expr *)
    "Err", ERR;
    "None", NONE;
    "Ok", OK;
    "Some", SOME;

    (* *)
    "function", FUNCTION;

    (* XXX *)
    "ghost", GHOST;

    (* Contract *)
    "and", AND;
    "contract", CONTRACT;
    "ensures", ENSURES;
    "invariant", INVARIANT;
    "metadata", METADATA;
    "returns", RETURNS;
    "with", WITH;

    (* lifetime keywords *)
    "this", THIS;
    "spawn", SPAWN;
    "onstartup", ONSTARTUP;
    "ondestroy", ONDESTROY;

    (* Primtive types *)
    "bool", PTYPE TBool;
    "int", PTYPE TInt;
    "float", PTYPE TFloat;
    "string", PTYPE TStr;
    "label", PTYPE TLabel;
    "void", PTYPE TVoid;
    "place", PTYPE TPlace;


    (* Preprocessor *)
    "use", USE;
    "inline", INLINE;

    (* State kind*)
    (* global keyword is already defined *)
    "local",  LOCAL;
    "shared",  SHARED;

    (* Implem lexer *)
    "impl", IMPL;
    "target", TARGET;
    "template", TEMPLATE;
    "where", WHERE;
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

| "&&"
    { BINOP And}
| "=="
    { BINOP Equal}
| "not" 
    { UNOP Not}
| "+"
    { BINOP Plus }
| "-"
    { BINOP Minus }
| "*"
    { BINOP Mult }
| "/"
    { BINOP Divide }


(* session types *)
| "!" 
    {BANG}
| "?"
    {RECV}
| "&"
    {BRANCH}
| "§"
    {SELECT}
| "µ"
    {RECST}
| "session"
    {SESSION}
| "timeout"
    {TIMEOUT}

(* binders *)

| "="
    { EQ }
| "("
    { LPAREN }
| ")"
    { RPAREN }
| "."
    { DOT }

(* arrows *)
| "->"
    {SIMPLE_RARROW}
| "=>"
    {DOUBLE_RARROW}

(* miscellaneous *)
| "<"
    { LANGLEBRACKET }
| ">"
    { RANGLEBRACKET }
| "["
    { LBRACKET }
| "]"
    { RBRACKET }
| ";"
    { SEMICOLON }
| ","
    { COMMA }
| "{"
    {LCURLYBRACKET}
| "}"
    {RCURLYBRACKET}
| "|"
    {MID}
| "::" 
    {DOUBLE_COLON}
| ":" 
    {COLON}

(* Impl *)
| "{="
    { BLACKBOX_BODY (string_of_chars(blackbox_body (place lexbuf) lexbuf)) }

(* Quotes *)
| "\'"
    {SIMPLE_QUOTE}

(* Placement keywords*)
| "@"
    { AT }

|  "true" as b | "false" as b 
    { try
        BOOLLITERAL (bool_of_string b)
      with Failure _ ->
        error (place lexbuf) "invalid bool literal." }
| (lowercase identchar *) as s
{ try Hashtbl.find keywords s with Not_found -> LID s }
| (uppercase identchar *) as s
{ try Hashtbl.find keywords s with Not_found -> UID s }
|  float  as f
    { try
        FLOATLITERAL (float_of_string f)
      with Failure _ ->
        error (place lexbuf) "invalid float literal." }
| "-"? digit+ as i
    { try
        INTLITERAL (int_of_string i)
      with Failure _ ->
        error (place lexbuf) "invalid integer literal." }
| (lowercase identchar *)("."(lowercase identchar *))+ as x
    { ATTR (String.split_on_char '.' x)}
| "\""(str_character* as str)"\"" 
    { STRLITERAL str }
| "l\""((str_character|'*'|'<'|'>')* as str)"\"" 
    { LABEL_LITERAL (List.map (parse_label_node (place lexbuf))  (Str.split (Str.regexp "::") str)) }
| "(*"
    { ocamlcomment (place lexbuf) lexbuf; entry lexbuf }
(* Comments that should be propagated to codegen*)
| "//"
    { COMMENTS (LineComment (string_of_chars(aspeconelinecomment (place lexbuf) lexbuf))) }
| "/*"
    { COMMENTS (BlockComment (string_of_chars(aspeccomment (place lexbuf) lexbuf)))}
| "/**"
    { COMMENTS (DocComment (string_of_chars(aspeccomment (place lexbuf) lexbuf))) }
| newline
    { new_line lexbuf; entry lexbuf }
| whitespace+
    { entry lexbuf }
| eof
    { EOF }
| _ as c
    { error (place lexbuf) "unexpected character: '%c'." c }


and aspeconelinecomment p = parse
| newline
    { new_line lexbuf; [] }
| eof
    { [] }
| _ as x
    { x::(aspeconelinecomment p lexbuf) }
and aspeccomment p = parse
| "*/"
    { [] }
| "/*"
    { (aspeccomment (place lexbuf) lexbuf) @ (aspeccomment p lexbuf) } (* handle nested comments *)
| newline as x
    { new_line lexbuf; (chars_of_string x)@(aspeccomment p lexbuf) }
| eof
    { error p "unterminated comment." }
| _ as x
    { x::(aspeccomment p lexbuf) }
and blackbox_body p = parse
| "=}"
    { [] }
| newline as x
    { new_line lexbuf; (chars_of_string x)@(blackbox_body p lexbuf) }
| eof
    { error p "unterminated comment." }
| _ as x
    { x::(blackbox_body p lexbuf) }


(* ------------------------------------------------------------------------ *)

  (* Skip OCaml-style comments. Comments can be nested. This sub-lexer is
   parameterized with the place of the opening comment, so if an unterminated
   comment is detected, we can show where it was opened. *)

and ocamlcomment p = parse
| "*)"
    { () }
| "(*"
    { ocamlcomment (place lexbuf) lexbuf; ocamlcomment p lexbuf }
| newline
    { new_line lexbuf; ocamlcomment p lexbuf }
| eof
    { error p "unterminated comment." }
| _
    { ocamlcomment p lexbuf }
