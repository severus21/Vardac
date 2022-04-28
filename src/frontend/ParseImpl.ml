(* -------------------------------------------------------------------------- *)
open Core

(* Reading and parsing a file. *)
let parse filename contents : Ast_impl.program = 
    let lexbuf = Lexing.from_string contents in
    Error.set_filename lexbuf filename;
    try
      ImplParser.entry Lexer.entry lexbuf
    with
    | ImplParser.Error ->
        raise (Error.SyntaxError (Error.place lexbuf))
        (*Error.perror (Error.place lexbuf) "Syntax error."*)

let read filename : Ast_impl.program =
  try
    let contents = Utils.file_get_contents filename in
    parse filename contents  
  with
  | Sys_error msg ->
      prerr_endline msg;
      exit 1