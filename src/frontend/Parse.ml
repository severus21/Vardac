(* -------------------------------------------------------------------------- *)
open Core

(* Reading and parsing a file. *)
let parse filename contents : Ast.program = 
    let lexbuf = Lexing.from_string contents in
    Error.set_filename lexbuf filename;
    try
        Parser.entry Lexer.entry lexbuf
    with
    | Parser.Error ->
        raise (Error.SyntaxError (Error.place lexbuf))
        (*Error.error (Error.place lexbuf) "Syntax error."*)

let parse_expr filename contents : Ast.expr = 
    let lexbuf = Lexing.from_string contents in
    Error.set_filename lexbuf filename;
    try
        Parser.entry_expr Lexer.entry lexbuf
    with
    | Parser.Error ->
        raise (Error.SyntaxError (Error.place lexbuf))
        (*Error.error (Error.place lexbuf) "Syntax error."*)

let read filename : Ast.program =
    try
        let contents = Utils.file_get_contents filename in
        parse filename contents  
    with
    | Sys_error msg ->
        prerr_endline msg;
        exit 1