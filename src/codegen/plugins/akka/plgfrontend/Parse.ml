(** Parsing a plg_annotations *)
let parse contents : Ast.plg_annotation list = 
    let lexbuf = Lexing.from_string contents in
    try
        Parser.entry Lexer.entry lexbuf
    with
    | Parser.Error ->
        raise (Core.Error.SyntaxError (Core.Error.place lexbuf))