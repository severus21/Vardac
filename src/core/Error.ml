open Lexing


type place =
  position * position                

let forge_place filename startpos endpos : place=
  {pos_fname=filename; pos_lnum=1; pos_bol=0; pos_cnum=startpos}, {pos_fname=filename; pos_lnum=1; pos_bol=0; pos_cnum=endpos}

let place lexbuf : place =
  lexbuf.lex_start_p, lexbuf.lex_curr_p

let line p : int =
  p.pos_lnum

let column p : int =
  p.pos_cnum - p.pos_bol

let get_line place : string =
  let startp, _ = place in
  let l = (line startp) in   
  let tmp = ref "" in  
  let aux chan = 
        let i= ref 0 in
          while !tmp = "" do
            incr i;
            let line = input_line chan in
                if !i == l then tmp := line
          done;
          close_in chan
    in
  
  Utils.with_open_in startp.pos_fname aux;
  !tmp

let show place : string =
  let startp, endp = place in
  Printf.sprintf "\027[1mFile \"%s\", line %d, characters %d-%d:\027[0m\n\027[1;31m%s\027[0m"
    startp.pos_fname
    (line startp)
    (column startp)
    (endp.pos_cnum - startp.pos_bol) (* intentionally [startp.pos_bol] *)
    (get_line place)

let display continuation header place format =
  Printf.fprintf stderr "%s:\n" (show place);
  Printf.kfprintf
    continuation
    stderr
    (header ^^ format ^^ "\n%!")

let error place format =
  display
    (fun _ -> exit 1)
    "Error: "
    place format

exception SyntaxError of place 
exception DeadbranchError of string 

let error_of_syntax_error = function
| SyntaxError p -> error p "Syntax error : unable to parse!"
| e -> raise e



let set_filename lexbuf filename =
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename }

let pp_place formatter _place =
  Format.fprintf formatter "<>"
