open Lexing

type loc = position * position

type place = loc list 


let forge_place filename startpos endpos : place=
  [ {pos_fname=filename; pos_lnum=1; pos_bol=0; pos_cnum=startpos}, {pos_fname=filename; pos_lnum=1; pos_bol=0; pos_cnum=endpos} ]

let place_to_yojson _ = [%to_yojson: string] "no_place"
let place_of_yojson _ = Ppx_deriving_yojson_runtime.Result.Ok (forge_place "place_of_yo_json" 0 0)

let place lexbuf : place =
  [ lexbuf.lex_start_p, lexbuf.lex_curr_p ] 

let line loc : int =
  loc.pos_lnum

let column loc : int =
  loc.pos_cnum - loc.pos_bol

let get_line loc : string =
  let startp, _ = loc in
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

let show_loc ?display_line:(display_line=true) loc : string =
  let show_line= 
    if display_line  then (
      if (Sys.file_exists (fst loc).pos_fname) then 
        Format.sprintf ":\027[0m\n\t> \027[1;31m%s\027[0m" (get_line loc) 
      else
        ":\027[0m\n\t> \027[1;31mcompilation pass is involved\027[0m"
    ) else (* if the location is abstract or the displaying of the line is disabled *)
      ""
  in
  let startp, endp = loc in
  assert(startp.pos_fname = endp.pos_fname);
  Format.sprintf "\027[1mFile \"%s\", line %d, characters %d-%d%s"
    startp.pos_fname
    (line startp)
    (column startp)
    (endp.pos_cnum - startp.pos_bol) (* intentionally [startp.pos_bol] *)
    show_line
    

let show ?display_line:(display_line=true) place : string =
  List.fold_left (
    fun acc tmp -> 
      acc^"\t"^tmp^"\n"
  ) "Locations:\n" (List.map (show_loc ~display_line:display_line) place)  

let display continuation header place format =
  Format.fprintf Format.err_formatter  "%s:\n" (show place);
  Format.kfprintf
    continuation
    Format.err_formatter 
    (header ^^ format ^^ "\n%!")

let perror place format =
  display
    (fun _ -> exit 1)
    "Error: "
    place format

  let error format = 
    Format.kfprintf
    (fun _ -> exit 1)
      Format.err_formatter 
      ("Error: " ^^ format ^^ "\n%!")

let plog_warning (logger:Easy_logging.Logging.logger) place (format:('a, unit, string, unit) format4) = 
  logger#warning "%s\n" (show place);
  logger#warning format 

exception SyntaxError of place 
exception DeadbranchError of string 
exception PlacedDeadbranchError of place * string 

let error_of_syntax_error = function
| SyntaxError p -> perror p "Syntax error : unable to parse!"
| PlacedDeadbranchError (p,msg) -> perror p "%s" msg
| e -> raise e



let set_filename lexbuf filename =
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename }

let pp_place formatter _place =
  Format.fprintf formatter "<>"

let pp_list sep ppx out l = 
    let pp_sep out () = Format.fprintf out "%s@ " sep in
    Format.pp_print_list ~pp_sep ppx out l

let show_list sep ppx l = 
  Format.fprintf Format.str_formatter "%a" (pp_list "," ppx) l;
  Format.flush_str_formatter ()