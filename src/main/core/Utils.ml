open Easy_logging
let logger = Logging.make_logger ("_1_ compspec.core") Debug [];;
(* -------------------------------------------------------------------------- *)

(* Printing a syntax tree in an intermediate language (for debugging). *)


let ast_to_str (show : 'term -> string) (t : 'term) =
    Printf.sprintf  "%s:n\n%s\n\n%!" "" (show t)

(* https://github.com/FStarLang/kremlin/blob/master/lib/Utils.ml *)

let try_finally f h = let result =
    try
      f ()
    with e ->
      h ();
      raise e
  in
  h ();
  result

let with_open_in file_path f =
  let c = open_in_bin file_path in
  try_finally (fun () ->
    f c
  ) (fun () ->
    close_in c
  )

let with_open_out_bin file_path f =
  let c = open_out_bin file_path in
  try_finally (fun () ->
    f c
  ) (fun () ->
    close_out c
  )


let cp dst src = with_open_out_bin dst (fun oc ->
  with_open_in src (fun ic ->
    let buf = Bytes.create 2048 in
    while
      let l = input ic buf 0 2048 in
      if l > 0 then begin
        output oc buf 0 l;
        true
      end else
        false
    do () done
  ))


let read ic =
  let buf = Buffer.create 4096 in
  let s = Bytes.create 2048 in
  while begin
    let l = input ic s 0 (Bytes.length s) in
    if l > 0 then begin
      Buffer.add_bytes buf (Bytes.sub s 0 l);
      true
    end else begin
      false
    end
  end do () done;
  Buffer.contents buf

let file_get_contents f =
  with_open_in f read

(** Sniff the size of the terminal for optimal use of the width. *)
let theight, twidth =
  let height, width = ref 0, ref 0 in
  match
    Scanf.sscanf (List.hd (Process.read_stdout "stty" [|"size"|])) "%d %d" (fun h w ->
      height := h;
      width := w);
    !height, !width
  with
  | exception _ ->
      24, 80
  | 0, 0 ->
      24, 80
  | h, w ->
      h, w

let hashtbl_values_to_list t =
Hashtbl.fold (fun _ v l -> if List.mem v l then l else v::l) t []

(* -------------------------------------------------------------------------- *)

(** [scandir] returns the paths of all regular files with the [ext] extension that are
 * contained in [dir]. Each file is a path starting with [dir].
  *)
let scandir path wanted_ext = 
  let rec aux acc = function
  | f::fs when Sys.is_directory f ->
    Sys.readdir f
    |> Array.to_list
    |> List.map (Filename.concat f)
    |> List.append fs
    |> aux acc
  | f::fs when (Filename.extension f = wanted_ext) -> aux (f::acc) fs
  | _::fs -> aux acc fs
  | [] -> acc
  in aux [] [path]

(***************************** Function manipulations *************************)

(** Function composition, TODO put it in some Core file*)
let (<->) f g x = f(g(x));;


(***************************** File manipulations *************************)

let refresh_or_create_dir (build_dir:Fpath.t) = 
    match Bos.OS.Dir.delete ~must_exist:false ~recurse:true build_dir with
    | Rresult.Error _ -> failwith "dir cleansing failure"
    | _ -> ();

    match Bos.OS.Dir.create build_dir with 
    | Rresult.Ok _ -> () 
    | _ -> failwith "dir failed"
  
  
let create_directory_hierarchy (parentdir:Fpath.t): unit = 
    assert( Fpath.is_dir_path parentdir);
    match Bos.OS.Dir.exists parentdir with
    | Rresult.Ok false -> begin
        match Bos.OS.Dir.create parentdir with
        | Rresult.Error _ -> logger#error "can not create dir %s" (Fpath.to_string parentdir); exit 1
        | _ -> () 
        end
    | Rresult.Ok true -> ()
    | Rresult.Error _ -> logger#error "directory exist check fails for %s" (Fpath.to_string parentdir); exit 1

let deduplicate key elts = 
    let keys = Hashtbl.create 32 in
    List.filter (function elt -> 
        let k = key elt in
        match Hashtbl.find_opt keys k with
        | None -> Hashtbl.add keys k (); true 
        | Some _ -> false 
    ) elts

let run_and_collect_stdout cmd : string list = 
    (* TODO use sites *)
    let ic = Unix.open_process_in cmd in
    let all_input = ref [] in
    try
        while true do
            all_input := input_line ic :: !all_input;
        done;
        !all_input
    with
        End_of_file -> close_in ic;

    !all_input