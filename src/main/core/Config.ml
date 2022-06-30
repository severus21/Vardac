let version = "0.0.0"

let _debug =
  ref false

let debug = function () ->
  Printexc.record_backtrace true;
  !_debug

(* 
  " " -> nothing
  * -> all pass
  pass1:pass2:..*)
let _debug_selector = ref "*"

(*
  * -> None
  " " -> Some []
  "pass1:pass2" -> Some [pass1; pass2]
*)
let debug_selector () =
    if !_debug_selector = "*" then None
    else if !_debug_selector = " " then Some []
    else 
        Some (String.split_on_char ':' !_debug_selector)

let _author = ref "author"

let author = function () -> !_author

let _project_name = ref "project_name"

let project_name = function () -> !_project_name


type provenance_lvl =
| None
| Medium (*only method/component + impl*)
| Full
[@@deriving enum]
let _provenance_lvl = ref Full
let provenance_lvl () = !_provenance_lvl

(* --skip-dt-strat*)
type detection_strategies = 
| NONE (* no violation detection*)
| LIFETIME (* if not specified, ghost onstratup and ghost ondestroy will be erased *)
| SESSION (* ensure protocol order is fullfiled*)
| METADATA (* allow metadata based contract*)
| ALL

let _keep_ghost = ref false
let keep_ghost = function () -> !_keep_ghost

let default_build_dir = Fpath.add_seg (Fpath.v (Sys.getcwd())) "compiler-build" 


(***************************************)
(* Recall applied compilation pass
    This used to ensure at_most_once_apply guarantee for some pass
    Assuming that name is unique
*)
let applied_passes : (string, unit) Hashtbl.t = Hashtbl.create 32
let passes_history = ref []
let register_pass name =
    Hashtbl.add applied_passes name ();
    passes_history := name::!passes_history;
    ()

let already_applied_pass name = 
    Hashtbl.find_opt applied_passes name <> None

let is_first_apply_pass name = 
    Hashtbl.find_opt applied_passes name = None

