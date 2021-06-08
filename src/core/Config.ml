let version = "0.0.0"

let _debug =
  ref false

let debug = function () ->
  !_debug

let _debug_cook =
  ref false

let debug_cook = function () ->
  !_debug_cook

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