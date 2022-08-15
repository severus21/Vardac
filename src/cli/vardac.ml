(* -------------------------------------------------------------------------- *)
(* Parse the command line. *)

let filenames = ref []

let impl_filenames = ref [] 

let places_file = ref ""

let targets_file = ref ""

let action = ref ""

let options = ref []

let build_dir = ref Core.Config.default_build_dir

let set_provenance x = 
    match Core.Config.provenance_lvl_of_enum x with
    | None -> raise (Arg.Bad "Incorrect provenance")
    | Some lvl -> Core.Config._provenance_lvl:=lvl

let common_options = 
[
    "--debug", Arg.Set Core.Config._debug, " Enable debugging output";
    "--debug-selector", Arg.Set_string Core.Config._debug_selector, "Print debug for selected pass e.g. pass name1:pass name2";
    "--places", Arg.Set_string places_file, "Load a YAML file describing the places"; 
    "--targets", Arg.Set_string targets_file, "Load a YAML file describing the targets";
    "--filename", Arg.String (function x-> filenames := x::!filenames), "Spec file to compile";
    "--provenance", Arg.Int set_provenance, "Select how provenance information should be propagated; 0: None; 1: Medium; 2:Full";
    "-o", Arg.String (function dir -> build_dir := Fpath.v dir), Printf.sprintf "Specify the build directory, by default %s" (Fpath.to_string Core.Config.default_build_dir)
]

let options_compile = 
    Arg.align common_options @ 
    [

        "--impl", Arg.String (function x-> impl_filenames := x::!impl_filenames), "Impl file";
    ]

let options_check = common_options
let options_stats = 
    common_options @ 
    [
        "--impl", Arg.String (function x-> impl_filenames := x::!impl_filenames), "Impl file";
    ]


let display_build_info = ref false
let list_codegen_plg = ref false
let list_check_plg = ref false
let options_info = 
    Arg.align [
        "--codegen-plgs", Arg.Set list_codegen_plg, "Display available plugins for codegeneration";
        "--check-plgs", Arg.Set list_check_plg, "Display available plugins for verification";
        "--build", Arg.Set display_build_info , "Build information";
    ]

let record a= 
action := a;
match !action with
| "compile" -> options := options_compile
| "check"   -> options := options_compile
| "info"    -> options := options_info 
| "stats"   -> options := options_stats
| _ ->  raise (Arg.Bad "This action is undefined, allowed actions are [compile, info]")

let usage =
    Printf.sprintf "Usage: <action> %s <options> <filename>" Sys.argv.(0)

let () =
    Printexc.record_backtrace false;
    Arg.parse_dynamic options record usage


let filenames =
    List.rev !filenames

(* -------------------------------------------------------------------------- *)

(* The main program. *)
let () =
    (* Setup logging 
        * define a Cli handler for all logging event
    *)
    let _ = Easy_logging.Logging.make_logger "vardac" Debug [Cli Debug] in

    let module Vardaclib = Vardaclib.Make() in

    begin
        match !action with
        | "check" -> begin
            try
                List.iter (Vardaclib.process_check !build_dir !places_file) filenames
            with
            | (Core.Error.SyntaxError _ as e) | (Core.Error.PlacedDeadbranchError _ as e)-> Core.Error.error_of_syntax_error e
        end
        | "compile" -> begin
            try
                List.iter (Vardaclib.process_compile !build_dir !places_file !targets_file !impl_filenames) filenames
            with
            | (Core.Error.SyntaxError _ as e) | (Core.Error.PlacedDeadbranchError _ as e)-> Core.Error.error_of_syntax_error e
        end
        | "stats" -> begin
            try
                List.iter (Vardaclib.process_stats !places_file !targets_file !impl_filenames) filenames
            with
            | (Core.Error.SyntaxError _ as e) | (Core.Error.PlacedDeadbranchError _ as e)-> Core.Error.error_of_syntax_error e
        end
        | "info" -> begin 
            Printf.fprintf stdout "Version: %s\n" Core.Config.version;

            if !display_build_info then 
                Printf.fprintf stdout "Commit: %s Built on: %s\n" Build.git_revision Build.build_time;

            if !list_codegen_plg then Codegen.display_available_plugins ();
            if !list_check_plg then Check.display_available_plugins ();
            ()
        end
        | _ -> Printf.fprintf stderr "Unknown action [%s]" !action
    end;

    if Core.Config.debug () then
        Yojson.Safe.to_file "logging_tree.json" (Easy_logging.Logging.tree_to_yojson ())
    else
        ()

