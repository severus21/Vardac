(* -------------------------------------------------------------------------- *)
open Core
open Utils
open Codegen
open Easy_logging
open Compspeclib

let logger = Logging.make_logger "_1_ compspec" Debug [Cli Debug];;


(* Parse the command line. *)

let filenames = ref []

let impl_filename = ref ""

let places_file = ref ""

let targets_file = ref ""

let action = ref ""

let options = ref []


let set_provenance x = 
    match Config.provenance_lvl_of_enum x with
    | None -> raise (Arg.Bad "Incorrect provenance")
    | Some lvl -> Config._provenance_lvl:=lvl

let common_options = 
[
    "--debug", Arg.Set Config._debug, " Enable debugging output";
    "--debug-cook", Arg.Set Config._debug_cook, "Print debug for cook pass";
    "--places", Arg.Set_string places_file, "Load a YAML file describing the places"; 
    "--targets", Arg.Set_string targets_file, "Load a YAML file describing the targets";
    "--filename", Arg.String (function x-> filenames := x::!filenames), "Spec file to compile";
    "--provenance", Arg.Int set_provenance, "Select how provenance information should be propagated; 0: None; 1: Medium; 2:Full";
]

let options_compile = 
    Arg.align common_options @ 
    [
        "--impl", Arg.Set_string impl_filename, "Impl file"
    ]

let options_check = common_options


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
| "check" -> options := options_compile
| "info"    -> options := options_info 
| _ ->  raise (Arg.Bad "This action is undefined, allowed actions are [compile, info]")

let usage =
    Printf.sprintf "Usage: <action> %s <options> <filename>" Sys.argv.(0)

let () =
    Arg.parse_dynamic options record usage


let filenames =
    List.rev !filenames

(* -------------------------------------------------------------------------- *)

(* The main program. *)
let () =
    match !action with
    | "check" -> begin
        try
            List.iter (process_check !places_file) filenames
        with
        | (Core.Error.SyntaxError _ as e) | (Core.Error.PlacedDeadbranchError _ as e)-> Core.Error.error_of_syntax_error e
    end
    | "compile" -> begin
        try
            List.iter (process_compile !places_file !targets_file !impl_filename) filenames
        with
        | (Core.Error.SyntaxError _ as e) | (Core.Error.PlacedDeadbranchError _ as e)-> Core.Error.error_of_syntax_error e
    end
    | "info" -> begin 
        Printf.fprintf stdout "Version: %s\n" Config.version;

        if !display_build_info then 
            Printf.fprintf stdout "Commit: %s Built on: %s\n" Build.git_revision Build.build_time;

        if !list_codegen_plg then Codegen.display_available_plugins ();
        if !list_check_plg then Check.display_available_plugins ();
        ()
    end

