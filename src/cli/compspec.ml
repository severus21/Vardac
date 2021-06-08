(* -------------------------------------------------------------------------- *)
open Core
open Utils
open Codegen
open Easy_logging

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

let options_info = 
    Arg.align [
        "--codegen-plgs", Arg.Unit (function () -> Codegen.display_available_plugins ()), "Display available plugins for codegeneration";
        "--debug", Arg.Set Config._debug, " Enable debugging output";
        "--version", Arg.Unit (function () -> Printf.fprintf stdout "Version: %s\n" Config.version), " Enable debugging output";
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

let build_dir : Fpath.t = 
    let default = Fpath.add_seg (Fpath.v (Sys.getcwd())) "compiler-build" in
    match Bos.OS.Dir.delete ~must_exist:false ~recurse:true default with
    | Rresult.Error _ -> failwith "build_dir cleansing failure"
    | _ -> ();

    match Bos.OS.Dir.create default with 
    | Rresult.Ok _ -> default
    | _ -> failwith "build_dir failed"

(* -------------------------------------------------------------------------- *)
let process_impl filename =
    Frontend.Main.to_impl filename

let process places targets_file impl_filename filename = 
    (* Prepare dir *)
    let build_dir = "/" :: Fpath.segs build_dir in
    Printf.eprintf "Target dir is \"%s\":\n" (FilePath.string_of_path build_dir);

    Frontend.Main.to_ir places filename 
    |> Core.PartialEval.peval_program 
    |> function x-> logger#sinfo "IR has been partially evaluated";x
    |> Core.AstUtils.dump "pevaled IR" IR.show_program 
    |> Frontend.Main.to_impl impl_filename  
    |> Codegen.codegen build_dir targets_file
    (*let out_ml = open_out (FilePath.make_filename (build_dir @ ["main.ml"])) in
    to_ocaml out_ml ir;*)


(* -------------------------------------------------------------------------- *)
(* Sanitize libs *)

(*let sanitize_libs libs=
  let res = List.map (function x -> Sys.file_exists x,x) libs in
  List.iter (function false,x -> Printf.eprintf "Library %s not found." x; exit 1  |true,_ -> ()) res  
 *)


(* -------------------------------------------------------------------------- *)

(* The main program. *)
let () =
    match !action with
    | "check" -> begin
        (* TODO get ride of the impl part here *)
        try
            let places = Frontend.Main.process_place !places_file in
            List.iter (process places !targets_file !impl_filename) filenames
        with
        | Core.Error.SyntaxError _ as e -> Core.Error.error_of_syntax_error e
    end
    | "compile" -> begin
        try
            let places = Frontend.Main.process_place !places_file in
            List.iter (process places !targets_file !impl_filename) filenames
        with
        | Core.Error.SyntaxError _ as e -> Core.Error.error_of_syntax_error e
    end
    | "info" -> ()

