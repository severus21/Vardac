(*
    Pick each examples and run the various functional testing (one per stage):
        - should be parsed without errors ( r*.spec, *.impl, targets.yml and places.yml)
        - should be type checked without errors
        - (future work) should be verified without errors (*TODO FIXME*)
        - the glue should be generated without errors
        - the glue should compiles without errors
        - (future work) the glue should run without errors -> run docker testbench (*TODO FIXME*)
    
    Notes : ensure no regression + ensure examples are up-to-date
*)

open Core
open OUnit2

(*  Examples/
        examples1/
            main.spec main.impl targets.yml places.yml
            bbla/other*.spec bbla/other*.impl
*)

let find_examples f = 
    Sys.readdir f 
    |> Array.to_list
    |> List.map (Filename.concat f)

let collect_examples f = 
    List.map ( function dirname ->
        Filename.basename dirname,
        (let [f] = Core.Utils.scandir dirname ".spec" in f),
        (let [f] = Core.Utils.scandir dirname ".impl" in f),
        Filename.concat dirname "targets.yml",
        Filename.concat dirname "places.yml"
    ) (find_examples f)


let examples () = collect_examples "examples"
(*
assert(examples () <> []) (* Just to be sure, that collection of examples works *)
*)

(* Generate tests for an example *)
let testsfrom (name, spec_file, impl_file, targets_file, places_file) : OUnit2.test = 
    name >::: [ 
    (* Parsing *)
    (Printf.sprintf "parsing_%s" name) >:: function _ -> ignore (Frontend.Parse.parse "" (Core.Utils.file_get_contents spec_file));
    (*(Printf.sprintf "parsing_impl_%s" name) >:: function _ -> ignore (Frontend.ParseImpl.read impl_file);
    (Printf.sprintf "parsing_places_%s" name) >:: function _ -> ignore (Frontend.Main.process_place places_file);
    (Printf.sprintf "parsing_targets_%s" name) >:: function _ -> ignore (Frontend.ParseTarget.parse_targets targets_file);

    (* TODO intermediate passes *)

    (* Codegen *)
    (Printf.sprintf "parsing_targets_%s" name) >:: (function _ -> ignore (Compspeclib.process_compile places_file targets_file impl_file spec_file));*)
]

let unittests () =
    "Examples" >::: (List.map testsfrom (examples ()))