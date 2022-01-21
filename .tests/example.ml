(*
    Pick each examples and run the various functional testing (one per stage):
        - should be parsed without errors ( r*.spec, *.impl, targets.yml and places.yml)
        - should be type checked without errors
        - (future work) should be verified without errors (*TODO FIXME*)
        - the glue should be generated without errors
        - the glue should compiles without errors
        - the (generated) tests of the glue should be run without errors
        - (future work) the glue should run without errors -> run docker testbench (*TODO FIXME*)

    Side tools:
        - check that sltopology.dot is present and can render to .png
    
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
        Printf.fprintf stdout "Loading example %s\n" dirname;
        Filename.basename dirname,
        (let [f] = Core.Utils.scandir dirname ".spec" in f),
        (let [f] = Core.Utils.scandir dirname ".impl" in f),
        Filename.concat dirname "targets.yml",
        Filename.concat dirname "places.yml"
    ) (find_examples f)


let examples () = collect_examples "examples"

(* Generate tests for an example *)
let testsfrom (name, spec_file, impl_file, targets_file, places_file) : OUnit2.test = 
    (* TODO intermediate passes *)
    name >::: [ 
    (* Parsing *)
    ((Printf.sprintf "parsing_%s" name) >:: function _ -> ignore (Frontend.Parse.parse "" (Core.Utils.file_get_contents spec_file)));
    ((Printf.sprintf "parsing_impl_%s" name) >:: function _ -> ignore (Frontend.ParseImpl.read impl_file));
    ((Printf.sprintf "parsing_places_%s" name) >:: function _ -> ignore (Frontend.process_place places_file));
    ((Printf.sprintf "parsing_targets_%s" name) >:: function _ -> ignore (Frontend.ParseTarget.parse_targets targets_file));
    (* Check *)
    (*((Printf.sprintf "check_%s" name) >:: function ctx -> ignore (Compspeclib.process_check (Fpath.v (OUnit2.bracket_tmpdir ctx)) places_file spec_file));
    (* Codegen *)
    ((Printf.sprintf "codegen_%s" name) >:: function ctx -> 
        ignore (Compspeclib.process_compile (Fpath.v (OUnit2.bracket_tmpdir ctx)) places_file targets_file impl_file spec_file)
    );
    *)
    ((Printf.sprintf "glu compilation%s" name) >:: function ctx -> begin 
        let build_dir = OUnit2.bracket_tmpdir ctx in
        ignore (Compspeclib.process_compile (Fpath.v build_dir) places_file targets_file impl_file spec_file);
        let code = Sys.command ("cd "^build_dir^"/akka && make build > /tmp/log.log") in
        Printf.fprintf stdout "Code %d\n" code;
        assert_equal 0 code 
    end);
    ((Printf.sprintf "glu test%s" name) >:: function ctx -> begin 
        let build_dir = OUnit2.bracket_tmpdir ctx in
        ignore (Compspeclib.process_compile (Fpath.v build_dir) places_file targets_file impl_file spec_file);
        let code = Sys.command ("cd "^build_dir^"/akka && make test > /tmp/log.log") in
        Printf.fprintf stdout "Code %d\n" code;
        assert_equal 0 code 
    end)
]

let unittests () =
    "Examples" >::: (List.map testsfrom (examples ()))