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

let examples () = collect_examples Core.Testutils.example_location

(* Generate tests for an example *)
let testsfrom (name, spec_file, impl_file, targets_file, places_file) : OUnit2.test = 
    name >::: [ 
    ((Printf.sprintf "Glue generation of %s" name) >:: function ctx -> begin 
        let build_dir = OUnit2.bracket_tmpdir ctx in
        ignore (Compspeclib.process_compile (Fpath.v build_dir) places_file targets_file impl_file spec_file);
    end);
    ((Printf.sprintf "Glue compilation of %s" name) >:: function ctx -> begin 
        let build_dir = OUnit2.bracket_tmpdir ctx in
        ignore (Compspeclib.process_compile (Fpath.v build_dir) places_file targets_file impl_file spec_file);
        let code = Sys.command (Printf.sprintf "cd %s/akka && make build > /tmp/varda-test-log-example-%s-generation.log" build_dir name) in
        Printf.fprintf stdout "Code %d\n" code;
        assert_equal 0 code 
    end);
    ((Printf.sprintf "Glue target tests of %s" name) >:: function ctx -> begin 
        let build_dir = OUnit2.bracket_tmpdir ctx in
        ignore (Compspeclib.process_compile (Fpath.v build_dir) places_file targets_file impl_file spec_file);
        let code = Sys.command (Printf.sprintf "cd %s/akka && make test > /tmp/varda-test-log-example-%s-target-tests.log" build_dir name) in
        Printf.fprintf stdout "Code %d\n" code;
        assert_equal 0 code 
    end)
]

let unittests () =
    "Examples" >::: (List.map testsfrom (examples ()))