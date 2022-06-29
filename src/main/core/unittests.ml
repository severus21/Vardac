
open OUnit2
open Testutils
open AstUtils
open IR

let fplace = (Error.forge_place "Main.Unittests" 0 0) 
let auto_fplace smth = {place = fplace; value=smth}
include AstUtils2.Mtype.Make(struct let fplace = fplace end)


(************************* Utils - misc ******************************)
let ftvars_of terms = List.map snd (snd (free_vars_program Atom.Set.empty terms))
let to_hints atoms = List.map Atom.hint atoms

let a = Atom.fresh "a"
let b = Atom.fresh "b"
let c = Atom.fresh "c"
let d = Atom.fresh "d"

let ftvars_suite () = [
    "ftvars" >:: (function ctw ->
        let terms = [
            Typedef (auto_fplace(EventDef (a, [], ())));
            Typedef (auto_fplace(EventDef (b, [], ())));
            Typedef (auto_fplace(EventDef (c, [mtype_of_var a; mtype_of_var b], ())));
        ] in    
        let terms = List.map (function x -> auto_fplace (auto_plgannot x)) terms in
        let ftvars = to_hints (ftvars_of terms) in

        assert_equal ftvars [];
    );
    "ftvars2" >:: (function ctw ->
        let terms = [
            Typedef (auto_fplace(EventDef (a, [], ())));
            Typedef (auto_fplace(EventDef (c, [mtype_of_var a; mtype_of_var b], ())));
        ] in    
        let terms = List.map (function x -> auto_fplace (auto_plgannot x)) terms in
        let ftvars = to_hints (ftvars_of terms) in

        assert_equal ftvars ["b"]
    );

]
let ftvars_error_suite () = [

]

(**********************************************************************)

let coverage_suite () = 
    List.flatten (List.map (function f -> f ()) [
        ftvars_suite;
    ])

let error_coverage_suite () = 
    List.flatten (List.map (function f -> f ()) [
        ftvars_error_suite;
    ])

let expected_suite () = [
]


let unittests () =
    "Parser" >::: (List.flatten [
        coverage_suite ();
        error_coverage_suite ();
        expected_suite ();
    ])