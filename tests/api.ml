(* *)
open Core
open OUnit2
open Core.Testutils

open IR
open AstUtils
open IRMisc

let fplace = (Error.forge_place "Tests.API" 0 0) 
let auto_fplace smth = {place = fplace; value=smth}
include AstUtils2.Mtype.Make(struct let fplace = fplace end)

(* Core: IRMisc *)
let irmisc_suite () = [
    "dual" >:: (function _ -> assert_equal ~printer:show_session_type (dual (auto_fplace STEnd)) (auto_fplace STEnd));
    "stages_of_st" >:: (function _ -> 
        let st = auto_fplace (STSend( mtype_of_ft TInt, auto_fplace (STSend (mtype_of_ft TInt, auto_fplace STEnd)))) in
        assert_equal  
            (stages_of_st st)
            [
                st.value;
                STSend (mtype_of_ft TInt, auto_fplace STEnd)
            ]);
]

let irmisc_error_suite () = [

]


(**********************************************************************)
let coverage_suite () = 
    List.flatten (List.map (function f -> f ()) [
        irmisc_suite;
    ])

let error_coverage_suite () = 
    List.flatten (List.map (function f -> f ()) [
        irmisc_error_suite;
    ])

let expected_suite () = [
]

let unittests () =
    "API" >::: (List.flatten [
        (coverage_suite ());
        (error_coverage_suite ());
        (expected_suite ());
    ])