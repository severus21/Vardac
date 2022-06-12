open OUnit2

let () =
    let tests = [
        Api.unittests ();
        
        (* Inner tests*)
        Frontend.unittests ();

        (* External/functional tests*)
        Example.unittests ();
    ] in

    OUnit2.run_test_tt_main (OUnit2.test_list tests);