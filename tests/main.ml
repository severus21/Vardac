let long =
    ref false
  
  let options =
    Arg.align [
      "--long", Arg.Set long, "Run long testsuits, otherwith run the critical subset of tests (design to run in less than 10s).";
    ]
  
  let usage =
    Printf.sprintf "Usage: %s <options> <filename>" Sys.argv.(0)
  
  let () =
    Arg.parse options (fun _ -> ()) usage (* discards any non label arguments *)
  
  
  let () =
      let long_tests =  OUnit2.test_list [
        Example.unittests ();
      ] in
      let tests = OUnit2.test_list [
          Parser.unittests ();
      ] in
  
      OUnit2.run_test_tt_main tests;
      if !long then  OUnit2.run_test_tt_main long_tests
  ;;
  