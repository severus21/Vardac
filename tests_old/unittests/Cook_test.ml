open OUnit2
open IR  


let encapsulation_suite = [
    ("test1", ("tests/data/cook_test/test1.abs", "tests/data/cook_test/test1.yml"), ());
]

let process_code places filename : IR.program =
  filename
  |> Parse.read
  |> Resolve.resolve_program   
  |> Cook.cook_program places

let process_place (filename:string) : Place.place=
  filename
  |> ParsePlace.read_place
  |> ParsePlace.parse_place filename "top_level"
  |> CookPlace.cook_place   

let process (cfilename, pfilename)=
  let places = process_place pfilename in
  let _ = List.map (process_code places) [cfilename] in ()

let unittests ()=
  "Cook" >::: [TUtils.make_suite "Encapsulation testing" (fun () -> "") process (function x -> x) encapsulation_suite;

  ]
