open OUnit2
open Place  


let suite_places = [
    ("test1", "tests/data/place_test/test1.yml", ());
]

let unittests ()=
  "CookPlace" >::: [TUtils.make_suite "Cook_place" (fun () -> "") (function tfilename-> begin
    let _= tfilename |> ParsePlace.read_place |> ParsePlace.parse_place tfilename "top_level" |>  CookPlace.cook_place  in () (*since we check only that there is no error detected*) (*TODO check that the env works as wanted*)
    end) (function x -> x) suite_places;

  ]
