open OUnit2
open RawPlace  


let suite_places = [
    ("test1", "tests/data/place_test/test1.yml", (Root
   [ (Place ("place1", (Some { addr = "127.0.0.1"; port = 14290 }),
       (Some { cpus = 3 }), []));
     (Place ("node4", None, None, []));
     (Place ("Cloud", None, None,
        [ (Place ("dc1", None, None, [
            (Var "place1"); 
            (Var "node4")
          ]));
          (Place ("dc2", None, None, []))]
     ));
     (Place ("Edge", None, None, [
       (Place ("node1026", None, None, []));
       (Place ("node1027", None, None, []))]))]));
]

let unittests ()=
  "ParsePlace" >::: [TUtils.make_suite "Cook_place" (RawPlace.show_place) (function tfilename-> ParsePlace.parse_place "" "top_level" (Rresult.R.get_ok (Yaml_unix.of_file Fpath.(v tfilename)))) (function x -> x) suite_places;

  ]

