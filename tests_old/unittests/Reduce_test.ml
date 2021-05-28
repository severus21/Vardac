open OUnit2
open ResolvedAst  


let forge_place startpos endpos : Error.place = Error.forge_place "" startpos endpos

let include_suite = [
    ("test1", "#include \"tests/data/resolve_test/std.abs\"", [{ place =
   ({pos_fname="tests/data/resolve_test/std.abs"; pos_lnum=1; pos_bol=0; pos_cnum=0},{pos_fname="tests/data/resolve_test/std.abs"; pos_lnum=1; pos_bol=0; pos_cnum=13});
   value =
   (Let (NonRecursive, Binder, "std_x",
      { place =
        ({pos_fname="tests/data/resolve_test/std.abs"; pos_lnum=1; pos_bol=0; pos_cnum=12},{pos_fname="tests/data/resolve_test/std.abs"; pos_lnum=1; pos_bol=0; pos_cnum=13});
        value = (Int 0) },
      { place =
        ({pos_fname="tests/data/resolve_test/std.abs"; pos_lnum=1; pos_bol=0; pos_cnum=0},{pos_fname="tests/data/resolve_test/std.abs"; pos_lnum=1; pos_bol=0; pos_cnum=13});
        value = Exit }
      ))
   }
  ]
);
]

let unittests ()=
  "Resolve" >::: [TUtils.make_suite "include" (Utils.ast_to_str ResolvedAst.show_program) ( function x -> Parse.parse "" x |> Resolve.resolve_program) (function x -> x) include_suite;

  ]

