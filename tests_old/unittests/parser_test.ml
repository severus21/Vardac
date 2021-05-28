open OUnit2
open Ast

let forge_place startpos endpos : Error.place = Error.forge_place "" startpos endpos

let suite_expressions = [
    ("int", "1", [{place = forge_place 0 1; value = Int 1}]);
    ("let", "let x = 1 in 2", [{ place = forge_place 0 14; value = Let (NonRecursive, Binder, "x",
      { place = forge_place 8 9; value = (Int 1)}, { place = forge_place 13 14; value = (Int 2)})}]);
    ("app", "a(x)(y)", [{place = forge_place 0 7; value = App ({place = forge_place 0 4;value=App ({place=forge_place 0 1; value=Var "a"}, {place=forge_place 2 3; value=Var "x"})}, {place=forge_place 5 6; value=Var "y"})}]);
]

(*let make_suite name suite =
    name >::: (List.map( function (name, code, ast)->
        name>::function _-> assert_equal ~printer:(Utils.ast_to_str Ast.show_program) (Parse.parse "" code) ast) suite)

let make_suites name suites =
    name >::: (List.map( function (name,suite)->  make_suite name suite)suites)

let unittests ()=
  "Parser" >::: [make_suite "Expression" suite_expressions]*)
    
let unittests () =                          
  "Parser" >::: [TUtils.make_suite "Expression" (Utils.ast_to_str Ast.show_program) (Parse.parse "") (function x -> x) suite_expressions]

