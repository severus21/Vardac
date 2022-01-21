open Core
open OUnit2

exception SyntaxError

let rewrite_error f arg =
    try f(arg) with
    | Core.Error.SyntaxError _ -> raise SyntaxError

let forge_place startpos endpos : Error.place = Error.forge_place "" startpos endpos

(* Check error freedom only *)
let make_coverage_suite name fct1 suite = 
    name >::: (List.map( function (name, code)->
        name>::function _-> (ignore (fct1 code))) suite)

(* Check error freedom + expected outputs *)
let make_expected_suite name printer fct1 fct2 suite =
    name >::: (List.map( function (name, code, ast)->
        name>::function _-> assert_equal ~printer:printer (fct1 code) (fct2 ast)) suite)

(* Check error presence *)
let make_error_coverage_suite name fct1 suite =
    name >::: (List.map( function (name, code, exn)->
        name>::function _-> assert_raises exn (function () -> ignore(rewrite_error fct1 code))) suite)
