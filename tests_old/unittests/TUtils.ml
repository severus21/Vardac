open OUnit2

let make_suite name printer fct1 fct2 suite =
    name >::: (List.map( function (name, code, ast)->
        name>::function _-> assert_equal ~printer:printer (fct1 code) (fct2 ast)) suite)
