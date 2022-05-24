open OUnit2
open Core.Testutils


(* TODO FIXME 
Tests for *.spec
Coverage suite for
    - types
    - expressions + statement
    - component dcl + architecture
    - component expr 
At different lvls:
    1) parser
    2) type checker - check rejection and acception
    3) operational semantics - check acception and rejection


TODO unitests for specific functions unification, free var, renaming, ....

TODO test for *....
*)


(************************* Parsing - misc ******************************)
let misc_suite () = [
    ("comments inline", "//comment");
    ("comments multiline", "/*comment\nt1\nt2*/");
    ("comments doc", "/**comment\nt1\nt2*/");
    ("ghost comments inline", "(*t1*)");
    ("ghost comments multiline", "(*\nt1\n*)");
]
let misc_error_suite () = [

]
(************************* Parsing - types *****************************)
let ct_suite () = [

]
let ct_error_suite () = [

]

let st_suite () = [
    ("timers","type _ = !ping{timer y|}?pong!ping{|(5<y) && (y<100)}!ping{|y<150}.;");
    ("metadata", "type x = !int { metadata int y | x : int -> x > 0 }?float{| x : int -> x > y}.;"); (* TODO add set y *)
]

let st_error_suite () = [

]

let cmt_suite () = [

]

let cmt_error_suite () = [

]

let mt_suite () = [

]

let typedef_suite () = [
    ("type definition abstract", "type begintx;");
    ("type definition value", "type test = int;");
    ("type definition arrow", "type test = int -> int;");
    ("type definition result", "type test = result<int,int>;");
    ("type definition option", "type test = option<int>;");
    ("type definition with nested constraints", "type listc = list<int{|x>0}>{|true};");
    ("type definition st send", "type test = !msg.;");
    ("type definition st recv", "type test = ?msg.;");
    ("type definition st choice", "type test = &{\"get\": !Msg. };");
    ("type definition st select", "type test = §{\"get\": !Msg. };");
    ("type definition st rec", "type test = µ x. !int ?float - x;");
    ("type definition st protocol", Core.Utils.file_get_contents ( dataset_lookup_file "protocol1.spec"));
    ("type definition st protocol with constraints", Core.Utils.file_get_contents ( dataset_lookup_file "protocol1_with_constraints.spec"));
]

let typedef_error_suite () = [
    ("type definition abstract", "type BeginTX",  SyntaxError);
]
(****************** Parsing - expression & statement *******************)
let expr_suite () = [
    ("expr lit bool", "true;");
    ("expr lit float", "0.1;");
    ("expr lit int", "1;");
    ("expr lit str", "\"ab\";");
    ("expr lit str - multiple words", "\"ab cd\";");
    ("expr lit str - multiple lines", "\"ab 
    cd\";");
    ("expr lit label", "l\"a::*::b::**\";");
    ("expr lit label", "l\"a::<u>::b::<<v>>\";");
    ("expr binop add", "1 + 1;");
    ("expr unop not", "not true;");
    ("expr dict noelmt", "dict<int, int> d = {};");
    ("expr dict onelmt", "dict<string, int> d = {\"a\":1};");
    ("expr dict multielmt", "dict<label, int> d = {l\"a\":1, l\"b\":2};");
    ("expr list empty", "list<int> l1 = [];");
    ("expr list onelmt", "list<int> l2 = [1];");
    ("expr list multelmt", "list<int> l3 = [1,2,3];");
    ("expr set empty", "set<int> l1 = {};"); (* TODO fixme with dict it is ambiguous -> can be a dict or set *)
    ("expr set onelmt", "set<int> l2 = {1};");
    ("expr set multelmt", "set<int> l3 = {1,2,3};");
    ("expr option none", "option<int> o1 = none;");
    ("expr option some", "option<int> o2 = some(1);");
    ("expr result err", "result<int, string> r1 = err(\"\");");
    ("expr result ok", "result<int, string> r2 = ok(1);");
    ("expr set empty", "set<int> s1 = {};");
    ("expr set onelmt", "set<int> s2 = {1};");
    ("expr set multielmt", "set<int> s3 = {1,2,3};");
    ("expr tuple onelmt", "tuple<int> t1 = (1,);");
    ("expr tuple multielmt", "tuple<int,int> t2 = (1,2);");
    ("expr app noargs", "f();");
    ("expr app oneargs", "f(10);");
    ("expr app multiargs", "f(10, \"r\");");
    ("expr lambda", "x : int -> x+1;");
    ("expr this", "this;");
    ("expr spawn notplaced", "spawn C(1, \"b\");");
    ("expr spawn placed", "spawn C() @ r;");
    ("expr access", "a.b.c;");
    ("expr with paren", "(1+1) + 4;");
]

let expr_error_suite () = [

]

(* TODO Add builtin tests *)

let stmt_suite () = [
    ("stmt assign", "int x = 1;");
    ("stmt block", "{ int x = 1; return 1; }");
    ("stmt controlflow if ", "if (true) { return 2; } else { return 10; }");
]
let stmt_error_suite () = [
    ("stmt label", "l\"a::b_::b::**\"",  SyntaxError);
    ("stmt controlflow if", "if (true) { 2 } else { return 10; }",  SyntaxError);
]

(************************* Parsing - component dcl *********************)
let cdcl_suite () = [
    ("componentdcl base", "component TransactionManager () {}");
    ("componentdcl abstract method", Core.Utils.file_get_contents ( dataset_lookup_file "cdcl_abstract_method.spec"));
    ("componentdcl method", Core.Utils.file_get_contents (dataset_lookup_file  "cdcl_method.spec"));
    ("componentdcl port", Core.Utils.file_get_contents (dataset_lookup_file  "cdcl_port.spec"));
    ("componentdcl state", Core.Utils.file_get_contents (dataset_lookup_file "cdcl_state.spec"));
    ("componentdcl contract", Core.Utils.file_get_contents (dataset_lookup_file "cdcl_contract.spec"));
]

let cdcl_error_suite () = [

]
(************************* Parsing - architecture **********************)
(************************* Parsing - component expr ********************)
let cexpr_suite () = [
    ("cexpr base", Core.Utils.file_get_contents (dataset_lookup_file "cexpr_base.spec"));
]
let cexpr_error_suite () = [

]


(************************* Parsing - placement ***************)
let placement_suite () = [

]
let placement_error_suite () = [

]

(************************* Parsing - term ********************)
let term_suite () = [
    ("ppterm use", "use a;");
    ("ppterm use", "use a.b.c;");
    ("function declaration no args", "void fct() { return (); }");
    ("function declaration one args", "void fct(int i) { i+1; return (); }");
    ("function declaration multiple args", "void fct(int i, string j, list<int> k) { i+1; return (); }");
    ("abstract function declaration", "void fct();");
]



let term_error_suite () = [

]

let coverage_suite () = 
    List.flatten (List.map (function f -> f ()) [
        misc_suite; ct_suite; st_suite; cmt_suite; typedef_suite; expr_suite;
        stmt_suite; cdcl_suite; cexpr_suite; placement_suite; term_suite;
    ])

let error_coverage_suite () = 
    List.flatten (List.map (function f -> f ()) [
        misc_error_suite; ct_error_suite; st_error_suite; cmt_error_suite; typedef_error_suite; expr_error_suite;
        stmt_error_suite; cdcl_error_suite; cexpr_error_suite; placement_error_suite; term_error_suite;
    ])

let expected_suite () = [
(*    ("int", "1", [{place = forge_place 0 1; value = Int 1}]);
    ("let", "let x = 1 in 2", [{ place = forge_place 0 14; value = Let (NonRecursive, Binder, "x",
      { place = forge_place 8 9; value = (Int 1)}, { place = forge_place 13 14; value = (Int 2)})}]);
    ("app", "a(x)(y)", [{place = forge_place 0 7; value = App ({place = forge_place 0 4;value=App ({place=forge_place 0 1; value=Var "a"}, {place=forge_place 2 3; value=Var "x"})}, {place=forge_place 5 6; value=Var "y"})}]);*)
]


let unittests () =
    "Parser" >::: [
         make_coverage_suite "parsing_coverage" ( function x -> Parse.parse "" x) (coverage_suite ());
         make_error_coverage_suite "parsing_error_coverage" ( function x -> Parse.parse "" x) (error_coverage_suite ());
         make_expected_suite "parsing_expected" (Core.Utils.ast_to_str Ast.show_program) ( function x -> Parse.parse "" x) (function x -> x) (expected_suite ());
    ]