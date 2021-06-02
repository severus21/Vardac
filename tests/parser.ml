open OUnit2

let coverage_suite = [
    ("comments inline", "//comment");
    ("comments multiline", "/*comment\nt1\nt2*/");
    ("comments doc", "/**comment\nt1\nt2*/");
    ("ghost comments inline", "(*t1*)");
    ("ghost comments multiline", "(*\nt1\n*)");
    ("type definition abstract", "type BeginTX;");
    ("type definition value", "type test = int;");
    ("type definition arrow", "type test = int -> int;");
    ("type definition result", "type test = Result<int,int>;");
    ("type definition option", "type test = Option<int>;");
    ("type definition with constraints", "type x = int { metadata string x, global int y | y > 0 };");
    ("type definition with nested constraints", "type listc = List<int{|x>0}>{|true};");
    ("type definition st send", "type test = !Msg.;");
    ("type definition st recv", "type test = ?Msg.;");
    ("type definition st choice", "type test = &{\"get\": !Msg. };");
    ("type definition st select", "type test = §{\"get\": !Msg. };");
    ("type definition st rec", "type test = µ x. !int ?float .x;");
    ("type definition st protocol", Core.Utils.file_get_contents "data/protocol1.spec");
    ("type definition st protocol with constraints", Core.Utils.file_get_contents "data/protocol1_with_constraints.spec");
    ("componentdcl base", "component TransactionManager () {}");
    ("componentdcl base args", "component TransactionManager (int a, Error e) {}");
    ("componentdcl base args", "component TransactionManager (int a, Error e) {}");
    ("componentdcl abstract method", Core.Utils.file_get_contents "data/cdcl_abstract_method.spec");
    ("componentdcl method", Core.Utils.file_get_contents "data/cdcl_method.spec");
    ("componentdcl port", Core.Utils.file_get_contents "data/cdcl_port.spec");
    ("componentdcl state", Core.Utils.file_get_contents "data/cdcl_state.spec");
    ("componentdcl contract", Core.Utils.file_get_contents "data/cdcl_contract.spec");
    ("cexpr base", Core.Utils.file_get_contents "data/cexpr_base.spec");
    ("stmt assign", "int x = 1;");
    ("stmt block", "{ int x = 1; return 1; }");
    ("stmt expr lit bool", "true;");
    ("stmt expr lit float", "0.1;");
    ("stmt expr lit int", "1;");
    ("stmt expr lit str", "\"ab\";");
    ("stmt expr lit label", "l\"a::*::b::**\";");
    ("stmt expr lit label", "l\"a::<u>::b::<<v>>\";");
    ("stmt expr binop add", "1 + 1;");
    ("stmt expr unop not", "not true;");
    ("stmt expr dict onelmt", "Dict<label, int> d = {a:1};");
    ("stmt expr dict multielmt", "Dict<label, int> d = {a:1, l\"b\":2};");
    ("stmt expr list empty", "List<int> l1 = [];");
    ("stmt expr list onelmt", "List<int> l2 = [1];");
    ("stmt expr list multelmt", "List<int> l3 = [1,2,3];");
    ("stmt expr option none", "Option<int> o1 = None;");
    ("stmt expr option some", "Option<int> o2 = Some(1);");
    ("stmt expr result err", "Result<int, string> r1 = Err(\"\");");
    ("stmt expr result ok", "Result<int, string> r2 = Ok(1);");
    ("stmt expr set empty", "Set<int> s1 = {};");
    ("stmt expr set onelmt", "Set<int> s2 = {1};");
    ("stmt expr set multielmt", "Set<int> s3 = {1,2,3};");
    ("stmt expr tuple onelmt", "Tuple<int> t1 = (1,);");
    ("stmt expr tuple multielmt", "Tuple<int,int> t2 = (1,2);");
    ("stmt expr app noargs", "f();");
    ("stmt expr app oneargs", "f(10);");
    ("stmt expr app multiargs", "f(10, \"r\");");
    ("stmt expr lambda", "t_fct incr = x -> x+1;");
    ("stmt expr this", "this;");
    ("stmt expr spawn notplaced", "spawn C(1, \"b\");");
    ("stmt expr spawn placed", "spawn C() @ r;");
    ("stmt expr access", "a.b.c;");
    ("stmt expr with paren", "(1+1) + 4;");
    ("stmt controlflow if ", "if (true) { return 2; } else { return 10; }");
    ("ppterm use", "use a;");
    ("ppterm use", "use a.b.c;");
] @ (List.map (function path ->  ("example at "^(Filename.basename path), Core.Utils.file_get_contents path)) (Core.Utils.scandir "examples" ".spec"))

let error_coverage_suite = [
    ("type definition abstract", "type BeginTX", Utils.SyntaxError);
    ("stmt label", "l\"a::b_::b::**\"", Utils.SyntaxError);
    ("stmt controlflow if", "if (true) { 2 } else { return 10; }", Utils.SyntaxError);
]

let expected_suite = [
(*    ("int", "1", [{place = forge_place 0 1; value = Int 1}]);
    ("let", "let x = 1 in 2", [{ place = forge_place 0 14; value = Let (NonRecursive, Binder, "x",
      { place = forge_place 8 9; value = (Int 1)}, { place = forge_place 13 14; value = (Int 2)})}]);
    ("app", "a(x)(y)", [{place = forge_place 0 7; value = App ({place = forge_place 0 4;value=App ({place=forge_place 0 1; value=Var "a"}, {place=forge_place 2 3; value=Var "x"})}, {place=forge_place 5 6; value=Var "y"})}]);*)
]


let unittests () =
    "Parser" >::: [
        Utils.make_coverage_suite "parsing_coverage" ( function x -> Frontend.Parse.parse "" x) coverage_suite;
        Utils.make_error_coverage_suite "parsing_error_coverage" ( function x -> Frontend.Parse.parse "" x) error_coverage_suite;
        Utils.make_expected_suite "parsing_expected" (Core.Utils.ast_to_str Frontend.Ast.show_program) ( function x -> Frontend.Parse.parse "" x) (function x -> x) expected_suite;
    ]
