let name = "Java"
let version = "0.0.2"

module Ast                  = Ast
module Output               = Output
module Clean                = Clean
module AstCompilationPass   = AstCompilationPass 
module HumanReadable        = HumanReadable

let display_info () = 
    let all_input = Core.Utils.run_and_collect_stdout "gradle -version" in

    List.iter print_endline (List.map (function str -> "\t\t"^str) all_input)