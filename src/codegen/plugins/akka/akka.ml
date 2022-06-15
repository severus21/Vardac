let name = "Akka"
let version = "0.0.1"

module Ast = Ast
module Finish = Finish
module Misc = Misc
module Prepare = Prepare
module IRI2AstCompilationPass = IRI2AstCompilationPass
module Interfaces = Interfaces

let display_info () = 
    (* TODO use sites *)
    let akka_version = Core.Utils.run_and_collect_stdout "cat templates/Akka\<Java\>/auto/build.gradle.j2 |grep AkkaVersion:" in
    let scala_version = Core.Utils.run_and_collect_stdout "cat templates/Akka\<Java\>/auto/build.gradle.j2 |grep ScalaBinary:" in

    List.iter print_endline (List.map (function str -> "\t\t"^str) akka_version);
    List.iter print_endline (List.map (function str -> "\t\t"^str) scala_version);