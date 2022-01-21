(* Register the plugin *)
let () =
  print_endline "Registration of Codegen Plugin AkkaJava";
  Queue.add (fun register -> 
    print_endline "Plugin1 is doing something...";
    register (module Plug : Registration.Plugin.Cg_plg)
) Registration.todo