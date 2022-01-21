let () =
  print_endline "Registration of Plugin2";
  Queue.add (fun f -> failwith "Plugin2 is doing something...") Registration.todo
