module StringMap = struct 
  include Map.Make(String) 

  let show m = (Error.show_list ";" (fun out (x,_)-> Format.fprintf out "%s" x)) (List.of_seq (to_seq m))  
  let pp _ fmt m = Format.fprintf fmt "%a" (Error.pp_list ";" (fun out (x,_)-> Format.fprintf out "%s" x)) (List.of_seq (to_seq m)) 

  let to_list x = List.of_seq (to_seq x)
  let of_list xs = of_seq (List.to_seq xs)

end

module StringSet = Set.Make(String) 