open Easy_logging

let logger = Logging.make_logger "vardac.Akka.Builtin" Debug [];;


(* No need for type since type checking already done *)

let builtin_methods : (string * string) list= [
    "getLeft", "io.vavr.Either::getLeft"; 
    "getRight", "io.vavr.Either::getRight"; 
    "isRight", "io.vavr.Either::isRight"; 
    "isLeft", "io.vavr.Either::isLeft"; 
    "get", "io.vavr.Either::get";
]

let builtin_htbl = Hashtbl.of_seq (List.to_seq builtin_methods)

let is_builtin_expr x = 
        (* whiteliste based selection *)
    try 
        let _ = Hashtbl.find builtin_htbl x in true 
    with Not_found -> false