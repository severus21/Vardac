(* issues with recursive type can not reference main_type constructore here or main_type serialization function here *)
type mapEncoding = Atom.atom list [@@deriving yojson]
let map_to_yojson m = mapEncoding_to_yojson @@ (List.map fst (Atom.VMap.bindings m)) 
let map_of_yojson json = Ppx_deriving_yojson_runtime.Result.Ok (Atom.VMap.empty)
    
type stringmapEncoding = (string * string) list [@@deriving yojson]
let htbl_to_yojson h = [%to_yojson: stringmapEncoding] @@ (List.of_seq (Hashtbl.to_seq h))

let htbl_of_yojson json = 
    Ppx_deriving_yojson_runtime.Result.(match [%of_yojson: stringmapEncoding] json with
        | Ppx_deriving_yojson_runtime.Result.Ok lst -> 
            let htbl = Hashtbl.create (List.length lst) in
            List.iter (function (k, v) -> Hashtbl.add htbl k v) lst;
            Ppx_deriving_yojson_runtime.Result.Ok htbl
        | Ppx_deriving_yojson_runtime.Result.Error s ->             
            Ppx_deriving_yojson_runtime.Result.Error s)

let stringmap_to_yojson m = [%to_yojson: stringmapEncoding] @@ (Collections.StringMap.bindings m) 
let stringmap_of_yojson json =     Ppx_deriving_yojson_runtime.Result.(match [%of_yojson: stringmapEncoding] json with
| Ppx_deriving_yojson_runtime.Result.Ok lst -> 
    Ppx_deriving_yojson_runtime.Result.Ok (
        List.fold_left (fun m (k, v) -> Collections.StringMap.add k v m) Collections.StringMap.empty lst
    ) 
| Ppx_deriving_yojson_runtime.Result.Error s ->             
    Ppx_deriving_yojson_runtime.Result.Error s)