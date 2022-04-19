open Core
open AstUtils

let fplace = (Error.forge_place "Builtin.*" 0 0)
let auto_fplace smth = {place = fplace; value=smth}

(* load mtype_of_... *)
include AstUtils2.Mtype.Make(struct let fplace = fplace end)

let builtin_mt_error_name = "error"
let builtin_mt_error = mtype_of_var (Atom.builtin builtin_mt_error_name)

let builtin_atomic_types = [
  "unit";
  "int";
  "float";
  "string";
  "bool";
  "place";
  "ipaddress";
  "place_selector";
  "uuid";
  builtin_mt_error_name;
  ]
