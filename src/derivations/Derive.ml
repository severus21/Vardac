open Core
open AstUtils
open IR

(* 
    Extract derivation from AST - since derivations can be defined anywhere in the program 
    they will executed in collection order (i.e. "line order")
*)
let rec _collect_derive_component_item place = function
| Term t -> collect_derive_term t
| citem -> []
and collect_derive_component_item (citem:component_item) = map0_place _collect_derive_component_item citem

and _collect_derive_component_dcl place = function
| ComponentAssign _ ->[] 
| ComponentStructure cdcl -> List.flatten (List.map collect_derive_component_item cdcl.body ) 
and collect_derive_component_dcl cdcl = map0_place _collect_derive_component_dcl cdcl

and _collect_derive_term place = function
| Component c -> collect_derive_component_dcl c
| Derive derive -> [ {place; value=derive}]
| t -> [] 
and collect_derive_term t = map0_place _collect_derive_term t

let collect_derive_program program = List.flatten (List.map collect_derive_term program)


(* Derivation *)
let apply_derive program {place; value=derive} =
    match Atom.hint derive.name with
    | "rpc" -> begin
        match derive.cargs, derive.targs, derive.eargs with 
        | [{value=VarCExpr cname,_}],[],[] ->
            RPC.derive_program program cname
        | _ -> Error.error place "Wrong arguments"

    end
    | _ -> raise (Error.PlacedDeadbranchError (place, (Printf.sprintf "Unknown derivation %s" (Atom.hint derive.name))))
let derive_program program = 
    let derivations = collect_derive_program program in
    (* TODO derivations sanity checks*)

    (* Apply derivation in order *)
    List.fold_left apply_derive program derivations
    
