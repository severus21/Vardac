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


let rec remove_derive_citem = function
| {place; value=Term t} -> {place; value = Term (remove_derive_term t)}
| citem -> citem
and remove_derive_term = function
| {place=p1; value=Component {place=p2; value=ComponentStructure cdcl}} -> {place=p1; value = Component {place=p2; value=ComponentStructure {
    cdcl with 
        body = List.filter (function | {value=Term {value=Derive _}} -> false | _ -> true)  (List.map remove_derive_citem cdcl.body)
}}}
| t -> t
let remove_derive_program program = List.filter (function | {value=Derive _} -> false | _ -> true) (List.map remove_derive_term program)


(* Derivation *)
let apply_derive program {place; value=derive} =
    match Atom.hint derive.name with
    | "rpc" -> begin
        match derive.cargs, derive.targs, derive.eargs with 
        | [{value=VarCExpr cname,_}],[],[] ->
            let module RPC = (RPC.Make(struct let cname = cname end):RPC.Sig) in
            let module RPC = Core.CompilationPass.Make(RPC) in

            RPC.apply program
        | _ -> Error.error place "Wrong arguments"

    end
    | _ -> raise (Error.PlacedDeadbranchError (place, (Printf.sprintf "Unknown derivation %s" (Atom.hint derive.name))))
let derive_program program = 

    (* *****************
     TODO move the assert into unittest using each example program to check that there is not free vars no free tvars*)
    let fvars0 = Atom.Set.of_seq (List.to_seq (List.map snd (snd (free_vars_program Atom.Set.empty program)))) in
    let ftvars0 = Atom.Set.of_seq (List.to_seq (snd (free_tvars_program Atom.Set.empty program))) in
    let ffvars0 = Atom.Set.union fvars0 ftvars0 in


    logger#debug "fvars %s" (Atom.Set.show fvars0);
    logger#debug "ftvars %s" (Atom.Set.show ftvars0);

        assert(ffvars0 = Atom.Set.empty);

    (*************)

    let derivations = collect_derive_program program in
    let program = remove_derive_program program in
    (* TODO derivations sanity checks*)

    (* Apply derivation in order *)
    List.fold_left apply_derive program derivations
    

(**********************************************************)

let displayed_pass_shortdescription = "Derives has been applied to IR"
let displayed_ast_name = "derived IR"
let show_ast = true
let precondition program = program
let postcondition program = program
let apply_program = derive_program