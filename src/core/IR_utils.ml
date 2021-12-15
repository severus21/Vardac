open AstUtils
open IR


(* Where add them in the scope ?
    Just after the point where there is no more free vars in new_terms
    than in terms
*)
let insert_in_terms new_terms terms = 
    let fvars0 = Atom.Set.of_seq (List.to_seq (List.map snd (snd (free_vars_program Atom.Set.empty terms)))) in
    let ftvars0 = Atom.Set.of_seq (List.to_seq (snd (free_tvars_program Atom.Set.empty terms))) in

    (*Since rec def*)
    let shallow_scan already_binded = function
    | {value = Component {value=ComponentStructure {name}}} -> Atom.Set.add name already_binded 
    | _ -> already_binded
    in
    let toplevel_components =  List.fold_left shallow_scan Atom.Set.empty terms in

    (* 
        acc::t::ts 
        try to add it between acc and t
    *)
    let insert_depth = ref 0 in
    let rec insert_new_terms acc = function
    | [] -> new_terms
    | t::ts -> 
        let current_terms = acc@new_terms in 
        let _fvars = Atom.Set.of_seq (List.to_seq (List.map snd (snd (free_vars_program Atom.Set.empty current_terms)))) in
        let _ftvars = Atom.Set.of_seq (List.to_seq (snd (free_tvars_program Atom.Set.empty current_terms))) in
        (* Because component are type rec per scope *)
        let _ftvars = Atom.Set.diff _ftvars toplevel_components in

        if Atom.Set.subset _fvars  fvars0  && Atom.Set.subset _ftvars ftvars0 then(
            logger#debug "RPC insert depth %d" !insert_depth;
            new_terms@(t::ts)
        )else
            t::(insert_new_terms (acc@[t]) ts) (* FIXME O(n²) *)
    in

    (* New terms do not depend of binders in terms *)
    insert_new_terms [] terms

(*********************************************************************)

(* lowest common ancestor *)
let rec aux_find_lca names current_name (subcomponents : component_dcl list) : bool * Atom.atom option  = 
    assert (Atom.Set.cardinal names > 1);
    let tmp = List.map (find_lca_cdcl names) subcomponents in
    let tmp_true = List.filter (function (f,_) -> f) tmp in
    if List.length tmp_true > 1 then 
        true, current_name
    else (
        (* Target contains all names + LCA *)
        let [_, Some target_name] = tmp_true in 
        let [target] = List.filter (function | {value=ComponentStructure scdcl} -> scdcl.name = target_name) subcomponents in

        find_lca_cdcl names target
    )

and find_lca_cdcl_ names place : _component_dcl ->  bool * Atom.atom option = function
| ComponentStructure cdcl when Atom.Set.mem cdcl.name names -> true, None
| ComponentStructure cdcl -> 
    let subcomponents = List.filter (function | {value=Term{value=Component _}} -> true | _ -> false) cdcl.body in
    let subcomponents = List.map (function | {value=Term{value=Component cdcl}} -> cdcl) subcomponents in

    aux_find_lca names (Some cdcl.name) subcomponents
and find_lca_cdcl names : component_dcl ->  bool * Atom.atom option = map0_place (find_lca_cdcl_ names)

and find_lca_program names program =  
    if Atom.Set.cardinal names > 1 then (
        let subcomponents = List.filter (function | {value=Component _} -> true | _ -> false) program in
        let subcomponents = List.map (function | {value=Component cdcl} -> cdcl) subcomponents in

        snd (aux_find_lca names None subcomponents)
    )else Some (Atom.Set.min_elt names)