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