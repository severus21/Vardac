open IR
open AstUtils

let fplace = (Error.forge_place "IRMisc" 0 0) 
let auto_fplace smth = {place = fplace; value=smth}
include AstUtils2.Mtype.Make(struct let fplace = fplace end)

let aid_of place e = 
    let auto_place smth = {place; value=smth} in

    auto_place (CallExpr (
        auto_place (VarExpr (Atom.builtin "activationid"), auto_place EmptyMainType),
        [
            e
        ]
    ), auto_place EmptyMainType)


(* FIXME limitaiton - static maybe add some dynamic reflexive capabilities *)
let schema_of cexpr = match fst cexpr.value with
| VarCExpr x -> x
| _ -> failwith "Other kind of component Expr not yet supported by schema_of"

let schema_to_label place schema = 
    let auto_place smth = {place; value=smth} in
    auto_place (LitExpr (auto_place (StringLit (Atom.to_string schema))), auto_place EmptyMainType)

(********** Session type utilities **********)
let rec _dual place : _session_type -> _session_type  = function
| STEnd -> STEnd
| STVar _ as st -> st
| STSend (mt, st) -> STRecv (mt, dual st)
| STRecv (mt, st) -> STSend (mt, dual st)
| STBranch choices -> STSelect (List.map (function (x, st, c) -> (x, dual st, c)) choices)
| STSelect choices -> STBranch (List.map (function (x, st, c) -> (x, dual st, c)) choices)
| STRec (x, st) -> STRec (x, dual st)
| STInline x -> STInline x
| STDual st -> (dual st).value
and dual st : session_type = 
{ st with value = _dual st.place st.value }


(* Return form do not start with an µx.*)
let rec _unfold_st_star = function
| STRec (x, st) ->
    let st = replace_stype_session_type x (None, Some st.value) st in
    st.value
| st -> st
and unfold_st_star st = {place = st.place; value = _unfold_st_star st.value}


(* returns the list of st stages that introduce a message send or receive
    e.g.
    st =def= µx. +{ l1: ?ping!pong. ; l2: x.};
    =>
    [
        st; //therefore we do not store +{ l1: ?ping!pong. ; l2: x.}
        ?ping!pong.
        !pong.
    ]
    then compute a set 

    @param recsts_def - retains (x -> µx. st) for each µx - st found
*)
let stages_of_st = 
    let rec aux_stages_of_st_ place = function
    | STEnd -> []
    | STVar x -> [] (* Do not store µx. ... to avoid doing deduplication afterwards.*)
    | (STRecv (_, st2) as st1) | (STSend (_, st2) as st1)-> st1 :: (aux_stages_of_st st2)
    | (STBranch branches as st) | (STSelect branches as st) -> 
        st :: (List.flatten (List.map (function (_, st_branch, _) -> aux_stages_of_st st_branch) branches))
    | STRec (x, st2) as st1 ->
        (* do not store twice the input stage of the recursion - we choose to retains the recusive aspect (i.e. storing st1 and not st2 (erase by List.tl)) *)
        st1:: List.tl (aux_stages_of_st st2)
    | STInline _ -> raise (Error.PlacedDeadbranchError (place, "STInlinie should have been compiled away before using [aux_stages_of_st]!"))
    and aux_stages_of_st st = map0_place aux_stages_of_st_ st
    in

    aux_stages_of_st 

let rec msgcont_of_st st = map0_place (function place -> function 
    | STEnd -> failwith "STEnd not supported for msgcont_of_st" 
    | STSend (tmsg, st_continuation) | STRecv (tmsg, st_continuation) -> tmsg, st_continuation
    | (STBranch _ as st) | (STSelect _ as st) -> 
        mtype_of_ft TBLabel, {place; value=st}  
    | STRecv (_, st2) as st ->
        fst (msgcont_of_st st2), unfold_st_star {place; value=st}
) st

let e_param_of str : Atom.atom * expr = 
    let param = Atom.fresh str in
    param, e2var param

let rec st_branch_of mt_st branch_label = 
    let blabel = match branch_label.value with 
        | BLabelLit l -> l
        | _ -> raise (Error.PlacedDeadbranchError (branch_label.place, "must be session type label"))
    in

    match mt_st.value with 
    | SType st -> begin 
        match st.value with 
        | STBranch branches -> begin 
            try
                let _, _st, _ = List.find (function (_label, _st, _) -> _label = blabel) branches in
                _st
            with Not_found -> Error.error mt_st.place "label [%s] not found in %s" (Atom.to_string blabel) (show_session_type st)
        end
        | STDual st -> (* when called before partial evaluation *)
            st_branch_of {place=st.place; value=SType st} branch_label
    | mt -> failwith (show__session_type mt)
    end
    | mt -> failwith (show__main_type mt)