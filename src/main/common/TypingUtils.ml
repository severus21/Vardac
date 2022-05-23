open Core
open IR
open AstUtils


let fplace = (Error.forge_place "TypeUtils" 0 0)
let auto_fplace smth = {place = fplace; value=smth}

(*let typeof_constructor = 
    let fplace = (Error.forge_place "TypeUtils.typeof_constructor" 0 0) in
    let auto_fplace smth = {place = fplace; value=smth} in
    let ctypeof x = auto_fplace (CType(auto_fplace x)) in
    function
| [mt] -> mt
| mt1::mts -> TArrow (mt1, typeof_constructor mts)*)


(* TODO dedup this fct exists somewhere else*)
let fct_sign argmts ret_type = 
    let fplace = (Error.forge_place "TypeUtils.fct_sign" 0 0) in
    let auto_fplace smth = {place = fplace; value=smth} in

    List.fold_right (fun t1 t2 -> auto_fplace (CType (auto_fplace(TArrow (t1, t2))))) argmts ret_type 


let inv_fct_sign sign =
    let rec aux args = map0_place (function place -> function
        | CType {value=TArrow (mt1, mt2)} -> begin
            match mt2.value with 
            | CType {value=TArrow _} -> aux (mt1::args) mt2
            | _ -> List.rev (mt1::args), mt2
        end)
    in
    aux [] sign



let rec _is_subtype_ct place1 place2 ct1 ct2 =  
    match (ct1, ct2) with
    | TActivationRef mt1, TActivationRef mt2 -> is_subtype mt1 mt2
    | TArrow (mt1_a, mt1_b), TArrow (mt2_a, mt2_b) ->
        is_subtype mt2_a mt1_a && (* contravariance *)
        is_subtype mt1_b mt2_b
    | TArray mt1, TArray mt2 | TList mt1, TList mt2 | TOption mt1, TOption mt2 | TSet mt1, TSet mt2 -> is_subtype mt1 mt2
    | TDict (mt1_a, mt1_b), TDict (mt2_a, mt2_b) | TResult (mt1_a, mt1_b), TResult (mt2_a, mt2_b)-> is_subtype mt1_a mt2_a && is_subtype mt1_b mt2_b
    | TTuple mts1, TTuple mts2 -> List.fold_left (fun flag (mt1,mt2) -> flag && is_subtype mt1 mt2) true (List.combine mts1 mts2)
    | TVPlace _, TVPlace _ -> failwith "FIXME TODO Subtyping for vplace not yet defined,  only equality for now"
    | TUnion (mt1_a, mt1_b), TUnion (mt2_a, mt2_b) -> 
        (* FIXME is it the wanted semantics A|B et B|A should be the same 
        still cases to handle ??*)
        (is_subtype mt1_a mt2_a && is_subtype mt1_b mt2_b) ||
        (is_subtype mt1_a mt2_b && is_subtype mt1_b mt2_a)
    | TUnion (mt1_a, mt1_b), mt2 ->
        is_subtype mt1_a (auto_fplace (CType (auto_fplace mt2))) && is_subtype mt1_b (auto_fplace (CType (auto_fplace mt2)))
    | mt1, TUnion (mt2_a, mt2_b) ->
        is_subtype (auto_fplace (CType (auto_fplace mt1))) mt2_a ||is_subtype (auto_fplace (CType (auto_fplace mt1))) mt2_b
    | TBridge tb1, TBridge tb2 -> 
        is_subtype tb1.in_type tb2.in_type &&
        is_subtype tb1.out_type tb2.out_type &&
        is_subtype tb1.protocol tb2.protocol
    | TPolyVar x, TPolyVar y -> x = y
    | TForall (x1, mt1), TForall(x2, mt2) -> (*TODO add bounded polymorphism*)
        (* x1 = x2 in the context since we are not  using Debruijn variable *)
        let mt2' = IR.replace_type_main_type x2 (Some x1, None) mt2 in
        is_subtype mt1 mt2'
    | _ -> false 
and is_subtype_ct ct1 ct2 =
    equal_ctype ct1 ct2 || (* Impl optimization to avoid runing complex computation if possible 
        capture the subtype relation for flattype
        *)
    _is_subtype_ct ct1.place ct2.place ct1.value ct2.value
and _is_subtype_st (known_subtypes:(_session_type * _session_type) list) place1 place2 st1 st2 =  
    List.mem (st1, st2) known_subtypes || (* for recursive types *)
    match (st1, st2) with
    | STRecv (mt1, st1), STRecv(mt2, st2) ->
        is_subtype mt1 mt2 && (* co-variance *)
        is_subtype_st st1 st2
    | STSend (mt1, st1), STSend(mt2, st2) ->
        is_subtype mt2 mt1 && (* contra-variance *)
        is_subtype_st st1 st2
    | STBranch branches1, STBranch branches2 -> 
        let labels1 = (Atom.Set.of_seq (List.to_seq (List.map (function (x, _, _) ->x) branches1))) in
        let tbl1 = (Atom.VMap.of_seq (List.to_seq (List.map (function (x,y,z) -> (x,(y,z))) branches1))) in
        let labels2 = (Atom.Set.of_seq (List.to_seq (List.map (function (x, _, _) -> x) branches2))) in
        let tbl2 = (Atom.VMap.of_seq (List.to_seq (List.map (function (x,y,z) -> (x,(y,z))) branches2))) in

        let common_labels = Atom.Set.inter labels1 labels2 in

        common_labels = labels1 && (* labels 1 \subset labels 2*)
        Seq.fold_left (fun flag label ->
            let st1,_ = Atom.VMap.find label tbl1 in
            let st2,_ = Atom.VMap.find label tbl2 in
            flag && is_subtype_st st1 st2 (* TODO FIXME check implication *)
        ) true (Atom.Set.to_seq common_labels)
    | STVar x, STVar y -> x = y
    | st0, (STRec (x, st1) as st_rec) ->
        (* One unfolding *)
        let st1 = replace_stype_session_type x (None, Some st_rec) st1 in
        let known_subtypes = (st0,st_rec)::known_subtypes in
        _is_subtype_st known_subtypes place1 st1.place st0 st1.value
    | STPolyVar x, STPolyVar y -> x = y
    | _ -> false
and is_subtype_st ?known_subtypes:(known_subtypes=[]) st1 st2 = 
    equal_stype st1 st2 ||
    _is_subtype_st known_subtypes st1.place st2.place st1.value st2.value

and _is_subtype_cmt place1 place2 cmt1 cmt2 =  
    match (cmt1, cmt2) with
    | CompTUid x, CompTUid y -> x = y
    | TStruct (_, sign1), TStruct (_, sign2) -> 
        (* NB schema name does not account for equality nor subtyping - just used when compiling down to named type system *)
        failwith "TODO subtyping relation between components"
    | TPolyCVar x, TPolyCVar y -> x = y
    | _ -> false 
and is_subtype_cmt cmt1 cmt2 = 
    equal_cmtype cmt1 cmt2 ||
    _is_subtype_cmt cmt1.place cmt2.place cmt1.value cmt2.value
and _is_subtype place1 place2 mt1 mt2 = 
    match (mt1, mt2) with
    | _, CType {value=TFlatType TWildcard} -> true
    | CType ct1, CType ct2 -> is_subtype_ct ct1 ct2
    | SType st1, SType st2 -> is_subtype_st st1 st2
    | CompType cmt1, CompType cmt2 -> is_subtype_cmt cmt1 cmt2 
    | ConstrainedType (mt1,_), ConstrainedType (mt2,_) -> is_subtype mt1 mt2
    (* TODO FIXME implication also*)
    | _ -> false 
and is_subtype mt1 mt2 = 
    equal_mtype mt1 mt2 ||
    _is_subtype mt1.place mt2.place mt1.value mt2.value


type tconstraint = 
| Equality of main_type * main_type
| Or of tconstraint list
[@@deriving show { with_path = false }]

let rec replace_type_tconstraint x_to_replace replaceby = function
| Equality (mt1, mt2) -> Equality(
    replace_type_main_type x_to_replace replaceby mt1,
    replace_type_main_type x_to_replace replaceby mt2
)
| Or tcs -> Or (List.map (replace_type_tconstraint x_to_replace replaceby) tcs )


(* Most general unifier like solver - however do not compute a renaming since we do use it yet 
    returns a renaming [(x,y)] rename x by y 
    argument place is just used to improve debugging
*)
let rec mgu_solver place = function
| [] -> [] (* id *)
| (Equality ({value=CType{value=TVar x}},{value=CType{value=TVar y}}))::u when x = y -> mgu_solver place u
(* TODO for other form of variable ?? *)
| Equality ({value=CType{value=TVar x}},mt)::u -> 
    (* TODO change the return type for free_var *)
    let _, ftvars = free_tvars_mtype Atom.Set.empty mt in
    if List.mem x ftvars then Error.perror place "mgu_solver failure"
    else
        mgu_solver place (List.map (replace_type_tconstraint x (None, Some mt.value)) u) @ [x,mt]
| Equality (mt1,({value=CType{value=TVar y}} as mt2))::u -> begin
   match mt1.value with
    | CType{value=TVar _ } -> Error.perror place "mgu_solver failure"
    | _ -> mgu_solver place (Equality (mt2, mt1)::u)
end
(* TODO add specific support for type constructor *)
| Equality({value=CType{value=TArrow (mt1_a, mt1_b)}},{value=CType{value=TArrow (mt2_a, mt2_b)}})::u -> (* Arrow constuctor *)
    mgu_solver place (
        Equality (mt1_a, mt2_a) ::
        Equality (mt1_b, mt2_b) ::
        u
    )
| _ -> Error.perror place "mgu_solver failure: unspecified behavior"

(* TODO 
    1. incorporate the flag return type in constraints
    2. create two operators (and and or) to combine constraints
*)
let rec _is_instance_ct (cvars:Atom.t list) constraints place1 place2 ct1 ct2 =  
    match (ct1, ct2) with
    | TActivationRef mt1, TActivationRef mt2 -> _is_instance cvars constraints mt1 mt2
    | TArrow (mt1_a, mt1_b), TArrow (mt2_a, mt2_b) ->
        let constraints1, flag1 = _is_instance cvars constraints mt1_a mt2_a in
        let constraints2, flag2 = _is_instance cvars constraints mt1_b mt2_b in
        constraints1@constraints2, flag1&&flag2
    | TArray mt1, TArray mt2 | TList mt1, TList mt2 | TOption mt1, TOption mt2 | TSet mt1, TSet mt2 -> _is_instance cvars constraints mt1 mt2
    | TDict (mt1_a, mt1_b), TDict (mt2_a, mt2_b) | TResult (mt1_a, mt1_b), TResult (mt2_a, mt2_b)-> 
        let constraints1, flag1 = _is_instance cvars constraints mt1_a mt2_a in
        let constraints2, flag2 = _is_instance cvars constraints mt1_b mt2_b in
        constraints1@constraints2, flag1&&flag2
    | TTuple mts1, TTuple mts2 -> 
        List.fold_left (fun (constraints, flag) (mt1,mt2) -> 
            if Bool.not flag then constraints, false (* optimization *)
            else
                _is_instance cvars constraints mt1 mt2 (*NB: cvars are not aggregated and should not *)
        )
        (constraints, true)
        (List.combine mts1 mts2)
    | TVPlace mt1, TVPlace mt2 -> _is_instance cvars constraints mt1 mt2 
    | TUnion (mt1_a, mt1_b), TUnion (mt2_a, mt2_b) -> 
        (* FIXME is it the wanted semantics A|B et B|A should be the same isn't it ?*)
        let constraints1, flag1 = _is_instance cvars constraints mt1_a mt2_a in
        let constraints2, flag2 = _is_instance cvars constraints mt1_b mt2_b in
        let constraints12 = Or (constraints1@constraints2) in

        let constraints3, flag3 = _is_instance cvars constraints mt1_a mt2_b in 
        let constraints4, flag4 = _is_instance cvars constraints mt1_b mt2_a in
        let constraints34 = Or (constraints3@constraints4) in

        [constraints12;constraints34], (flag1&&flag2) || (flag3&&flag4)
    | TBridge tb1, TBridge tb2 -> 
        let constraints1, flag1 = _is_instance cvars constraints tb1.in_type tb2.in_type in
        let constraints2, flag2 = _is_instance cvars constraints tb1.out_type tb2.out_type in
        let constraints3, flag3 = _is_instance cvars constraints tb1.protocol tb2.protocol in
        constraints1@constraints2@constraints3, flag1&&flag2&&flag3
    | TPolyVar x, TPolyVar y -> constraints, x = y
    | TForall (x1, mt1), TForall(x2, mt2) -> (*TODO add bounded polymorphism*)
        (* x1 = x2 in the context since we are not  using Debruijn variable *)
        let mt2' = IR.replace_type_main_type x2 (Some x1, None) mt2 in
        _is_instance cvars constraints mt1 mt2'
    | _ -> constraints, false 
and is_instance_ct cvars constraints ct1 ct2 =
    if equal_ctype ct1 ct2 then (* Impl optimization to avoid runing complex computation if possible 
        capture the instance relation for flattype
        *)
        constraints, true
    else _is_instance_ct cvars constraints ct1.place ct2.place ct1.value ct2.value
and _is_instance_st cvars constraints (known_instances:(_session_type * _session_type) list) place1 place2 st1 st2 : tconstraint list * bool =  
    if List.mem (st1, st2) known_instances then (* for recursive types *)
        constraints, true
    else
        match (st1, st2) with
        | STRecv (mt1, st1), STRecv(mt2, st2) | STSend (mt1, st1), STSend(mt2, st2) ->
            let constraints1, flag1 = _is_instance cvars constraints mt1 mt2 in
            let constraints2, flag2 = is_instance_st cvars constraints st1 st2 in
            constraints1@constraints2, flag1 && flag2
        | STBranch branches1, STBranch branches2 -> 
            let labels1 = (Atom.Set.of_seq (List.to_seq (List.map (function (x, _, _) ->x) branches1))) in
            let tbl1 = (Atom.VMap.of_seq (List.to_seq (List.map (function (x,y,z) -> (x,(y,z))) branches1))) in
            let labels2 = (Atom.Set.of_seq (List.to_seq (List.map (function (x, _, _) -> x) branches2))) in
            let tbl2 = (Atom.VMap.of_seq (List.to_seq (List.map (function (x,y,z) -> (x,(y,z))) branches2))) in

            if Bool.not (labels1 = labels2) then constraints, false
            else
                Seq.fold_left (fun (constraints, flag) label ->
                    let st1,_ = Atom.VMap.find label tbl1 in
                    let st2,_ = Atom.VMap.find label tbl2 in
                    if Bool.not flag then constraints, flag 
                    else 
                        is_instance_st cvars constraints st1 st2 (* TODO FIXME check implication in fact equivalence of predicat*)
                ) (constraints, true) (Atom.Set.to_seq labels1)
        | STVar x, STVar y -> constraints, x = y
        | st0, (STRec (x, st1) as st_rec) ->
            (* One unfolding *)
            let st1 = replace_stype_session_type x (None, Some st_rec) st1 in
            let known_instances = (st0,st_rec)::known_instances in
            _is_instance_st cvars constraints known_instances place1 st1.place st0 st1.value
        | STPolyVar x, STPolyVar y -> constraints, x = y
        | _ -> constraints, false
and is_instance_st cvars constraints ?known_instances:(known_instances=[]) st1 st2 :tconstraint list * bool= 
    if equal_stype st1 st2 then constraints, true 
    else
    _is_instance_st cvars constraints known_instances st1.place st2.place st1.value st2.value

and _is_instance_cmt cvars constraints place1 place2 cmt1 cmt2 =  
    match (cmt1, cmt2) with
    | CompTUid x, CompTUid y -> constraints, x = y
    | TStruct (_, sign1), TStruct (_, sign2) -> failwith "TODO instance relation between components"
    | TPolyCVar x, TPolyCVar y -> constraints, x = y
    | _ -> constraints, false 
and is_instance_cmt cvars constraints cmt1 cmt2 = 
    if equal_cmtype cmt1 cmt2 then constraints, true
    else _is_instance_cmt cvars constraints cmt1.place cmt2.place cmt1.value cmt2.value 
(* TODO FIXME change type of form list to set for perf cvars - if we keep this architecture *)
(* cvars are the type schema variable already seen in mt2 that can be used to create new constraints*)
and __is_instance (cvars: Atom.t list) constraints place1 place2 mt1 mt2 : tconstraint list * bool = 
    match (mt1, mt2) with
    | mt1, (CType{value=TVar x} as mt2) | mt1, (CType{value=TPolyVar x} as mt2)| mt1, (SType{value=STVar x} as mt2) | mt1, (SType{value=STPolyVar x} as mt2)| mt1, (CompType{value=TPolyCVar x} as mt2) when List.mem x cvars -> (* constraint generation *)
        let new_constraint = Equality ({place=place1; value=mt1}, {place=place2; value=mt2}) in
        [ new_constraint ], true
    | CType ct1, CType ct2 -> is_instance_ct cvars constraints ct1 ct2
    | SType st1, SType st2 -> is_instance_st cvars constraints st1 st2
    | CompType cmt1, CompType cmt2 -> is_instance_cmt cvars constraints cmt1 cmt2 
    | ConstrainedType (mt1,_), ConstrainedType (mt2,_) -> _is_instance cvars constraints mt1 mt2
    (* TODO FIXME implication also in fact here it is equivalence *)
    | mt1, CType{value=TForall (x, mt2)} -> 
        logger#debug "Try to unify %s" (Atom.to_string x);
        _is_instance (x::cvars) constraints {place=place1; value=mt1} mt2
    (* constraints using x will be generated afterwards when trying to unify mt1 and mt2*)
    | _ -> constraints, false 
and _is_instance cvars constraints mt1 mt2 = 
    if equal_mtype mt1 mt2 then constraints, true 
    else __is_instance cvars constraints mt1.place mt2.place mt1.value mt2.value
and is_instance mt1 mt2 = 
    logger#error "is_instance %s \n with >>>%s" (show_main_type mt1) (show_main_type mt2);
    let constraints, flag = _is_instance [] [] mt1 mt2 in
    if Bool.not flag then false
    else ( 
        ignore(mgu_solver (mt1.place@mt2.place) constraints); true (* No error => mgu succeed*)
    )

(* check if st1 is a valide suffix of st2 (including subtyping) *)
let rec is_suffix_st st1 st2 = 
    if is_subtype_st st1 st2 then true
    else
    match st2.value with (* try each suffix*)
    | STEnd | STVar _ -> false
    | STSend(_, st2) | STRecv (_, st2) -> is_suffix_st st1 st2
    | STBranch branches | STSelect branches -> 
        [] <> List.filter Fun.id (List.map (function (_, st2, _) -> is_suffix_st st1 st2) branches)
    | STRec (_, st2) ->  is_suffix_st st1 st2
    
