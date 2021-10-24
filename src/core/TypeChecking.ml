open Utils
open Error
open Easy_logging
open Fieldslib
open AstUtils
open TypingUtils

module S = IR
module T = IR
open IR

let logger = Logging.make_logger "_1_ compspec.frontend" Debug [];;


let rec _is_subtype_ct place1 place2 ct1 ct2 =  
    (=) ct1 ct2 || (* Impl optimization to avoid runing complex computation if possible 
        capture the subtype relation for flattype
        *)
    match (ct1, ct2) with
    | TActivationInfo mt1, TActivationInfo mt2 -> is_subtype mt1 mt2
    | TArrow (mt1_a, mt1_b), TArrow (mt2_a, mt2_b) ->
        is_subtype mt2_a mt1_a && (* contravariance *)
        is_subtype mt1_b mt2_b
    | TArray mt1, TArray mt2 | TList mt1, TList mt2 | TOption mt1, TOption mt2 | TSet mt1, TSet mt2 -> is_subtype mt1 mt2
    | TDict (mt1_a, mt1_b), TDict (mt2_a, mt2_b) | TResult (mt1_a, mt1_b), TResult (mt2_a, mt2_b)-> is_subtype mt1_a mt2_a && is_subtype mt1_b mt2_b
    | TTuple mts1, TTuple mts2 -> List.fold_left (fun flag (mt1,mt2) -> flag && is_subtype mt1 mt2) true (List.combine mts1 mts2)
    | TVPlace _, TVPlace _ -> failwith "Subtyping for vplace not yet defined"
    | TUnion (mt1_a, mt1_b), TUnion (mt2_a, mt2_b) -> 
        (* FIXME is it the wanted semantics*)
        is_subtype mt1_a mt2_a && is_subtype mt1_b mt2_b
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
and is_subtype_ct ct1 ct2 = _is_subtype_ct ct1.place ct2.place ct1.value ct2.value
and _is_subtype_st (known_subtypes:(_session_type * _session_type) list) place1 place2 st1 st2 =  
    (=) st1 st2 || (* Impl optimization to avoid runing complex computation if possible *)
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
and is_subtype_st ?known_subtypes:(known_subtypes=[]) st1 st2 = _is_subtype_st known_subtypes st1.place st2.place st1.value st2.value

and _is_subtype_cmt place1 place2 cmt1 cmt2 =  
    (=) cmt1 cmt2 || (* Impl optimization to avoid runing complex computation if possible *)
    match (cmt1, cmt2) with
    | CompTUid x, CompTUid y -> x = y
    | TStruct mts1, TStruct mts2 -> failwith "TODO subtyping relation between components"
    | TPolyCVar x, TPolyCVar y -> x = y
    | _ -> false 
and is_subtype_cmt cmt1 cmt2 = _is_subtype_cmt cmt1.place cmt2.place cmt1.value cmt2.value
and _is_subtype place1 place2 mt1 mt2 = 
    (=) mt1 mt2 || (* Impl optimization to avoid runing complex computation if possible *)
    match (mt1, mt2) with
    | _, CType {value=TFlatType TWildcard} -> true
    | CType ct1, CType ct2 -> is_subtype_ct ct1 ct2
    | SType st1, SType st2 -> is_subtype_st st1 st2
    | CompType cmt1, CompType cmt2 -> is_subtype_cmt cmt1 cmt2 
    | ConstrainedType (mt1,_), ConstrainedType (mt2,_) -> is_subtype mt1 mt2
    (* TODO FIXME implication also*)
    | _ -> false 
and is_subtype mt1 mt2 = _is_subtype mt1.place mt2.place mt1.value mt2.value

let rec a2d_place a2d_value ({ AstUtils.place ; AstUtils.value}: 'a AstUtils.placed) = 
    let value = a2d_value place value in
    {AstUtils.place; AstUtils.value}


let rec a2d_var_expr x = x 
and a2d_var_type x = x
and a2d_var_component x = x

(************************************ Types **********************************)

(* notation debruin uniquement pour les expressions le reste reste en atom*)
and _a2d_composed_type ct = function 
| TActivationInfo mt -> TActivationInfo (a2d_main_type mt)
| TArrow (mt1, mt2) -> TArrow (
    a2d_main_type mt1,
    a2d_main_type mt2
)
| TVar x -> TVar (a2d_var_type x)
| TFlatType ft -> TFlatType ft
| TArray mt -> TArray (a2d_main_type mt)
| TDict (mt1, mt2) -> TDict (
    a2d_main_type mt1,
    a2d_main_type mt2
)
| TList mt -> TList (a2d_main_type mt)
| TOption mt -> TOption (a2d_main_type mt)
| TResult (mt1, mt2) -> TResult (
    a2d_main_type mt1,
    a2d_main_type mt2
)
| TSet mt -> TSet (a2d_main_type mt)
| TTuple mts -> TTuple (List.map a2d_main_type mts)
| TVPlace mt -> TVPlace (a2d_main_type mt)
| TUnion (mt1, mt2) -> TUnion (
    a2d_main_type mt1,
    a2d_main_type mt2
)
| TBridge b -> TBridge {
    in_type = a2d_main_type b.in_type;
    out_type = a2d_main_type b.out_type;
    protocol = a2d_main_type b.protocol;
}
| TRaw bt -> TRaw bt
and a2d_composed_type ct = a2d_place _a2d_composed_type ct

and _a2d_session_type place = function
| STEnd -> STEnd
| STVar x -> STVar x
| STSend (mt, st) -> 
    let mt = a2d_main_type mt in
    let st = a2d_session_type st in
    STSend (mt, st)
| STRecv (mt, st) -> 
    let mt = a2d_main_type mt in
    let st = a2d_session_type st in
    STRecv (mt, st)
| STRec (x, st) -> 
    let st = a2d_session_type st in
    STRec (x, st)
| STInline x -> STInline x 
and a2d_session_type st = a2d_place _a2d_session_type st

and _a2d_component_type place = function
| CompTUid x -> CompTUid x
and a2d_component_type ct = a2d_place _a2d_component_type ct 

and _a2d_main_type place = function
| CType ct -> CType (a2d_composed_type ct)
| SType st -> SType (a2d_session_type st)
| CompType ct -> CompType (a2d_component_type ct)
| ConstrainedType (mt, guard) -> ConstrainedType (
    a2d_main_type mt, 
    a2d_applied_constraint guard)
and a2d_main_type mt = a2d_place _a2d_main_type mt 

(******************************** Constraints ********************************)

and _a2d_constraint_header place = function
| UseMetadata (mt, x) -> 
    UseMetadata (
        a2d_main_type mt,
        a2d_var_expr x
    )
| SetTimer x -> SetTimer (a2d_var_expr x)
| SetFireTimer (x, i) -> SetFireTimer (a2d_var_expr x, i)
and a2d_constraint_header h= a2d_place _a2d_constraint_header h

and a2d_applied_constraint (headers, guard_opt) = 
    List.map a2d_constraint_header headers, Option.map a2d_expr guard_opt

(************************************ (V) - Place ****************************)

and a2d_vplace (vp:vplace) = 
 {
    name = a2d_var_component vp.name;
    nbr_instances = a2d_expr vp.nbr_instances;
    features = vp.features;
    children = List.map a2d_vplace vp.children;
}

(************************************* Literals ******************************)

and _a2d_literal place = function
| VoidLit -> VoidLit
| BoolLit b -> BoolLit b
| FloatLit f -> FloatLit f
| IntLit i -> IntLit i
| LabelLit l -> failwith "a2d_literal label"
| StringLit s -> StringLit s
| VPlace vp -> VPlace (a2d_vplace vp)
| Bridge b -> Bridge {
    id = a2d_var_component b.id;
    protocol_name = a2d_var_component b.protocol_name;
}
and a2d_literal lit = a2d_literal lit

and _a2d_expr place (e, mt_e) =
    let e = match e with  
        | VarExpr x -> VarExpr (a2d_var_expr x) 
        | AccessExpr (e1, e2) -> AccessExpr (
            a2d_expr e1,
            a2d_expr e2
        )
        | BinopExpr (e1, op, e2) -> BinopExpr (
            a2d_expr e1,
            op,
            a2d_expr e2
        )
        | LambdaExpr (x, mt, stmt) -> failwith "TODO a2d_expr binder"
        | LitExpr l -> LitExpr (a2d_literal l)
        | UnopExpr (op, e) -> UnopExpr (op, a2d_expr e)
        | CallExpr (e, es) -> CallExpr(
            a2d_expr e,
            List.map a2d_expr es
        )
        | NewExpr (e, es) -> NewExpr(
            a2d_expr e,
            List.map a2d_expr es
        )
        | This -> This 
        | Spawn spawn -> Spawn {
            c = a2d_component_expr spawn.c;
            args = List.map a2d_expr spawn.args;
            at = Option.map a2d_expr spawn.at;
        } 
        | BoxCExpr ce -> BoxCExpr (a2d_component_expr ce)
        | OptionExpr e_opt -> OptionExpr (Option.map a2d_expr e_opt)
        | ResultExpr (e1_opt, e2_opt) -> ResultExpr (Option.map a2d_expr e1_opt, Option.map a2d_expr e2_opt)
        | BlockExpr (b, es) -> BlockExpr (
            b,
            List.map a2d_expr es
        )
        | Block2Expr (b, es) -> Block2Expr (
            b,
            List.map (function (e1, e2) -> a2d_expr e1, a2d_expr e2) es
        )
    in
    e, mt_e
and a2d_expr e = a2d_place _a2d_expr e

and _a2d_stmt place : _stmt -> _stmt = function
| EmptyStmt -> EmptyStmt
| AssignExpr (x, e) -> AssignExpr (
    a2d_var_expr x,
    a2d_expr e
)
| AssignThisExpr (x, e) -> AssignThisExpr (
    a2d_var_component x,
    a2d_expr e
)
| LetExpr _ -> failwith "binders"
| CommentsStmt c -> CommentsStmt c

| BreakStmt -> BreakStmt
| ContinueStmt -> ContinueStmt
| ExitStmt i -> ExitStmt i
| ForStmt (mt, x, e, stmt) -> ForStmt (
    a2d_main_type mt,
    a2d_var_expr x,
    a2d_expr e,
    a2d_stmt stmt
)
| IfStmt (e, stmt1, stmt2_opt) -> IfStmt (
    a2d_expr e,
    a2d_stmt stmt1,
    Option.map a2d_stmt stmt2_opt
)
| MatchStmt (e, branches) -> MatchStmt (
    a2d_expr e,
    List.map (function (e, stmt) -> 
        a2d_expr e, a2d_stmt stmt
    ) branches
)
| ReturnStmt e -> ReturnStmt (a2d_expr e)
| ExpressionStmt e -> ExpressionStmt (a2d_expr e)
| BlockStmt stmts -> 
    let stmts = List.map a2d_stmt stmts in
    BlockStmt stmts
and a2d_stmt stmt =  a2d_place _a2d_stmt stmt

and _a2d_param place (mt, x) = ( a2d_main_type mt, a2d_var_expr x)
and a2d_param arg = a2d_place _a2d_param arg

and _a2d_port place (p:_port) = {
    name = a2d_var_component p.name;
    input = a2d_expr p.input;
    expecting_st = a2d_main_type p.expecting_st;
    callback = a2d_expr p.callback;
} 
and a2d_port p = a2d_place _a2d_port p


and _a2d_contract place (p:_contract) = {
    method_name = a2d_var_component p.method_name;
    pre_binders = List.map (function (mt, x, e) -> 
        a2d_main_type mt,
        a2d_var_expr x,
        a2d_expr e
    ) p.pre_binders;
    ensures = Option.map a2d_expr p.ensures;
    returns = Option.map a2d_expr p.returns;
} 
and a2d_contract c = a2d_place _a2d_contract c

and _a2d_method place m = {
    m with
    name = a2d_var_component m.name;
    ghost = m.ghost;
    ret_type = a2d_main_type m.ret_type;
    args =  List.map a2d_param m.args;
    body = List.map a2d_stmt m.body;
    contract_opt = Option.map a2d_contract m.contract_opt;
} 
and a2d_method m = a2d_place _a2d_method m

and _a2d_state place = function 
| StateDcl s -> StateDcl {
    name = a2d_var_component s.name;
    ghost = s.ghost;
    type0 = a2d_main_type s.type0;
    body = Option.map a2d_expr s.body;
} 
and a2d_state s = a2d_place _a2d_state s

and _a2d_component_item place = function 
| Contract s -> Contract (a2d_contract s)
| Include ce -> Include (a2d_component_expr ce)
| Method m -> Method (a2d_method m)
| Port p -> Port (a2d_port p)
| State s -> State (a2d_state s)
| Term t -> Term (a2d_term t)
and a2d_component_item citem= a2d_place _a2d_component_item citem


and _a2d_component_dcl place = function 
| ComponentStructure cdcl -> ComponentStructure {
    target_name = cdcl.target_name;
    name = a2d_var_component cdcl.name;
    args = List.map a2d_param cdcl.args;
    body = List.map a2d_component_item cdcl.body
} 
| ComponentAssign cdcl -> ComponentAssign {
    name = a2d_var_component cdcl.name;
    args = List.map a2d_param cdcl.args;
    value = a2d_component_expr cdcl.value;
} 
and a2d_component_dcl cdcl = a2d_place _a2d_component_dcl cdcl



(********************** Manipulating component structure *********************)
and _a2d_component_expr place (ce, mt_ce)=
    let ce = match ce with 
        | VarCExpr x -> VarCExpr (a2d_var_component x)
        | AppCExpr (ce1, ce2) -> AppCExpr (
            a2d_component_expr ce1,
            a2d_component_expr ce2
        )
        | UnboxCExpr e -> UnboxCExpr (a2d_expr e)
        | AnyExpr e -> AnyExpr (a2d_expr e)
    in ce, mt_ce
and a2d_component_expr ce = a2d_place _a2d_component_expr ce

(************************************ Program *****************************)

and _a2d_function_dcl place (fdcl:_function_dcl) : _function_dcl = {
    name = a2d_var_expr fdcl.name;
    ret_type = a2d_main_type fdcl.ret_type;
    args = List.map a2d_param fdcl.args;
    body = List.map a2d_stmt fdcl.body;
} 
and a2d_function_dcl fdcl = a2d_place _a2d_function_dcl fdcl

and _a2d_typedef place = function 
| ClassicalDef (x, mts, body) -> ClassicalDef (
    a2d_var_type x,
    List.map a2d_main_type mts,
    body
) 
| EventDef (x, mts, body) -> EventDef (
    a2d_var_type x,
    List.map a2d_main_type mts,
    body
)  
| ProtocolDef (x, mt) -> ProtocolDef (
    a2d_var_type x,
    a2d_main_type mt
)  
and a2d_typedef tdef = a2d_place _a2d_typedef tdef

and _a2d_term place = function 
| EmptyTerm -> EmptyTerm
| Comments c -> Comments c
| Stmt stmt -> 
    let stmt = a2d_stmt stmt in
    Stmt stmt
| Component c -> Component (a2d_component_dcl c)
| Function f -> Function (a2d_function_dcl f)
| Typealias (x, body) -> Typealias (
    a2d_var_type x,
    Option.map a2d_main_type body
)
| Typedef tdef -> Typedef (a2d_typedef tdef)
and a2d_term t = a2d_place _a2d_term t

and a2d_program program = 
    List.map a2d_term program
