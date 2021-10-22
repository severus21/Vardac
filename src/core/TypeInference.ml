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




let rec typeof_place typeof_value ({ place ; value}: 'a placed) = 
    let value = typeof_value place value in
    {place; value}

let typeof_literal l = 
    let fplace = (Error.forge_place "TypeInference.typeof_literal" 0 0) in
    let auto_fplace smth = {place = fplace; value=smth} in
    let of_tflat ft = auto_fplace(CType ( auto_fplace (TFlatType ft))) in
    let ctypeof ct = auto_fplace(CType(auto_fplace ct)) in
match l with
| VoidLit -> of_tflat TVoid
| BoolLit _ -> of_tflat TBool
| FloatLit _ -> of_tflat TFloat
| IntLit _ -> of_tflat TInt
| LabelLit _ -> of_tflat TLabel
| StringLit _ -> of_tflat TStr
| ActivationInfo _ -> failwith "ActivationInfo Typeinference - do we need this literal since it carries no value"
| Place _ -> failwith "Place do we need this literal since it can not exists statically"
| VPlace _-> 
    (* forall x, vplace<x> - x must be unified during typecheking *)
    let x = Atom.fresh "x" in
    ctypeof (TForall(x, ctypeof (TVPlace (ctypeof (TVar x)))))
| Bridge b -> failwith "How to infer type of Bridge" 

let typeof_unop op mt_e = 
    let fplace = (Error.forge_place "TypeInference.typeof_literal" 0 0) in
    let auto_fplace smth = {place = fplace; value=smth} in
    let of_tflat ft = auto_fplace(CType ( auto_fplace (TFlatType ft))) in
match (op, mt_e.value) with
| Not, _ -> of_tflat TBool
| UnpackResult, CType{value=TResult (ok,err)} -> ok

let typeof_binop op mt_e1 mt_e2 = 
    let fplace = (Error.forge_place "TypeInference.typeof_literal" 0 0) in
    let auto_fplace smth = {place = fplace; value=smth} in
    let of_tflat ft = auto_fplace(CType ( auto_fplace (TFlatType ft))) in
match (op, mt_e1.value, mt_e2.value) with
| And, _,_ | Or, _, _ | StructuralEqual, _ ,_ | Equal, _, _ | GreaterThanEqual, _,_ | LessThanEqual, _,_ | GreaterThan, _, _ | LessThan, _, _| In, _, _ -> of_tflat TBool    
| _, CType{value=TFlatType TInt},CType{value=TFlatType TInt} -> begin   
    match op with
    | Plus | Minus | Mult | Divide -> of_tflat TInt
end
| _, CType{value=TFlatType TFloat},CType{value=TFlatType TFloat} -> begin   
    match op with
    | Plus | Minus | Mult | Divide -> of_tflat TInt
end


let typeof_block b (mts:main_type list) = 
    let fplace = (Error.forge_place "TypeInference.typeof_block" 0 0) in
    let auto_fplace smth = {place = fplace; value=smth} in
    let of_tflat ft = auto_fplace(CType ( auto_fplace (TFlatType ft))) in

    let mt, wrapper = 
        match mts with
        | [] -> begin 
            let x = Atom.fresh "x" in
            auto_fplace(CType(auto_fplace (TPolyVar x))), function (mt:_main_type) -> auto_fplace(CType(auto_fplace (TForall(x, auto_fplace mt))))
        end
        | mt::_ -> mt, function mt -> auto_fplace mt 
    in

    wrapper(CType(auto_fplace(match b with
        | Block -> failwith "typeof_block Block semantics ????"
        | List -> TList mt
        | Tuple -> assert(mts <> []); TTuple mts
        | Set -> TSet mt 
    )))
let typeof_block2 b (mts: (main_type * main_type) list) = 
    let fplace = (Error.forge_place "TypeInference.typeof_block" 0 0) in
    let auto_fplace smth = {place = fplace; value=smth} in
    let of_tflat ft = auto_fplace(CType ( auto_fplace (TFlatType ft
    ))) in

    let mt1, mt2, wrapper = 
        match mts with
        | [] -> begin 
            let x = Atom.fresh "x" in
            let y = Atom.fresh "y" in
            auto_fplace(CType(auto_fplace (TPolyVar x))), 
            auto_fplace(CType(auto_fplace (TPolyVar y))), 
            function mt -> auto_fplace(CType(auto_fplace (TForall(x, auto_fplace(CType(auto_fplace(TForall(y, auto_fplace mt))))))))
        end
        | (mt1, mt2)::_ -> mt1, mt2, function mt -> auto_fplace mt
    in
    wrapper(CType(auto_fplace(match b with
        | Dict -> TDict (mt1, mt2) 
    )))

let rec tannot_place (tannot_value:Error.place -> 'a -> 'a) ({ AstUtils.place ; AstUtils.value}: 'a placed) = 
    let value : 'a = tannot_value place value in
    {place; value}

(************************************ Types **********************************)

let rec _tannot_session_type (ctx:context) place : _session_type -> context * _session_type = function
| STEnd -> ctx, STEnd
| STVar x -> ctx, STVar x
| STSend (mt, st) -> 
    (* CTX to propagate headers *)
    let ctx, mt = tannot_full_main_type ctx mt in
    let ctx, st = tannot_full_session_type ctx st in
    ctx, STSend (mt, st)
| STRecv (mt, st) -> 
    let ctx, mt = tannot_full_main_type ctx mt in
    let ctx, st = tannot_full_session_type ctx st in
    ctx, STRecv (mt, st)
| STRec (x, st) -> 
    let ctx, st = tannot_full_session_type ctx st in
    ctx, STRec (x, st)
| STInline x -> ctx, STInline x 
and tannot_full_session_type ctx st : context * session_type = 
    let ctx, _st = _tannot_session_type ctx st.place st.value in
    ctx, {place = st.place; value = _st}
and tannot_session_type ctx st = snd (tannot_full_session_type ctx st)


(* Searching for constraints *)
and _tannot_composed_type ctx place = function 
| TActivationInfo mt -> TActivationInfo (tannot_main_type ctx mt)
| TArrow (mt1, mt2) -> TArrow (
    tannot_main_type ctx mt1,
    tannot_main_type ctx mt2
)
| TVar x -> TVar (x)
| TFlatType ft -> TFlatType ft
| TArray mt -> TArray (tannot_main_type ctx mt)
| TDict (mt1, mt2) -> TDict (
    tannot_main_type ctx mt1,
    tannot_main_type ctx mt2
)
| TList mt -> TList (tannot_main_type ctx mt)
| TOption mt -> TOption (tannot_main_type ctx mt)
| TResult (mt1, mt2) -> TResult (
    tannot_main_type ctx mt1,
    tannot_main_type ctx mt2
)
| TSet mt -> TSet (tannot_main_type ctx mt)
| TTuple mts -> TTuple (List.map (tannot_main_type ctx) mts)
| TVPlace mt -> TVPlace (tannot_main_type ctx mt)
| TUnion (mt1, mt2) -> TUnion (
    tannot_main_type ctx mt1,
    tannot_main_type ctx mt2
)
| TBridge b -> TBridge {
    in_type = tannot_main_type ctx b.in_type;
    out_type = tannot_main_type ctx b.out_type;
    protocol = tannot_main_type ctx b.protocol;
}
| TRaw bt -> TRaw bt
and tannot_composed_type ctx ct : composed_type = {
    place = ct.place; 
    value = _tannot_composed_type ctx ct.place ct.value
}

and _tannot_component_type ctx place = function
| CompTUid x -> CompTUid x
and tannot_component_type ctx ct = failwith "" (*tannot_place _tannot_component_type ctx ct*) 

and _tannot_main_type ctx place = function
| CType ct -> ctx, CType (tannot_composed_type ctx ct)
| SType st -> 
    let ctx, st = tannot_full_session_type ctx st in
    ctx, SType st 
| CompType ct -> ctx, CompType (tannot_component_type ctx ct)
| ConstrainedType (mt, guard) -> 
    let outer_ctx, guard = tannot_applied_constraint ctx guard in (* FIXME only use for timer and metadata for protocol -> should not be used on other constraitn 
    therefore only  stype and constraint type returns an outer ctx CType and CompType return the identity
    *)
    outer_ctx, ConstrainedType (
        tannot_main_type ctx mt, 
        guard
    )
and tannot_full_main_type ctx mt = 
    let ctx, _mt =  _tannot_main_type ctx mt.place mt.value in
    ctx, {
        place = mt.place;
        value = _mt
    }
and tannot_main_type ctx mt : main_type = snd (tannot_full_main_type ctx mt)

(******************************** Constraints ********************************)

and _tannot_constraint_header ctx place = 
    let fplace = (Error.forge_place "TypeInference._tannot_constraint_header" 0 0) in
    let auto_fplace smth = {place = fplace; value=smth} in
    let ctypeof x = auto_fplace (CType(auto_fplace x)) in
function
| UseMetadata (mt, x) -> 
    let outer_ctx = register_expr_type ctx x mt in
    outer_ctx, UseMetadata (
        tannot_main_type ctx mt,
        x
    )
| SetTimer x -> 
    let ctx = register_expr_type ctx x (ctypeof (TFlatType TTimer)) in
    ctx, SetTimer x
| SetFireTimer (x, i) -> 
    let ctx = register_expr_type ctx x (ctypeof (TFlatType TTimer)) in
    ctx, SetFireTimer (x, i)
and tannot_constraint_header ctx h= 
    let ctx, _h = _tannot_constraint_header ctx h.place h.value in
    ctx, {
        place = h.place;
        value = _h 
    }

and tannot_applied_constraint ctx (headers, guard_opt) = 
    let outer_ctx, headers = List.fold_left_map tannot_constraint_header ctx headers in
    outer_ctx, (headers, Option.map (tannot_expr ctx) guard_opt)

(************************************ (V) - Place ****************************)

and tannot_vplace ctx (vp:vplace) = 
 {
    name = vp.name;
    nbr_instances = tannot_expr ctx vp.nbr_instances;
    features = vp.features;
    children = List.map (tannot_vplace ctx) vp.children;
}

(************************************* Literals ******************************)

and _tannot_expr ctx place (e, {value=EmptyMainType}) =
    let fplace = (Error.forge_place "TypeInference.typeof_block" 0 0) in
    let auto_fplace smth = {place = fplace; value=smth} in
    let ctypeof x = auto_fplace (CType(auto_fplace x)) in

    match e with  
        | VarExpr x -> VarExpr x, typeof_var_expr ctx x 
        | AccessExpr (e1, e2) -> AccessExpr(
            tannot_expr ctx e1,
            tannot_expr ctx e2
        ), failwith "todo typeof_expr" 
        | BinopExpr (e1, op, e2) ->
            let e1 = tannot_expr ctx e1 in
            let e2 = tannot_expr ctx e2 in
            BinopExpr (e1, op, e2), typeof_binop op (snd e1.value) (snd e2.value)
        | LambdaExpr (x, mt, e) -> 
            let ctx = register_expr_type ctx x mt in
            let e = tannot_expr ctx e in
            LambdaExpr (x, mt, e), ctypeof (TArrow (mt, snd e.value))
        | LitExpr l -> LitExpr l, typeof_literal l.value
        | UnopExpr (op, e) -> 
            let e = tannot_expr ctx e in
            UnopExpr (op, e), typeof_unop op (snd e.value) 
        | CallExpr (e, es) -> 
            let e = tannot_expr ctx e in
            let es = List.map (tannot_expr ctx) es in
            let rec ret_typeof depth mt = match mt.value with (*TODO check types here ??*)
                | _ when depth = 0 -> mt
                | CType{value=TArrow (_, mt2)} -> ret_typeof (depth-1) mt2 
                | _ -> Error.error place "Function expect %d args, not %d" ((List.length es)-depth) (List.length es)
            in
            CallExpr(e, es), ret_typeof (List.length es) (snd e.value) 
        | NewExpr (e, es) -> 
            let e = tannot_expr ctx e in
            let es = List.map (tannot_expr ctx) es in
            let rec ret_typeof depth mt = match mt.value with (*TODO check types here ??*)
                | _ when depth = 0 -> mt
                | CType{value=TArrow (_, mt2)} -> ret_typeof (depth-1) mt2 
                | _ -> Error.error place "Function expect %d args, not %d" ((List.length es)-depth) (List.length es)
            in
            NewExpr(
                tannot_expr ctx e,
                List.map (tannot_expr ctx) es
            ), ret_typeof (List.length es) (snd e.value)
        | This -> begin 
            match ctx.self with
            | None -> Error.error place "[this] can not be used outside component definition" 
            | Some self -> This, auto_fplace( CompType (auto_fplace (CompTUid (self))))
        end
        | Spawn spawn -> 
            let c = tannot_component_expr ctx spawn.c in
            Spawn {
            c = c;
            args = List.map (tannot_expr ctx) spawn.args;
            at = Option.map (tannot_expr ctx) spawn.at;
        }, ctypeof(TActivationInfo(snd c.value))
        | BoxCExpr ce -> failwith "BoxCExpr Typeinference"
        | OptionExpr e_opt ->  
            let e_opt = Option.map (tannot_expr ctx) e_opt in
            let ct = match e_opt with
                | Some {value=(_, mt)} -> TOption mt
                | None -> 
                    let x = Atom.fresh "x" in
                    TForall(x, ctypeof(TOption (ctypeof (TPolyVar x))))
            in
            OptionExpr e_opt, ctypeof ct
        | ResultExpr (e1_opt, e2_opt) -> 
            let e1_opt = Option.map (tannot_expr ctx) e1_opt in
            let e2_opt = Option.map (tannot_expr ctx) e2_opt in

            let x = Atom.fresh "x" in
            let ct = match e1_opt, e2_opt with
                | Some {value=(_, mt)}, None  -> 
                    TForall(x, ctypeof(TResult (mt, ctypeof(TPolyVar x))))
                | None, Some{value=(_, mt)} -> TForall(x, ctypeof( TResult (ctypeof(TPolyVar x), mt)))
            in
            ResultExpr (e1_opt, e2_opt), ctypeof ct 
        | BlockExpr (b, es) -> 
            let es = List.map (tannot_expr ctx) es in
            let mt = typeof_block b (List.map (function e -> snd e.value)  es) in
            BlockExpr ( b, es), mt
        | Block2Expr (b, es) -> 
            let es = List.map (function (e1, e2) -> tannot_expr ctx e1, tannot_expr ctx e2) es in
            let mt = typeof_block2 b (List.map (function (e1, e2) -> snd e1.value, snd e2.value) es) in
            
            Block2Expr (b, es), mt
and tannot_expr ctx e = {
    place = e.place;
    value = _tannot_expr ctx e.place e.value
}

and _tannot_stmt ctx place : _stmt -> context * _stmt = function
| EmptyStmt -> ctx, EmptyStmt
| AssignExpr (x, e) -> ctx, AssignExpr (
    x,
    tannot_expr ctx e
)
| AssignThisExpr (x, e) -> ctx, AssignThisExpr (
    x,
    tannot_expr ctx e
)
| LetExpr (mt, x, e) -> 
    let ctx = register_expr_type ctx x mt in
    let e = tannot_expr ctx e in 
    ctx, LetExpr (mt, x, e)
| CommentsStmt c -> ctx, CommentsStmt c
| BreakStmt -> ctx, BreakStmt
| ContinueStmt -> ctx, ContinueStmt
| ExitStmt i -> ctx, ExitStmt i
| ForStmt (mt, x, e, stmt) -> 
    let inner_ctx = register_expr_type ctx x mt in
    ctx, ForStmt (
        tannot_main_type ctx mt,
        x,
        tannot_expr ctx e,
        snd (tannot_stmt inner_ctx stmt)
    )
| IfStmt (e, stmt1, stmt2_opt) -> ctx, IfStmt (
    tannot_expr ctx e,
    snd (tannot_stmt ctx stmt1),
    Option.map snd (Option.map (tannot_stmt ctx) stmt2_opt)
)
| MatchStmt (e, branches) -> ctx, MatchStmt (
    tannot_expr ctx e,
    List.map (function (e, stmt) -> 
        tannot_expr ctx e, snd (tannot_stmt ctx stmt)
    ) branches
)
| ReturnStmt e -> ctx, ReturnStmt (tannot_expr ctx e)
| ExpressionStmt e -> ctx, ExpressionStmt (tannot_expr ctx e)
| BlockStmt stmts -> 
    let ctx, stmts = List.fold_left_map tannot_stmt ctx stmts in
    ctx, BlockStmt stmts
and tannot_stmt ctx stmt =  
    let ctx, _stmt = _tannot_stmt ctx stmt.place stmt.value in
    ctx, {place = stmt.place; value = _stmt }

and _tannot_param ctx place (mt, x) = ( tannot_main_type ctx mt, x)
and tannot_param ctx arg = {
    place = arg.place;
    value = _tannot_param ctx arg.place arg.value
}

and _tannot_port ctx place (p:_port) = ctx, {
    name = p.name;
    input = tannot_expr ctx p.input;
    expecting_st = tannot_main_type ctx p.expecting_st;
    callback = tannot_expr ctx p.callback;
} 
and tannot_port ctx p = 
    let ctx, _p = _tannot_port ctx p.place p.value in
    ctx, {
        place = p.place;
        value = _p
    }


and _tannot_contract ctx place (p:_contract) = 
    let inner_ctx = List.fold_left (fun ctx (mt, x, _) -> register_expr_type ctx x mt) ctx p.pre_binders in
    {
        method_name = p.method_name;
        pre_binders = List.map (function (mt, x, e) -> 
            tannot_main_type ctx mt,
            x,
            tannot_expr ctx e
        ) p.pre_binders;
        ensures = Option.map (tannot_expr inner_ctx) p.ensures; 
        returns = Option.map (tannot_expr inner_ctx) p.returns; (* TODO add an Arrow ret_type -> ...*)
    } 
and tannot_contract ctx c = {
    place = c.place;
    value = _tannot_contract ctx c.place c.value
}

and _tannot_method ctx place =
    let fplace = (Error.forge_place "TypeInference.typeof_literal" 0 0) in
    let auto_fplace smth = {place = fplace; value=smth} in
function
| CustomMethod m -> 
    let fct_sign = List.fold_right (fun t1 t2 -> auto_fplace (CType (auto_fplace(TArrow (t1, t2))))) (List.map (function (arg: param) -> fst arg.value) m.args) m.ret_type in 
    let outer_ctx = register_expr_type ctx m.name fct_sign in
    let inner_ctx = List.fold_left (fun ctx {value=(mt, x)} -> register_expr_type ctx x mt) ctx m.args in
    
    outer_ctx, CustomMethod {
        name = m.name;
        ghost = m.ghost;
        ret_type = tannot_main_type ctx m.ret_type;
        args = List.map (tannot_param ctx) m.args;
        body = snd (List.fold_left_map tannot_stmt inner_ctx m.body);
        contract_opt =(Option.map (tannot_contract ctx) m.contract_opt);
    } 
| OnStartup m -> 
    let outer_ctx, m = tannot_method ctx m in
    outer_ctx, OnStartup m
| OnDestroy m -> 
    let outer_ctx, m = tannot_method ctx m in
    outer_ctx, OnDestroy m

and tannot_method ctx m = 
    let ctx, _m = _tannot_method ctx m.place m.value in
    ctx, {
        place = m.place;
        value = _m
    }

and _tannot_state ctx place = function 
| StateDcl s -> 
    let outer_ctx = register_expr_type ctx s.name s.type0 in

    outer_ctx, StateDcl {
    name = s.name;
    ghost = s.ghost;
    type0 = tannot_main_type ctx s.type0;
    body = Option.map (tannot_expr ctx) s.body;
} 
and tannot_state ctx s = 
    let ctx, _s = _tannot_state ctx s.place s.value in
    ctx, {
        place = s.place;
        value = _s
    }

and _tannot_component_item ctx place = function 
| Contract s -> ctx, Contract (tannot_contract ctx s)
| Include ce -> ctx, Include (tannot_component_expr ctx ce)
| Method m -> 
    let ctx, m = tannot_method ctx m in
    ctx, Method m 
| Port p -> 
    let ctx, p = tannot_port ctx p in
    ctx, Port p 
| State s -> 
    let ctx, s = tannot_state ctx s in
    ctx, State s
| Term t -> 
    let ctx, t = tannot_term ctx t in
    ctx, Term t 
and tannot_component_item ctx citem = 
    let ctx, _citem = _tannot_component_item ctx citem.place citem.value in 
    ctx, {
        place = citem.place;
        value = _citem
    }

and _tannot_component_dcl ctx place = function 
| ComponentStructure cdcl -> 
    let outer_ctx = register_cexpr_type ctx cdcl.name (failwith "TODO defined type of a component") in
    outer_ctx, ComponentStructure {
    target_name = cdcl.target_name;
    name = cdcl.name;
    args = List.map (tannot_param ctx) cdcl.args;
    body = snd (List.fold_left_map tannot_component_item  ctx cdcl.body) (* TODO first pass allow mutual recursive function ?? - only from header *)
} 
| ComponentAssign cdcl -> ctx, ComponentAssign {
    name = cdcl.name;
    args = List.map (tannot_param ctx) cdcl.args;
    value = tannot_component_expr ctx cdcl.value;
} 
and tannot_component_dcl ctx cdcl = 
    let ctx, _cdcl = _tannot_component_dcl ctx cdcl.place cdcl.value in
    ctx, {
        place = cdcl.place;
        value = _cdcl 
    }



(********************** Manipulating component structure *********************)
and _tannot_component_expr ctx place (ce, {value=EmptyMainType})=
    match ce with 
    | VarCExpr x -> (VarCExpr x, typeof_var_cexpr ctx x)
    | _ -> failwith "tannot_component_cexpr semantics not defined" 
and tannot_component_expr ctx ce = {
    place = ce.place; 
    value = _tannot_component_expr ctx ce.place ce.value
}


(************************************ Program *****************************)

and _tannot_function_dcl ctx place (fdcl:_function_dcl) : context * _function_dcl = 
    let fplace = (Error.forge_place "TypeInference._tannot_function_dcl" 0 0) in
    let auto_fplace smth = {place = fplace; value=smth} in

    let fct_sign = List.fold_right (fun t1 t2 -> auto_fplace (CType (auto_fplace(TArrow (t1, t2))))) (List.map (function (arg: param) -> fst arg.value) fdcl.args) fdcl.ret_type in 
    let outer_ctx = register_expr_type ctx fdcl.name fct_sign in
    let inner_ctx = List.fold_left (fun ctx {value=(mt, x)} -> register_expr_type ctx x mt) ctx fdcl.args in
    
    outer_ctx, {
        name = fdcl.name;
        ret_type = tannot_main_type ctx fdcl.ret_type;
        args = List.map (tannot_param ctx) fdcl.args;
        body = snd (List.fold_left_map tannot_stmt inner_ctx fdcl.body);
    } 
and tannot_function_dcl ctx fdcl = 
    let ctx, _fdcl = _tannot_function_dcl ctx fdcl.place fdcl.value in
    ctx, {
        place = fdcl.place; 
        value = _fdcl
    }

and _tannot_typedef ctx place = function 
| ClassicalDef (x, mts, body) as tdef -> 
    let outer_ctx = register_def_type ctx tdef in
    outer_ctx, ClassicalDef (
        x,
        List.map (tannot_main_type ctx) mts,
        body
    ) 
| EventDef (x, mts, body) as tdef -> 
    let outer_ctx = register_def_type ctx tdef in
    outer_ctx, EventDef (
    x,
    List.map (tannot_main_type ctx) mts,
    body
)  
| ProtocolDef (x, mt) as tdef -> 
    let outer_ctx = register_def_type ctx tdef in
    outer_ctx, ProtocolDef (
    x,
    tannot_main_type ctx mt
)  
and tannot_typedef ctx tdef = 
    let ctx, _tdef = _tannot_typedef ctx tdef.place tdef.value in
    ctx, {
        place = tdef.place;
        value = _tdef 
    }

and _tannot_term ctx place = function 
| EmptyTerm -> ctx, EmptyTerm
| Comments c -> ctx, Comments c
| Stmt stmt -> 
    let ctx, stmt = tannot_stmt ctx stmt in
    ctx, Stmt stmt
| Component c -> 
    let ctx, c = tannot_component_dcl ctx c in
    ctx, Component c 
| Function f -> 
    let ctx, f = tannot_function_dcl ctx f in
    ctx, Function f 
| Typealias (x, body) -> begin
    match Option.map (tannot_main_type ctx) body with(*FIXME why an option for a type alias *)
    | None -> ctx, Typealias (x, None)
    | Some mt -> 
        let ctx = register_type ctx x mt in
        ctx, Typealias (x, Some mt)
end
| Typedef tdef -> 
    let ctx, tdef = tannot_typedef ctx tdef in
    ctx, Typedef tdef
and tannot_term ctx t = 
    let ctx, _t = _tannot_term ctx t.place t.value in
    ctx, {
        place = t.place;
        value = _t 
    }

and tannot_program program = 
    snd (List.fold_left_map tannot_term (fresh_context ()) program) (*TODO scan header for recursive definition of function, method and state*)