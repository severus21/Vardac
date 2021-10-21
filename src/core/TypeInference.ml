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

let typeof_var_expr ctx x : main_type =
    Hashtbl.find ctx.ectx x

let typeof_var_cexpr ctx x : main_type =
    Hashtbl.find ctx.cctx x

let typeof_literal l = 
    let fplace = (Error.forge_place "TypeInference.typeof_literal" 0 0) in
    let auto_fplace smth = {place = fplace; value=smth} in
    let of_tflat ft = auto_fplace(CType ( auto_fplace (TFlatType ft))) in
match l with
| VoidLit -> of_tflat TVoid
| BoolLit _ -> of_tflat TBool
| FloatLit _ -> of_tflat TFloat
| IntLit _ -> of_tflat TInt
| LabelLit _ -> of_tflat TLabel
| StringLit _ -> of_tflat TStr
| ActivationInfo _ -> failwith "ActivationInfo Typeinference - do we need this literal since it carries no value"
| Place _ -> failwith "Place do we need this literal since it can not exists statically"
| VPlace _-> failwith "How to infer type of vplace" 
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

and _tannot_session_type (ctx:context) place : _session_type -> _session_type = function
| STEnd -> STEnd
| STVar x -> STVar x
| STSend (mt, st) -> 
    let mt = tannot_main_type ctx mt in
    let st = tannot_session_type ctx st in
    STSend (mt, st)
| STRecv (mt, st) -> 
    let mt = tannot_main_type ctx mt in
    let st = tannot_session_type ctx st in
    STRecv (mt, st)
| STRec (x, st) -> 
    let st = tannot_session_type ctx st in
    STRec (x, st)
| STInline x -> STInline x 
and tannot_session_type ctx st : session_type = tannot_place (_tannot_session_type ctx) st


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
| CType ct -> CType (tannot_composed_type ctx ct)
| SType st -> SType (tannot_session_type ctx st)
| CompType ct -> CompType (tannot_component_type ctx ct)
| ConstrainedType (mt, guard) -> ConstrainedType (
    tannot_main_type ctx mt, 
    tannot_applied_constraint ctx guard)
and tannot_main_type ctx mt = {
    place = mt.place;
    value = _tannot_main_type ctx mt.place mt.value
}

(******************************** Constraints ********************************)

and _tannot_constraint_header ctx place = function
| UseGlobal (mt, x) -> 
    UseGlobal (
        tannot_main_type ctx mt,
        x
    )
| UseMetadata (mt, x) -> 
    UseMetadata (
        tannot_main_type ctx mt,
        x
    )
| SetTimer x -> SetTimer (x)
| SetFireTimer (x, i) -> SetFireTimer (x, i)
and tannot_constraint_header ctx h= {
    place = h.place;
    value = _tannot_constraint_header ctx h.place h.value
}

and tannot_applied_constraint ctx (headers, guard_opt) = 
    List.map (tannot_constraint_header ctx) headers, Option.map (tannot_expr ctx) guard_opt

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
        | LambdaExpr (x, mt, stmt) -> failwith "TODO stmt -> expr in all src/" 
        (*
            let stmt, mt_stmt = tannot_expr ctx stmt in
            LambdaExpr (x, mt, stmt), TArrow (mt, mt_stmt) 
            *)
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

and _tannot_stmt ctx place : _stmt -> _stmt = function
| EmptyStmt -> EmptyStmt
| AssignExpr (x, e) -> AssignExpr (
    x,
    tannot_expr ctx e
)
| AssignThisExpr (x, e) -> AssignThisExpr (
    x,
    tannot_expr ctx e
)
| LetExpr _ -> failwith "binders"
| CommentsStmt c -> CommentsStmt c

| BreakStmt -> BreakStmt
| ContinueStmt -> ContinueStmt
| ExitStmt i -> ExitStmt i
| ForStmt (mt, x, e, stmt) -> ForStmt (
    tannot_main_type ctx mt,
    x,
    tannot_expr ctx e,
    tannot_stmt ctx stmt
)
| IfStmt (e, stmt1, stmt2_opt) -> IfStmt (
    tannot_expr ctx e,
    tannot_stmt ctx stmt1,
    Option.map (tannot_stmt ctx) stmt2_opt
)
| MatchStmt (e, branches) -> MatchStmt (
    tannot_expr ctx e,
    List.map (function (e, stmt) -> 
        tannot_expr ctx e, tannot_stmt ctx stmt
    ) branches
)
| ReturnStmt e -> ReturnStmt (tannot_expr ctx e)
| ExpressionStmt e -> ExpressionStmt (tannot_expr ctx e)
| BlockStmt stmts -> 
    let stmts = List.map (tannot_stmt ctx) stmts in
    BlockStmt stmts
and tannot_stmt ctx stmt =  failwith ""(*tannot_place _tannot_stmt ctx stmt*)

and _tannot_param ctx place (mt, x) = ( tannot_main_type ctx mt, x)
and tannot_param ctx arg = {
    place = arg.place;
    value = _tannot_param ctx arg.place arg.value
}

and _tannot_port ctx place (p:_port) = {
    name = p.name;
    input = tannot_expr ctx p.input;
    expecting_st = tannot_main_type ctx p.expecting_st;
    callback = tannot_expr ctx p.callback;
} 
and tannot_port ctx p = {
    place = p.place;
    value = _tannot_port ctx p.place p.value
}


and _tannot_contract ctx place (p:_contract) = {
    method_name = p.method_name;
    pre_binders = List.map (function (mt, x, e) -> 
        tannot_main_type ctx mt,
        x,
        tannot_expr ctx e
    ) p.pre_binders;
    ensures = Option.map (tannot_expr ctx) p.ensures;
    returns = Option.map (tannot_expr ctx) p.returns;
} 
and tannot_contract ctx c = {
    place = c.place;
    value = _tannot_contract ctx c.place c.value
}

and _tannot_method ctx place = function
| CustomMethod m -> CustomMethod {
    name = m.name;
    ghost = m.ghost;
    ret_type = tannot_main_type ctx m.ret_type;
    args =  List.map (tannot_param ctx) m.args;
    body = List.map (tannot_stmt ctx) m.body;
    contract_opt = Option.map (tannot_contract ctx) m.contract_opt;
} 
| OnStartup m -> OnStartup (tannot_method ctx m)
| OnDestroy m -> OnDestroy (tannot_method ctx m)
and tannot_method ctx m = {
    place = m.place;
    value = _tannot_method ctx m.place m.value
}

and _tannot_state ctx place = function 
| StateDcl s -> StateDcl {
    name = s.name;
    ghost = s.ghost;
    type0 = tannot_main_type ctx s.type0;
    body = Option.map (tannot_expr ctx) s.body;
} 
and tannot_state ctx s = {
    place = s.place;
    value = _tannot_state ctx s.place s.value
}

and _tannot_component_item ctx place = function 
| Contract s -> Contract (tannot_contract ctx s)
| Include ce -> Include (tannot_component_expr ctx ce)
| Method m -> Method (tannot_method ctx m)
| Port p -> Port (tannot_port ctx p)
| State s -> State (tannot_state ctx s)
| Term t -> Term (tannot_term ctx t)
and tannot_component_item ctx citem = {
    place = citem.place;
    value = _tannot_component_item ctx citem.place citem.value
}

and _tannot_component_dcl ctx place = function 
| ComponentStructure cdcl -> ComponentStructure {
    target_name = cdcl.target_name;
    name = cdcl.name;
    args = List.map (tannot_param ctx) cdcl.args;
    body = List.map (tannot_component_item ctx) cdcl.body
} 
| ComponentAssign cdcl -> ComponentAssign {
    name = cdcl.name;
    args = List.map (tannot_param ctx) cdcl.args;
    value = tannot_component_expr ctx cdcl.value;
} 
and tannot_component_dcl ctx cdcl = {
    place = cdcl.place;
    value = _tannot_component_dcl ctx cdcl.place cdcl.value
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

and _tannot_function_dcl ctx place (fdcl:_function_dcl) : _function_dcl = {
    name = fdcl.name;
    ret_type = tannot_main_type ctx fdcl.ret_type;
    args = List.map (tannot_param ctx) fdcl.args;
    body = List.map (tannot_stmt ctx) fdcl.body;
} 
and tannot_function_dcl ctx fdcl = {
    place = fdcl.place; 
    value = _tannot_function_dcl ctx fdcl.place fdcl.value
}

and _tannot_typedef ctx place = function 
| ClassicalDef (x, mts, body) -> ClassicalDef (
    x,
    List.map (tannot_main_type ctx) mts,
    body
) 
| EventDef (x, mts, body) -> EventDef (
    x,
    List.map (tannot_main_type ctx) mts,
    body
)  
| ProtocolDef (x, mt) -> ProtocolDef (
    x,
    tannot_main_type ctx mt
)  
and tannot_typedef ctx tdef = {
    place = tdef.place;
    value = _tannot_typedef ctx tdef.place tdef.value
}

and _tannot_term ctx place = function 
| EmptyTerm -> EmptyTerm
| Comments c -> Comments c
| Stmt stmt -> 
    let stmt = tannot_stmt ctx stmt in
    Stmt stmt
| Component c -> Component (tannot_component_dcl ctx c)
| Function f -> Function (tannot_function_dcl ctx f)
| Typealias (x, body) -> Typealias (
    x,
    Option.map (tannot_main_type ctx) body
)
| Typedef tdef -> Typedef (tannot_typedef ctx tdef)
and tannot_term ctx t = {
    place = t.place;
    value = _tannot_term ctx t.place t.value
}

and tannot_program program = 
    List.map (tannot_term (fresh_context ())) program