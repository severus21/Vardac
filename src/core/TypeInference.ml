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

let fplace = (Error.forge_place "TypeInference" 0 0)
include AstUtils2.Mtype.Make(struct let fplace = fplace end)


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
| ActivationRef _ -> failwith "ActivationRef Typeinference - do we need this literal since it carries no value"
| Place _ -> failwith "Place do we need this literal since it can not exists statically"
| VPlace _-> 
    (* forall x, vplace<x> - x must be unified during typecheking *)
    let x = Atom.fresh "x" in
    ctypeof (TForall(x, ctypeof (TVPlace (ctypeof (TVar x)))))
| StaticBridge b -> failwith "How to infer type of Bridge" 

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

let typeof_arrow ret_type args= 
    let fplace = (Error.forge_place "TypeInference.typeof_arrow" 0 0) in
    let auto_fplace smth = {place = fplace; value=smth} in
    mtype_of_fun args ret_type 
let typeof_method (m:method0) = typeof_arrow m.value.ret_type m.value.args
let typeof_function fdcl = typeof_arrow fdcl.value.ret_type fdcl.value.args

let typeof_port p = 
    snd p.value (* Already computed from programmer annotation *)

let typeof_outport p = 
    snd p.value (* Already computed from programmer annotation *)
let typeof_state s = 
    match s.value with
    | StateDcl sdcl -> sdcl.type0
    | _ -> failwith "typeof_state state alias not supported"

(* Search for component definition
    We do not implement equi/iso recursive types
    we used named type for components and we unfold the definition when needed (e.g. subtyping)
    But we compute the signature of a component - based on named types - before doing more.
*)
let rec _shallow_scan_component_item ctx place = function
| Contract _ -> ctx, [] (*TODO*)
| Include _ -> ctx, []
| Method m -> register_expr_type ctx m.value.name (typeof_method m), [m.value.name, typeof_method m]
|Inport p -> register_expr_type ctx (fst p.value).name (typeof_port p), [(fst p.value).name, typeof_port p]
| Outport p -> register_expr_type ctx (fst p.value).name (typeof_outport p), [(fst p.value).name, typeof_outport p]
| State ({value=StateDcl {name}} as s)| State ({value=StateAlias {name}} as s) -> 
    register_expr_type ctx name (typeof_state s), [name, typeof_state s]
| Term t -> shallow_scan_term ctx t
and shallow_scan_component_item ctx = map0_place (_shallow_scan_component_item ctx)
and _shallow_scan_component_dcl ctx place = 
    let fplace = (Error.forge_place "TypeInference._tannot_constraint_header" 0 0) in
    let auto_fplace smth = {place = fplace; value=smth} in
function
| ComponentStructure cdcl -> begin
    logger#warning "collect %s" (Atom.to_string cdcl.name);
    let ctx, cstruct = List.fold_left_map shallow_scan_component_item ctx cdcl.body in (*TODO should we propagate this ctx ?*)
    let cstruct = Atom.VMap.of_seq (List.to_seq (List.flatten cstruct)) in
    let signature = auto_fplace(CompType(auto_fplace(TStruct cstruct))) in
    register_cexpr_type ctx cdcl.name signature, [cdcl.name, signature]
end
| ComponentAssign cdcl -> failwith "TypeInference shallow ComponentAssign not yet supported" 
and shallow_scan_component_dcl ctx = map0_place (_shallow_scan_component_dcl ctx)

and _shallow_scan_term ctx place = function 
| Component c -> shallow_scan_component_dcl ctx c 
| _ -> ctx, [] 
and shallow_scan_term ctx = map0_place (_shallow_scan_term ctx)

and top_shallow_scan_term ctx t = fst (shallow_scan_term ctx t) 



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
| STDual st -> 
    let ctx , st = tannot_full_session_type ctx st in
    ctx, STDual st
and tannot_full_session_type ctx st : context * session_type = 
    let ctx, _st = _tannot_session_type ctx st.place st.value in
    ctx, {place = st.place; value = _st}
and tannot_session_type ctx st = snd (tannot_full_session_type ctx st)


(* Searching for constraints *)
and _tannot_composed_type ctx place = function 
| TActivationRef mt -> TActivationRef (tannot_main_type ctx mt)
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
and tannot_component_type ctx ct = failwith "" (*map_place _tannot_component_type ctx ct*) 

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


and mt_of_citem ctx place mt_component mname = 
    let c_sign = match mt_component.value with
        | CompType {value=TStruct sign} -> sign
        | CompType {value=CompTUid name} -> begin 
            match (typeof_var_cexpr ctx name).value with 
            |  CompType {value=TStruct sign} -> sign
            | _ -> Error.error place "internal error when fetching structural type of component"
        end
        | _ -> Error.error place "This expr has no attributes" 
    in

    let ret_type = 
        match Atom.VMap.find_opt mname c_sign with
        | None -> raise (Error.PlacedDeadbranchError (place, (Printf.sprintf "The infered component have no field/method named %s" (Atom.to_string mname))))
        | Some mt -> mt
    in
    ret_type

and _tannot_expr ctx place (e, mt_e) =
    let fplace = (Error.forge_place "TypeInference.typeof_block" 0 0) in
    let auto_fplace smth = {place = fplace; value=smth} in
    let ctypeof x = auto_fplace (CType(auto_fplace x)) in

    (match mt_e.value with 
    | EmptyMainType -> begin 
        match e with  
            | VarExpr x -> 
                VarExpr x, typeof_var_expr ctx x 
            | ImplicitVarExpr x -> 
                ImplicitVarExpr x, typeof_var_expr ctx x 
            | ActivationAccessExpr (cname, e, mname) ->
                let e = tannot_expr ctx e in
                
                ActivationAccessExpr (cname, e, mname), mt_of_citem ctx place ( typeof_var_cexpr ctx cname) mname
            | AccessExpr (e1, e2) -> begin
                let e1 = tannot_expr ctx e1 in


                let field = match fst e2.value with 
                    | VarExpr f -> f 
                    | _ -> Error.error e2.place "This is not a valid attribute"
                    (* No means to add a type to e2 *)
                in
                
                let ret_type : main_type = 
                    (* Accessing inductive type parts - dirty hack *)
                    if List.mem (Atom.hint field) ["_0_"; "_1_"; "_2_"; "_3_"] then begin
                        let i = int_of_string (String.sub (Atom.hint field) 1 1)  in
                        match (snd e1.value).value with
                        | CType{value=TVar t1} -> begin 
                            match (defof_tvar ctx t1).value  with
                            | CType {value=TTuple targs} when List.length targs > i -> List.nth targs i 
                            | _ -> raise (Error.PlacedDeadbranchError (place, (Printf.sprintf "The infered inductive type has only %d parts" i)))
                        end
                        | _ -> raise (Error.PlacedDeadbranchError (place, (Printf.sprintf "The infered type is not an inductive type %s" (Atom.to_string field))))
                    end
                    else begin
                        mt_of_citem ctx place (snd e1.value) field
                    end
                in
                
                AccessExpr(e1, e2), ret_type 
            end
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
                    | CType{value=TForall(_, mt)} -> ret_typeof depth mt
                    | _ -> Error.error place "Function expect %d args, not %d" ((List.length es)-depth) (List.length es)
                in

                CallExpr(e, es), ret_typeof (List.length es) (snd e.value) 
            | NewExpr (e, es) -> 
                let e = tannot_expr ctx e in
                let es = List.map (tannot_expr ctx) es in
                let rec ret_typeof depth mt = match mt.value with (*TODO check types here ??*)
                (* TODO dedup with call expr*)
                    | _ when depth = 0 -> mt
                    | CType{value=TArrow (_, mt2)} -> ret_typeof (depth-1) mt2 
                    | CType{value=TForall(_, mt)} -> ret_typeof depth mt
                    | _ -> Error.error place "Type constructor expect %d args, not %d" ((List.length es)-depth) (List.length es)
                in
                NewExpr(e, es), ret_typeof (List.length es) (snd e.value)
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
            }, ctypeof(TActivationRef(snd c.value))
            | BridgeCall b -> failwith "How to infer type of Bridge" 
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
            | PolyApp (e, mts) -> 
                let e = tannot_expr ctx e in
                let mt = snd e.value in
                let rec apply = function
                | {value=CType{value=TForall (x, mt)}}, mt'::mts' ->
                    replace_type_main_type x (None, Some mt'.value) (apply (mt, mts'))
                | mt, [] -> mt  
                | mt,_ -> Error.error (place@mt.place) "Type specialization error"
                in
                (fst e.value, apply (mt, mts))
    end
    | CType{value=TVPlace _ } -> e, mt_e
    | other -> Error.error (place@mt_e.place) "Should not be hydrated %s\n%s" (show__expr e) (show__main_type other)
    );


and tannot_expr ctx e = {
    place = e.place;
    value = _tannot_expr ctx e.place e.value
}

and _tannot_stmt ctx place : _stmt -> context * _stmt = function
| EmptyStmt -> ctx, EmptyStmt
| AssignExpr (x, e) -> 
    let mt_x = typeof_var_expr ctx x in
    let e = tannot_expr ctx e in

    if Bool.not (is_subtype (snd e.value) mt_x) then
        Error.error place "Type error: types do not match";
    
    ctx, AssignExpr (x, e)
| AssignThisExpr (x, e) -> 
    let mt_x = typeof_var_expr ctx x in
    let e = tannot_expr ctx e in

    if Bool.not (is_subtype (snd e.value) mt_x) then
        Error.error place "Type error: types do not match";
    
    ctx, AssignThisExpr (x, e)
| LetStmt (mt, x, e) -> 
    let ctx = register_expr_type ctx x mt in
    let e = tannot_expr ctx e in 
    ctx, LetStmt (mt, x, e)
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
| WithContextStmt (anonymous_mod, cname, e, stmts) -> 
    (* From the outside WithContextStmt is transparent in term of ctx *)
    let ctx, stmts = List.fold_left_map tannot_stmt ctx stmts in
    ctx, WithContextStmt (anonymous_mod, cname, tannot_expr ctx e, stmts)
and tannot_stmt ctx stmt =  
    let ctx, _stmt = _tannot_stmt ctx stmt.place stmt.value in
    ctx, {place = stmt.place; value = _stmt }

and _tannot_param ctx place (mt, x) = ( tannot_main_type ctx mt, x)
and tannot_param ctx arg = {
    place = arg.place;
    value = _tannot_param ctx arg.place arg.value
}

and _tannot_port ctx place ((p, mt_p):_port*main_type) = ctx, {
    name = p.name;
    input = tannot_expr ctx p.input;
    expecting_st = tannot_main_type ctx p.expecting_st;
    callback = tannot_expr ctx p.callback;
} 
and tannot_port ctx p = 
    let fplace = (Error.forge_place "TypeInference.tannot_port" 0 0) in
    let auto_fplace smth = {place = fplace; value=smth} in
    let ctypeof x = auto_fplace (CType(auto_fplace x)) in

    let ctx, _p = _tannot_port ctx p.place p.value in
    let mt_port = ctypeof (TInport (
        snd _p.input.value,
        _p.expecting_st
    )) in
    (* TODO FIXME outer_ctx should be remove from here since it is already compute when doing the shallow_scan*)
    let outer_ctx = register_expr_type ctx _p.name mt_port in

    outer_ctx, {
        place = p.place;
        value = _p, mt_port 
    }

and _tannot_outport ctx place ((p, mt_p):_outport*main_type) = ctx, {
    name = p.name;
    input = tannot_expr ctx p.input;
} 
and tannot_outport ctx p = 
    let fplace = (Error.forge_place "TypeInference.tannot_outport" 0 0) in
    let auto_fplace smth = {place = fplace; value=smth} in
    let ctypeof x = auto_fplace (CType(auto_fplace x)) in

    let ctx, _p = _tannot_outport ctx p.place p.value in
    let mt_outport = ctypeof (TOutport (
        snd _p.input.value
    )) in
    (* TODO FIXME outer_ctx should be remove from here since it is already compute when doing the shallow_scan*)
    let outer_ctx = register_expr_type ctx _p.name mt_outport in

    outer_ctx, {
        place = p.place;
        value = _p, mt_outport 
    }

and _tannot_contract ctx ret_type place (p:_contract) = 
    let inner_ctx = List.fold_left (fun ctx (mt, x, _) -> register_expr_type ctx x mt) ctx p.pre_binders in
    {
        method_name = p.method_name;
        pre_binders = List.map (function (mt, x, e) -> 
            tannot_main_type ctx mt,
            x,
            tannot_expr ctx e
        ) p.pre_binders;
        ensures = Option.map (tannot_expr inner_ctx) p.ensures; 
        returns = match p.returns with
        | None -> None
        | Some e -> 
            let e, _ = (tannot_expr inner_ctx e).value in
            Some {place; value = e, {place; value=T.CType{place; value=T.TArrow (ret_type, {place; value=T.CType{place;value=T.TFlatType AstUtils.TBool}})}}}
    } 
and tannot_contract ctx ret_type c = {
    place = c.place;
    value = _tannot_contract ctx ret_type c.place c.value
}

and _tannot_method ctx place (m:_method0) =
    let fplace = (Error.forge_place "TypeInference.typeof_literal" 0 0) in
    let auto_fplace smth = {place = fplace; value=smth} in
    (* TODO FIXME outer_ctx should be remove from here since it is already compute when doing the shallow_scan*)
    let fct_sign = mtype_of_fun m.args m.ret_type in 
    let outer_ctx = register_expr_type ctx m.name fct_sign in
    let inner_ctx = List.fold_left (fun ctx {value=(mt, x)} -> register_expr_type ctx x mt) ctx m.args in
    
    outer_ctx, {
        m with
            ret_type = tannot_main_type ctx m.ret_type;
            args = List.map (tannot_param ctx) m.args;
            body = snd (List.fold_left_map tannot_stmt inner_ctx m.body);
            contract_opt =(Option.map (tannot_contract ctx m.ret_type) m.contract_opt);
    } 

and tannot_method ctx m = 
    let ctx, _m = _tannot_method ctx m.place m.value in
    ctx, {
        place = m.place;
        value = _m
    }

and _tannot_state ctx place = function 
| StateDcl s -> 
    (* TODO FIXME outer_ctx should be remove from here since it is already compute when doing the shallow_scan*)
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
| Contract s -> failwith  "contract must have been bounded to method before calling type inference"
| Include ce -> ctx, Include (tannot_component_expr ctx ce)
| Method m -> 
    let ctx, m = tannot_method ctx m in
    ctx, Method m 
|Inport p -> 
    let ctx, p = tannot_port ctx p in
    ctx,Inport p 
| Outport p -> 
    let ctx, p = tannot_outport ctx p in
    ctx, Outport p 
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

and _tannot_component_dcl ctx place = 
    let fplace = (Error.forge_place "TypeInference._tannot_component_dcl" 0 0) in
    let auto_fplace smth = {place = fplace; value=smth} in
function 
| ComponentStructure cdcl as c0 -> 
    let inner_ctx = register_self ctx  cdcl.name in 
    let inner_ctx = fst (shallow_scan_component_dcl inner_ctx {place; value=c0}) in
    let _, body = List.fold_left_map tannot_component_item  inner_ctx cdcl.body in 

    let outer_ctx = fst (shallow_scan_component_dcl ctx {place; value=c0}) in

    outer_ctx, ComponentStructure {
    target_name = cdcl.target_name;
    annotations = cdcl.annotations;
    name = cdcl.name;
    args = List.map (tannot_param ctx) cdcl.args;
    body =  body (* TODO first pass allow mutual recursive function ?? - only from header *)
} 
| ComponentAssign {name; value} -> ctx, ComponentAssign {
    name = name;
    value = tannot_component_expr ctx value;
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

    let ctx_with_targs = List.fold_left (fun ctx targ -> 
        if Str.string_match (Str.regexp "[A-Z].*") (Atom.hint targ) 0 then(
            register_cexpr_type ctx targ (auto_fplace(CompType(auto_fplace(TPolyCVar targ))))
        )else
            failwith "TODO how to specify the write number of constructor"
    ) ctx fdcl.targs in

    let fct_sign = mtype_of_fun fdcl.args fdcl.ret_type in 
    (* Adding forall targs on top of regular signature *)
    let fct_sign = List.fold_left (fun sign tvar -> 
        mtype_of_ct (TForall (tvar, sign))     
    ) fct_sign fdcl.targs in

    let outer_ctx = register_expr_type ctx fdcl.name fct_sign in
    let inner_ctx = List.fold_left (fun ctx {value=(mt, x)} -> register_expr_type ctx x mt) ctx_with_targs fdcl.args in
    
    outer_ctx, {
        name = fdcl.name;
        targs = fdcl.targs; (* TODO annote with type constraints ??*)
        ret_type = tannot_main_type ctx_with_targs fdcl.ret_type;
        args = List.map (tannot_param ctx_with_targs) fdcl.args;
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
| Derive derive ->
    let cargs = List.map (tannot_component_expr ctx) derive.cargs in
    let targs = List.map (tannot_main_type ctx) derive.targs in
    let eargs = List.map (tannot_expr ctx) derive.eargs in
    ctx, Derive {name = derive.name; cargs; targs; eargs} 
and tannot_term ctx t = 
    let ctx, _t = _tannot_term ctx t.place t.value in
    ctx, {
        place = t.place;
        value = _t 
    }

and tannot_program program = 
    (* Scan header for recursive definition of function, method and state *)
    let toplevel_ctx = List.fold_left top_shallow_scan_term (fresh_context ()) program in
    snd (List.fold_left_map tannot_term toplevel_ctx program) 


(**********************************************************)
let displayed_pass_shortdescription = "IR has been annotated with types (type reconstruction only)"
let displayed_ast_name = "annotated IR (with types)"
let show_ast = true
let precondition program = program
let postcondition program = program
let apply_program = tannot_program