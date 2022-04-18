open Utils
open Error
open Easy_logging
open Fieldslib
open AstUtils
open TypingUtils
open IRMisc

module S = IR
module T = IR
open IR

let logger = Logging.make_logger "_1_ compspec.TypeChecking" Debug [];;


(*
TODO FIXME tcheck_... should retourn unit or raise an error
*)

let fplace = (Error.forge_place "TypeChecking" 0 0)
let auto_fplace smth = {place = fplace; value=smth}


(************************************ Types **********************************)

(* we need to find constraints to type check them *)
let rec _tcheck_composed_type ct = function 
| TActivationRef mt -> tcheck_main_type mt
| TArrow (mt1, mt2) -> 
    tcheck_main_type mt1; 
    tcheck_main_type mt2
| TVar x -> () 
| TFlatType ft -> () 
| TArray mt -> tcheck_main_type mt
| TDict (mt1, mt2) | TResult (mt1, mt2) | TUnion (mt1, mt2)->
    tcheck_main_type mt1;
    tcheck_main_type mt2
| TList mt | TOption mt | TSet mt | TVPlace mt-> tcheck_main_type mt
| TTuple mts -> List.iter tcheck_main_type mts
| TBridge b ->
    tcheck_main_type b.in_type;
    tcheck_main_type b.out_type;
    tcheck_main_type b.protocol
and tcheck_composed_type ct = map0_place _tcheck_composed_type ct

and _tcheck_session_type place = function
| STEnd -> () 
| STVar _ -> () 
| STSend (mt, st) | STRecv (mt, st) -> 
    tcheck_main_type mt;
    tcheck_session_type st
| STRec (_, st) -> tcheck_session_type st 
| STInline _ -> () 
and tcheck_session_type st = map0_place _tcheck_session_type st

and _tcheck_component_type place = function
| CompTUid _ -> () 
| TStruct (_, sign) -> Atom.VMap.iter (function _ -> tcheck_main_type) sign 
| TPolyCVar _ -> () 
and tcheck_component_type ct = map0_place _tcheck_component_type ct 

and _tcheck_main_type place = function
| CType ct -> tcheck_composed_type ct
| SType st -> tcheck_session_type st
| CompType ct -> tcheck_component_type ct
| ConstrainedType (mt, guard) -> 
    tcheck_main_type mt;
    tcheck_applied_constraint guard
and tcheck_main_type mt = map0_place _tcheck_main_type mt 

(******************************** Constraints ********************************)

and _tcheck_constraint_header place = function
| UseMetadata (mt, x) -> tcheck_main_type mt
| SetTimer x -> () 
| SetFireTimer (x, i) -> () 
and tcheck_constraint_header h= map0_place _tcheck_constraint_header h

and tcheck_applied_constraint (headers, guard_opt) = 
    List.iter tcheck_constraint_header headers;
    Option.iter tcheck_expr guard_opt

(************************************ (V) - Place ****************************)

and tcheck_vplace (vp:vplace) = tcheck_expr vp.nbr_instances

(************************************* Literals ******************************)

and _tcheck_literal place = function
| VPlace vp -> tcheck_vplace vp
| _ -> () 
and tcheck_literal lit = tcheck_literal lit

and tcheck_binop place ret_mt op mt_e1 mt_e2 = 
match (op, mt_e1.value, mt_e2.value) with
| And, _,_ | Or, _, _ -> begin
    (*mt_ret == TBool by reconstruction algo *)
    if Bool.not (equal_mtype mt_e1 mt_e2) then
        Error.error place "Type error: this operation expect type equality (and not subtyping relation)";
    match mt_e1.value with 
    | CType{value=TFlatType TBool} -> () (*FIXME add subtype of bool *)
    | _ -> Error.error place "Type error: this operation expect boolean arguments" 
end
| StructuralEqual, _ ,_ | Equal, _, _ ->
    (*mt_ret == TBool by reconstruction algo *)
    if Bool.not (equal_mtype mt_e1 mt_e2) then (* No subtyping for equality *)
        Error.error place "Type error: equality expected type equality (and not subtyping relation)"
| GreaterThanEqual, _,_ | LessThanEqual, _,_ | GreaterThan, _, _ | LessThan, _, _ ->
    begin
    (*mt_ret == TBool by reconstruction algo *)
    if Bool.not (equal_mtype mt_e1 mt_e2) then (* No subtyping *) 
        Error.error place "Type error: this operation expect type equality (and not subtyping relation)";

    match mt_e1.value with
    | CType{value=TFlatType TInt} | CType{value=TFlatType TFloat} | CType{value=TFlatType TTimer}-> () (* Timer is just an int *) 
    | _ -> Error.error place "Type error: this operation is only defined for Int and Float" 
end
| In, _, CType{value=t} -> begin
    (*mt_ret == TBool by reconstruction algo *)
    match t with
    | TArray mt_elt | TList mt_elt | TSet mt_elt ->    
        if Bool.not (equal_mtype mt_elt mt_e1) then
            Error.error place "Type error: types mismatched (subtyping is forbiden)"
    | TDict (mt_key, mt_value) -> 
        if Bool.not (equal_mtype mt_key mt_e1) then
            Error.error place "Type error: types mismatched (subtyping is forbiden)"
    | _ -> Error.error place "[in] operation is not defined for this type of data structure" 
end
| _, CType{value=TFlatType TInt},CType{value=TFlatType TInt} -> begin   
    match op with
    | Plus | Minus | Mult | Divide -> 
        if Bool.not (equal_mtype ret_mt mt_e1 && equal_mtype mt_e1 mt_e2) then 
            Error.error place "Type error: types mismatched (subtyping is forbiden)"
end
| _, CType{value=TFlatType TFloat},CType{value=TFlatType TFloat} -> begin   
    match op with
    | Plus | Minus | Mult | Divide -> 
        if Bool.not (equal_mtype ret_mt mt_e1 && equal_mtype mt_e1 mt_e2) then 
            Error.error place "Type error: types mismatched (subtyping is forbiden)"
end

and tcheck_unop place ret_mt op mt_e = 
match (op, mt_e.value) with
| Not, CType{value=TFlatType TBool} -> 
    (*mt_ret == TBool by reconstruction algo *)
    () 
| Not, _ -> Error.error place "[not] expect a boolean expression"
| UnpackOrPropagateResult, CType{value=TResult (mt_res,_)} ->
    if Bool.not (equal_mtype mt_res mt_res) then
        Error.error place "Type error: types mismatched (subtyping is forbiden)"
| UnpackOrPropagateResult, _ -> Error.error place "[?] expect a result expression"


and check_call place mt_e = 
    let fplace = (Error.forge_place "Akka.GuardTransform.filter_headers" 0 0) in
    let auto_fplace smth = {place = fplace; value=smth} in
    let ctypeof ct = auto_fplace(CType(auto_fplace ct)) in
function
| mt1, [] -> 
    if Bool.not (equal_mtype mt_e {value=mt1;place}) then
        Error.error place "Type error: ret types mismatched (equality error)"
 
| CType{value=TArrow (mt1, mt2)}, mt3::mts -> 
    if Bool.not (is_subtype mt3 mt1 || is_instance mt3 mt1) then
        Error.error place "Type error: types mismatched (subtyping or instance error)"
    else
        check_call place mt_e (mt2.value, mts)
| mt0, mt3::mts  ->
    let cvar = Atom.fresh "'a" in
    let tconstraint = Equality ( auto_fplace mt0, (ctypeof (TArrow (mt3, ctypeof (TVar cvar))))) in
    ignore(mgu_solver place [tconstraint]);
    Error.error place "unification ok"
| _, _::_ -> Error.error place "Type error: called expr is not an arrow"
| _ -> Error.error place "Type error: wrong number of parameters"

and _tcheck_expr place (e, mt_e) =
    match e with
        | VarExpr x -> () 
        | AccessExpr (e1, e2) -> 
            (* Access type [mt_e] is already checked during type reconstruction - otherwise reconstruction can not be performed *) 
            tcheck_expr e1;
            tcheck_expr e2
        | BinopExpr (e1, op, e2) -> 
            tcheck_binop place mt_e op (snd e1.value) (snd e2.value)
        | LambdaExpr (params, e) -> 
            List.iter (map0_place (fun _ (mt,_) -> tcheck_main_type mt)) params;
            tcheck_expr e (* propagation of mt already done  by TypeInference and mt_e is the mt -> type of e by type inference *)
        | LitExpr l -> () 
        | UnopExpr (op, e) -> tcheck_unop place mt_e op (snd e.value)
        | CallExpr (e, es) | NewExpr (e, es) -> begin
           check_call place mt_e ((snd e.value).value, List.map (function e -> snd e.value) es)
        end
        | This -> () 
        | Spawn spawn -> begin 
            match spawn.at with 
            | None | Some {value=_,{value=CType {value=TFlatType (TPlace _)}}} -> 
                check_call place mt_e ((snd spawn.c.value).value, List.map (function e -> snd e.value) spawn.args)
            | _ -> Error.error place "Type error: spawn @ must be a place"
        end
        | BoxCExpr ce -> begin
            match (snd ce.value).value with
            | CompType _ -> tcheck_component_expr ce
            | _ -> Error.error place "Type error: boxing expects cexpr"
        end
        | OptionExpr None -> () 
        | OptionExpr Some e -> tcheck_expr e 
        | ResultExpr (e1_opt, e2_opt) -> begin
            match Option.map tcheck_expr e1_opt, Option.map tcheck_expr e2_opt with
            | Some b,_ | _, Some b -> b
        end
        | BlockExpr (b, es) -> begin
            match mt_e.value with 
            | CType{value=TList t_elt} | CType{value=TSet t_elt} -> 
                ignore(List.fold_left (fun previous_mt current -> 
                    if equal_mtype previous_mt (snd current.value) then
                        Error.error place "Type error: types mismatched (equality error)"
                    else
                        snd current.value
                ) t_elt es)
            | CType{value=TTuple _} -> () 
        end
        | Block2Expr (b, ees) -> begin
            match mt_e.value with 
            |  CType{value=TDict (t_key, t_elt)} -> 
                List.fold_left (fun (t_key, t_elt) (key,elt) -> 
                    equal_mtype t_key (snd key.value); 
                    equal_mtype t_elt (snd elt.value); snd key.value, snd elt.value
                )  (t_key, t_elt) ees;
                ()  
        end
and tcheck_expr (e:expr) : unit = map0_place _tcheck_expr e

and _tcheck_stmt ret_type_opt place : _stmt -> unit = function
| EmptyStmt -> () 
| AssignExpr (x, e) -> () (* Type checking in TypeInference -> FIXME to it here we need to have a context ..... but having context here is redundent ..... *) 
| AssignThisExpr (x, e) -> () (* cf. AssignExpr*) 
| LetStmt (mt, x, e) as ee -> 
    tcheck_main_type mt; 
    tcheck_expr e; 

    (* type of e must a subtype of mt or mt must be an instance of the general type of e *)
    if Bool.not (is_subtype (snd e.value) mt || is_instance mt (snd e.value)) then
        Error.error place "Type error: type mismatch (no equality, no subtyping relation) - let"
        (* The propagation of x:mt has been done by TypeInference to scope of the let *)
| CommentsStmt _ -> () 
| BreakStmt -> () 
| ContinueStmt -> () 
| ExitStmt i -> () 
| ForStmt (mt, x, e, stmt) -> begin
    match (snd e.value).value with 
    | CType{value=TList mt_elt } | CType{value=TSet mt_elt} ->
        tcheck_main_type mt;
        tcheck_stmt ret_type_opt stmt; (* The propagation of x:mt has been done by TypeInference to scope of the let *)

        if Bool.not (is_subtype mt_elt mt) then 
            Error.error place "Type error: type mismatch (no equality, no subtyping relation) - for"
    | _ -> Error.error e.place "Type error: this expression is not iterable"
end
| IfStmt (e, stmt1, stmt2_opt) -> begin 
    match (snd e.value).value with 
    | CType{value=TFlatType TBool} -> 
        Option.iter (tcheck_stmt ret_type_opt) stmt2_opt;
        tcheck_stmt ret_type_opt stmt1
    | _ -> Error.error e.place "Type error: if condition must be a boolean"
end
| MatchStmt (e, branches) -> failwith "TODO typecheck match" 
| ReturnStmt e -> begin
    tcheck_expr e;
    match ret_type_opt with 
    | None -> Error.error place "Type error: return statement can not be used outside a function or a method"
    | Some ret_type -> 
        if Bool.not (is_subtype (snd e.value) ret_type) then
            Error.error place "Type error: type mismatch (no equality, no subtyping relation) - return"
        else ()
end
| ExpressionStmt e -> tcheck_expr e
| BlockStmt stmts -> List.iter (tcheck_stmt ret_type_opt) stmts
(* ret_type_opt - expected return type if in method/function *)
and tcheck_stmt ret_type_opt stmt =  map0_place (_tcheck_stmt ret_type_opt) stmt

and _tcheck_param place (mt, x) = tcheck_main_type mt
and tcheck_param arg = map0_place _tcheck_param arg

and _tcheck_port place ((p, mt_p): _port * main_type) = 
    tcheck_expr p.callback;

    (match mt_p.value with 
    | CType{value=TInport mt_st} -> begin 
        (* Just checking that the Type reconstruction is correct *)
        assert(equal_mtype mt_st p.expecting_st);

        (match mt_st.value with 
        | SType st -> 
            match (unfold_st_star st).value with 
            | STRecv (mt_msg, mt_continuation) -> begin 
                match (snd p.callback.value).value with
                | CType{value=TArrow (mt1, {value=CType{value=TArrow (mt2, {value=CType{value=TFlatType TBool}})}})} ->
                    if Bool.not (is_subtype mt_msg mt1 &&
                    is_subtype (auto_fplace (SType mt_continuation)) mt2) then (* continuation is subtype *)
                        Error.error place "Type error: types mismatched"

                | _ -> Error.error place "Type error: port callback must be of type msg -> continuation -> bool"
            end
            | STBranch _ -> failwith "TODO tcheck_port STBranch"
            | _ -> Error.error place "Type error: should expected an incomming label or message"
        | _-> Error.error p.expecting_st.place "Type error: must be a session type"
        )
    end
    )
and tcheck_port p = map0_place _tcheck_port p


and _tcheck_outport place (p, mt_p) = 
    (match mt_p.value with 
    | CType{value=TOutport} -> () 
    | _ -> Error.error place "outport must have type outport (internal error)"
    )
and tcheck_outport p = map0_place _tcheck_outport p

and _tcheck_contract place (p:_contract) = 
    List.iter (function (mt, x, e) -> 
        tcheck_main_type mt; 
        if Bool.not (is_subtype (snd e.value) mt) then
            Error.error place "Type error: types mismatched (equality error)"
            (* The propagation of x:mt has been done by TypeInference to scope of the binder *)
    ) p.pre_binders;
    Option.map tcheck_expr p.ensures;
    Option.map tcheck_expr p.returns;
    ()
and tcheck_contract c = map0_place _tcheck_contract c

and _tcheck_method place (m:_method0) = 
    tcheck_main_type m.ret_type;
    List.map tcheck_param m.args;
    List.map (tcheck_stmt(Some m.ret_type))  m.body; (* propagation of mt already done  by TypeInference and mt_e is the mt -> type of e by type inference *)
    Option.map tcheck_contract m.contract_opt;
    ()
and tcheck_method (m:method0) = map0_place _tcheck_method m

and _tcheck_state place = function 
| StateDcl s ->
    tcheck_main_type s.type0;
    match s.body with
    | None -> ()
    | Some e -> 
        tcheck_expr e;
        if Bool.not (is_subtype (snd e.value) s.type0) then  
            Error.error place "Type error: types mismatched (subtyping error)"
and tcheck_state s = map0_place _tcheck_state s

and _tcheck_component_item place = function 
| Contract s -> tcheck_contract s
| Include ce -> tcheck_component_expr ce
| Method m -> tcheck_method m
|Inport p -> tcheck_port p
| State s -> tcheck_state s
| Term t -> tcheck_term t
and tcheck_component_item citem= map0_place _tcheck_component_item citem


and _tcheck_component_dcl place = function 
| ComponentStructure cdcl ->
    List.iter tcheck_component_item cdcl.body
| ComponentAssign {value} -> 
    tcheck_component_expr value
and tcheck_component_dcl cdcl = map0_place _tcheck_component_dcl cdcl



(********************** Manipulating component structure *********************)
and _tcheck_component_expr place (ce, mt_ce)=
    match ce with 
        | VarCExpr x -> () 
        | AppCExpr (ce1, ce2) -> failwith "TODO tcheck cexpr AppCExpr" 
        | UnboxCExpr e -> begin
            match (snd e.value).value with
            | CompType _ -> tcheck_expr e
            | _ -> Error.error place "Type error: unboxing expects cexpr"
        end
        | AnyExpr e -> tcheck_expr e (*FIXME TODO used ??? or remove *)
and tcheck_component_expr ce = map0_place _tcheck_component_expr ce

(************************************ Program *****************************)

and _tcheck_function_dcl place (fdcl:_function_dcl) : unit = 
    tcheck_main_type fdcl.ret_type;
    List.iter tcheck_param fdcl.args;
    List.iter (tcheck_stmt(Some fdcl.ret_type)) fdcl.body (* propagation of mt already done  by TypeInference and mt_e is the mt -> type of e by type inference *)
and tcheck_function_dcl fdcl = map0_place _tcheck_function_dcl fdcl

and _tcheck_typedef place = function 
(* propagation of mt already done  by TypeInference *)
| ClassicalDef (x, mts, ()) -> 
    List.iter tcheck_main_type mts
| EventDef (x, mts, ()) -> 
    List.iter tcheck_main_type mts
| ProtocolDef (x, mt) ->
    tcheck_main_type mt
and tcheck_typedef tdef = map0_place _tcheck_typedef tdef

and _tcheck_term place = function 
(* propagation of binders already done  by TypeInference *)
| EmptyTerm -> () 
| Comments c -> ()
| Stmt stmt -> tcheck_stmt None stmt
| Component c -> tcheck_component_dcl c
| Function f -> tcheck_function_dcl f
| Typealias (x, body) -> Option.iter tcheck_main_type body
| Typedef tdef -> tcheck_typedef tdef
and tcheck_term t = map0_place _tcheck_term t

and tcheck_program program = 
    List.iter tcheck_term program;
    program

(**********************************************************)
let name = "TypeChecking"
let displayed_pass_shortdescription = "IR has been typed checked successfully"
let displayed_ast_name = "annotated IR (with types)"
let show_ast = true
let global_at_most_once_apply = false

let precondition program = program
let postcondition program = program
let apply_program = tcheck_program