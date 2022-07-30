open Easy_logging
open IR_common
open AstUtils

let logger = Easy_logging.Logging.make_logger ("vardac.CommonUtils") Info []
let fplace = (Error.forge_place "CommonUtils" 0 0) 
let auto_fplace smth = {place = fplace; value=smth}
include AstUtils2.Mtype.Make(struct let fplace = fplace end)

(* TODO get ride of return fvars and encode it as a selector collector if possible ?? links with stmt ?? *)
(* collector : env -> expr -> 'a list*)
let rec collect_expr_expr_ parent_opt (already_binded:Atom.Set.t) selector collector place (e,mt) : Atom.Set.t * 'a list * (main_type * expr_variable) list = 
    (* Collection *)
    let collected_elts0 = if selector e then collector parent_opt already_binded {place; value=(e,mt)} else [] in 

    (* Handling scope and propagation *)
    match e with 
    | LambdaExpr (params, e) ->
        let _, collected_elts1, fvars1 = List.fold_left (fun (set, collected_elts0, fvars0) {value=mt, x} -> 
            let _, collected_elts, fvars = collect_expr_mtype parent_opt set selector collector mt in
            set, collected_elts0@collected_elts, fvars0@fvars
        ) (already_binded, [], []) params in

        let inner_already_binded = List.fold_left (fun set {value=_,x} -> Atom.Set.add x set) already_binded params in


        let _, collected_elts2, fvars2 = collect_expr_expr parent_opt inner_already_binded selector collector e in

        already_binded, collected_elts0@collected_elts1@collected_elts2, fvars1@fvars2
    | (VarExpr x) | (ImplicitVarExpr x) when Atom.Set.find_opt x already_binded <> None  -> already_binded, collected_elts0, [] 
    | (VarExpr x) | (ImplicitVarExpr x) when Atom.is_builtin x -> already_binded, collected_elts0, [] 
    | (VarExpr x) | (ImplicitVarExpr x)-> already_binded, collected_elts0, [mt, x]
    | BridgeCall _ | BoxCExpr _ | EmptyExpr | LitExpr _ | OptionExpr None | ResultExpr (None, None) | RawExpr _ | This | Self -> already_binded, collected_elts0, []
    | AccessExpr (e1, {value=VarExpr _, _}) -> (* TODO AccessExpr : expr * Atom.t *)
        let _, collected_elts1, fvars1 = collect_expr_expr parent_opt already_binded selector collector e1 in
        already_binded, collected_elts0@collected_elts1, fvars1

    | AccessExpr (e1, e2) | BinopExpr (e1, _, e2) | ResultExpr (Some e1, Some e2) ->
        let _, collected_elts1, fvars1 = collect_expr_expr parent_opt already_binded selector collector e1 in
        let _, collected_elts2, fvars2 = collect_expr_expr parent_opt already_binded selector collector e2 in
        already_binded, collected_elts0@collected_elts1@collected_elts2, fvars1@fvars2
    | ActivationAccessExpr (_, e, _) | UnopExpr (_, e) | OptionExpr (Some e) | ResultExpr (Some e, None) | ResultExpr (None, Some e)->
        let _, collected_elts, fvars = collect_expr_expr parent_opt already_binded selector collector e in
        already_binded, collected_elts0@collected_elts, fvars
    | CallExpr ({value=(VarExpr _,_) }, es) | NewExpr ({value=(VarExpr _, _)}, es) | Create {args=es} -> (* no first class function nor constructor inside stmt - so we get ride of all possible constructors *)
        let collected_elts, fvars = List.fold_left (fun (acc0, acc1) e -> 
            let _, collected_elts, fvars = collect_expr_expr parent_opt already_binded selector collector e in
            collected_elts@acc0, fvars@acc1
        ) ([], []) es in 
        already_binded, collected_elts0@collected_elts, fvars
    | CallExpr (e, es) | NewExpr (e, es) | Spawn {args=es; at = Some e} | Spawn {args=es; inline_in = Some e}->
        (* TODO factorization create a collect_expr_exprs like the collect_types_mtypes *)
        let _, collected_elts1, fvars1 = collect_expr_expr parent_opt already_binded selector collector e in
        let collected_elts2, fvars2 = List.fold_left (fun (acc0, acc1) e -> 
            let _, collected_elts, fvars = collect_expr_expr parent_opt already_binded selector collector e in
            collected_elts@acc0, fvars@acc1
        ) ([], []) es
        in
        already_binded, collected_elts0@collected_elts1@collected_elts2, fvars1@fvars2
    | BlockExpr (_, es) | Spawn {args=es} -> 
        let collected_elts, fvars = List.fold_left (fun (acc0, acc1) e -> 
            let _, collected_elts, fvars = collect_expr_expr parent_opt already_binded selector collector e in
            collected_elts@acc0, fvars@acc1) (collected_elts0, []) es
        in
        already_binded, collected_elts, fvars
    | Block2Expr (_, ees) ->
        let collect_expr_exprs = List.fold_left (fun (acc0, acc1) e -> 
            let _, collected_elts, fvars = collect_expr_expr parent_opt already_binded selector collector e in
            collected_elts@acc0, fvars@acc1) (collected_elts0, [])
        in
        let es1, es2 = List.split ees in
        let collected_elts1, fvars1 = collect_expr_exprs es1 in 
        let collected_elts2, fvars2 = collect_expr_exprs es2 in
        already_binded, collected_elts1@collected_elts2, fvars1@fvars2
    | InterceptedActivationRef (e1, e2_opt) ->
        let _, collected_elts1, fvars1 = collect_expr_expr parent_opt already_binded selector collector e1 in
        let _, collected_elts2, fvars2 = match e2_opt with
            | None -> Atom.Set.empty, [], []
            | Some e2 -> collect_expr_expr parent_opt already_binded selector collector e2 
        in
        already_binded, collected_elts0@collected_elts1@collected_elts2, fvars1@fvars2
    | TernaryExpr (e1, e2, e3) ->
        let _, collected_elts1, fvars1 = collect_expr_expr parent_opt already_binded selector collector e1 in
        let _, collected_elts2, fvars2 = collect_expr_expr parent_opt already_binded selector collector e2 in
        let _, collected_elts3, fvars3 = collect_expr_expr parent_opt already_binded selector collector e3 in
        already_binded, collected_elts0@collected_elts1@collected_elts2@collected_elts3, fvars1@fvars2@fvars3
and collect_expr_expr (parent_opt:Atom.atom option) (already_binded:Atom.Set.t) (selector:_expr->bool) (collector:Atom.atom option -> Atom.Set.t -> expr -> 'a list) (expr:expr) = 
    map0_place (collect_expr_expr_ parent_opt already_binded selector collector) expr
and free_vars_expr already_binded e = 
    let already_binded, _, fvars = collect_expr_expr None  already_binded (function e -> false) (fun parent_opt env e -> []) e in
    already_binded, Utils.deduplicate snd fvars 

(* TODO FIXME to be used in side fvars_expr/stmt *)
and collect_expr_mtype_ parent_opt already_binded selector collector place = function
| EmptyMainType -> already_binded, [], []
| CType ct ->   already_binded, [], [] (*TODO FIXME*)
| SType st -> already_binded, [], []
| CompType cmt ->already_binded, [], []
| ConstrainedType _ ->already_binded, [], []
| TRaw _ ->already_binded, [], []
and collect_expr_mtype parent_opt (already_binded:Atom.Set.t) selector collector mt =       
    map0_place (collect_expr_mtype_ parent_opt already_binded selector collector) mt 
and free_vars_mtype already_binded mt =
    let already_binded, _, fvars = collect_expr_mtype None  already_binded (function e -> false) (fun parent_opt env e -> []) mt in
    already_binded, Utils.deduplicate snd fvars 

and collect_expr_stmt_ parent_opt (already_binded:Atom.Set.t) selector collector place = 
    let collect_stmts already_binded stmts : Atom.Set.t * (('a list * (main_type*expr_variable) list)) list = 
        List.fold_left_map (fun already_binded stmt  -> 
            let already_binded, collected_elts, fvars = collect_expr_stmt parent_opt already_binded selector collector stmt in
            already_binded, (collected_elts, fvars)
        ) already_binded stmts
    in
function 
| EmptyStmt -> already_binded, [], []
| AssignExpr (x, e) ->
    collect_expr_expr parent_opt already_binded selector collector e
| AssignThisExpr (x, e) | AssignSelfExpr (x, e)->
    collect_expr_expr parent_opt already_binded selector collector e

| BranchStmt {s; label; branches} ->
    let _, collected_elts1, fvars1 = collect_expr_expr parent_opt already_binded selector collector s in
    let _, collected_elts2, fvars2 = collect_expr_expr parent_opt already_binded selector collector label in
    let collected_elts3, fvars3 = List.split (List.map (function {branch_s; body} -> 
        (* branch_s is a binder *)
        let inner_binded = Atom.Set.add branch_s already_binded in
        let _, collect_elts, fvars = collect_expr_stmt parent_opt inner_binded selector collector body in

        collect_elts, fvars
    ) branches)
    in
    already_binded, collected_elts1@collected_elts2@(List.flatten collected_elts3),  fvars1@fvars2@(List.flatten fvars3)
| BlockStmt stmts ->
    let _, res = collect_stmts already_binded stmts in 
    let collected_elts = List.map fst res in
    let fvars = List.map snd res in
    already_binded, List.flatten collected_elts, List.flatten fvars
| BreakStmt -> already_binded, [], []
| CommentsStmt c -> already_binded, [], []
| ContinueStmt -> already_binded, [], []
| ExpressionStmt e -> 
    let _, collected_elts, fvars = collect_expr_expr parent_opt already_binded selector collector e in
    already_binded, collected_elts, fvars
| ExitStmt _ -> already_binded, [], []
| ForeachStmt (mt, x, e, stmt) ->
    let _, collected_elts1, fvars1 = collect_expr_expr parent_opt already_binded selector collector e in
    let _, collected_elts2, fvars2 = collect_expr_stmt parent_opt (Atom.Set.add x already_binded) selector collector stmt in
    already_binded, collected_elts1@collected_elts2,  fvars1@fvars2
| GhostStmt stmt -> 
    let _, collected_elts, fvars = collect_expr_stmt parent_opt already_binded selector collector stmt in
    already_binded, collected_elts, fvars
| IfStmt (e, stmt1, stmt2_opt) -> begin 
    let _, collected_elts0, fvars0 = collect_expr_expr parent_opt already_binded selector collector e in
    let _, collected_elts1, fvars1 = collect_expr_stmt parent_opt already_binded selector collector stmt1 in

    match Option.map (collect_expr_stmt parent_opt already_binded selector collector) stmt2_opt with
    | None ->already_binded, collected_elts0@collected_elts1, fvars0@fvars1
    | Some (_, collected_elts2, fvars2) -> already_binded, collected_elts0@collected_elts1@collected_elts2,fvars0@fvars1@fvars2
end
| LetStmt (ct, x, e) -> 
    let already_binded = Atom.Set.add x already_binded in
    let _, collected_elts, fvars = collect_expr_expr parent_opt already_binded selector collector e in  
    already_binded, collected_elts, fvars 
| MatchStmt (e, branches) ->
    let _, collected_elts, fvars = collect_expr_expr parent_opt already_binded selector collector e in
    let collected_elts1, fvars1 = List.fold_left (fun (acc0, acc1) (_,stmt) -> 
        let _, collected_elts, fvars =  collect_expr_stmt parent_opt already_binded selector collector stmt in
        acc0@collected_elts, acc1@fvars
    ) (collected_elts, fvars) branches in
    already_binded, collected_elts1, fvars1 
| ReturnStmt e ->
    let _, collected_elts, fvars = collect_expr_expr parent_opt already_binded selector collector e in
    already_binded, collected_elts, fvars
| WithContextStmt (anonymous_mod, cname, e, stmts) -> 
    let _, collected_elts1, fvars1 = collect_expr_expr parent_opt already_binded selector collector e in
    let already_binded, res = collect_stmts already_binded stmts in
    let collected_elts2, fvars2 = List.split res in

    already_binded, collected_elts1@(List.flatten collected_elts2), fvars1@(List.flatten fvars2)

and collect_expr_stmt parent_opt (already_binded:Atom.Set.t) selector collector stmt =       
    map0_place (collect_expr_stmt_ parent_opt already_binded selector collector) stmt

and free_vars_stmt already_binded stmt = 
    let already_binded, _, fvars = collect_expr_stmt None  already_binded (function e -> false) (fun parent_opt env e -> []) stmt in
    already_binded, Utils.deduplicate snd fvars 

let rec collect_cexpr_cexpr_ (parent_opt:Atom.atom option) (already_binded:Atom.Set.t) selector collector place = 
    let collect_cexpr = collect_cexpr_cexpr parent_opt already_binded selector collector in
function 
| cexpr, _ when selector cexpr -> collector parent_opt already_binded place cexpr

(* Propagation *)
| VarCExpr _, _ -> []
| AppCExpr (ce1, args), _ -> (collect_cexpr ce1) @ (List.flatten (List.map collect_cexpr args))
| UnboxCExpr e, _ | AnyExpr e, _ -> collect_cexpr_expr parent_opt already_binded selector collector e

and collect_cexpr_cexpr parent_opt already_binded selector collector cexpr =       
    map0_place (collect_cexpr_cexpr_ parent_opt already_binded selector collector) cexpr

and collect_cexpr_expr_ (parent_opt:Atom.atom option) already_binded selector (collector) place e = 
    let selector_e = function
        | Spawn _  | BoxCExpr _ -> true
        | _ -> false
    in
    let collector_e _ _ = function
        | {value=Spawn {c}, _}  | {value=BoxCExpr c, _} -> [c]
    in
    let _, ces, _ = collect_expr_expr parent_opt Atom.Set.empty selector_e collector_e {place; value=e} in

    List.flatten (List.map (collect_cexpr_cexpr parent_opt already_binded selector collector) ces)


and collect_cexpr_expr parent_opt already_binded selector collector e =       
    map0_place (collect_cexpr_expr_ parent_opt already_binded selector collector) e

and collect_cexpr_stmt_ parent_opt already_binded selector collector place stmt = 
    let selector_e = function
        | Spawn _  | BoxCExpr _ -> true
        | _ -> false
    in
    let collector_e _ _ = function
        | {value=Spawn {c}, _}  | {value=BoxCExpr c, _} -> [c]
    in
    let _, collected_elts, _ = collect_expr_stmt parent_opt already_binded selector_e collector_e {place; value=stmt} in

    List.flatten (List.map (collect_cexpr_cexpr parent_opt already_binded selector collector) collected_elts)
and collect_cexpr_stmt parent_opt already_binded selector collector stmt =       
    map0_place (collect_cexpr_stmt_ parent_opt already_binded selector collector) stmt

let rec collect_stmt_stmt_ parent_opt selector collector place = 
    let collect_stmt = collect_stmt_stmt parent_opt  selector collector in
function 
| stmt when selector stmt -> collector parent_opt place stmt

(* Propagation *)
| EmptyStmt | AssignExpr _ | AssignThisExpr  _ | AssignSelfExpr _ | BreakStmt | CommentsStmt _ | ContinueStmt | ExpressionStmt _ | ExitStmt _ | LetStmt _ | ReturnStmt _-> []
| BlockStmt stmts | WithContextStmt (_, _, _, stmts) ->
    List.flatten (List.map collect_stmt stmts) 
| ForeachStmt (_, _, _, stmt) | GhostStmt stmt  ->
    collect_stmt stmt
| IfStmt (_, stmt1, stmt2_opt) -> begin 
    let collected_elts1 = collect_stmt stmt1 in

    match Option.map collect_stmt stmt2_opt with
    | None -> collected_elts1 
    | Some collected_elts2 -> collected_elts1@collected_elts2
end
| MatchStmt (_, branches) ->
    List.flatten (List.map collect_stmt (List.map snd branches))
and collect_stmt_stmt parent_opt selector collector stmt =       
    map0_place (collect_stmt_stmt_ parent_opt selector collector) stmt


(*retrun free  type variable *)
let rec collect_type_ctype_ flag_tcvar parent_opt already_binded selector collector place = 
    let collect_mtype = collect_type_mtype ~flag_tcvar:flag_tcvar parent_opt already_binded selector collector in    
    
function
| TFlatType _ -> already_binded, [], []
| TActivationRef mt | TArray mt | TList mt | TOption mt | TSet mt | TVPlace mt | TInport mt | TEport mt | TOutport mt -> collect_mtype  mt
| TArrow (mt1, mt2) | TDict (mt1, mt2) | TResult (mt1, mt2) | TUnion (mt1, mt2) -> 
    let _, collected_elts1, ftvars1 = collect_mtype mt1 in
    let _, collected_elts2, ftvars2 = collect_mtype mt2 in
    already_binded, collected_elts1@collected_elts2, ftvars1@ftvars2
| TBridge b -> 
    let _,  collected_elts1, ftvars1 = collect_mtype b.in_type in
    let _,  collected_elts2, ftvars2 = collect_mtype b.out_type in
    let _,  collected_elts3, ftvars3 = collect_mtype b.protocol in
    already_binded,  collected_elts1@collected_elts2@collected_elts3, ftvars1@ftvars2@ftvars3
| TTuple mts | TInductive mts -> 
    let collected_elts, ftvars = collect_type_mtypes flag_tcvar parent_opt already_binded selector collector mts in
    already_binded, collected_elts, ftvars
| TVar x | TPolyVar x when Atom.Set.find_opt x already_binded <> None  -> already_binded, [], [] 
| TVar x when Atom.is_builtin x -> already_binded, [], [] 
| TVar x | TPolyVar x | TObject x -> 
    already_binded, [], [x]
| TForall (x, mt) -> 
    let inner_already_binded = Atom.Set.add x already_binded in
    let _,  collected_elts, ftvars = collect_type_mtype ~flag_tcvar:flag_tcvar parent_opt inner_already_binded selector collector mt in
    assert ( false = List.mem x ftvars);
    already_binded, collected_elts, ftvars
and collect_type_ctype flag_tcvar parent_opt already_binded selector collector ct =       
    map0_place (collect_type_ctype_ flag_tcvar parent_opt already_binded selector collector) ct 
and free_tvars_ctype flag_tcvar already_binded ct = 
    let already_binded, _, ftvars = collect_type_ctype flag_tcvar None  already_binded (function e -> false) (fun parent_opt env e -> []) ct in
    already_binded, Utils.deduplicate Fun.id ftvars

and collect_type_stype_ flag_tcvar parent_opt already_binded selector collector place = function
| STEnd | STWildcard | STBottom -> already_binded, [], []
| STVar x | STPolyVar x when Atom.Set.find_opt x already_binded <> None  -> already_binded, [], []
| STVar x | STPolyVar x -> already_binded, [], [x]
| STRecv (mt, st) | STSend (mt, st) -> 
    let _, collected_elts1, ftvars1 = collect_type_mtype ~flag_tcvar:flag_tcvar parent_opt already_binded selector collector mt in
    let _, collected_elts2, ftvars2 = collect_type_stype flag_tcvar parent_opt already_binded selector collector st in
    already_binded, collected_elts1@collected_elts2, ftvars1@ftvars2
| STBranch branches | STSelect branches -> 
    let sts =  List.map (function (_,st,_) -> st) branches in
    let acs =  List.map Option.get (List.filter (function x -> x <> None) (List.map (function (_,_,ac) -> ac) branches)) in
    let collected_elts1, ftvars1 = collect_type_stypes flag_tcvar parent_opt already_binded selector collector sts in
    let collected_elts2, ftvars2 = collect_type_aconstraints flag_tcvar parent_opt already_binded selector collector acs in
    already_binded, collected_elts1@collected_elts2, ftvars1@ftvars2 
| STRec (x, st) -> 
    let inner_already_binded = (Atom.Set.add x already_binded) in
    let _, collected_elts, ftvars = collect_type_stype flag_tcvar parent_opt inner_already_binded selector collector st in
    already_binded, collected_elts, ftvars
| STDual st -> collect_type_stype flag_tcvar parent_opt already_binded selector collector st
| STInline x -> raise (Error.DeadbranchError (Printf.sprintf "STInline should have been resolved before running collect_type_stype_ %s" (Atom.to_string x)))
and collect_type_stype flag_tcvar parent_opt already_binded selector collector st =       
    map0_place (collect_type_stype_ flag_tcvar parent_opt already_binded selector collector) st 
and collect_type_stypes flag_tcvar parent_opt already_binded selector collector =
    List.fold_left (fun (acc0, acc1) st -> 
        let _, collected_elts, fvars = collect_type_stype flag_tcvar parent_opt already_binded selector collector st in
        collected_elts@acc0, fvars@acc1
    ) ([], []) 

and free_tvars_stype flag_tcvar (already_binded:Atom.Set.t) st = 
    let already_binded, _, ftvars = collect_type_stype flag_tcvar None  already_binded (function e -> false) (fun parent_opt env e -> []) st in
    already_binded, Utils.deduplicate Fun.id ftvars

and collect_type_header_ flag_tcvar parent_opt already_binded selector collector place = 
(* TODO FIXME *)
failwith "collect_type_header : needs that variable and type variable to be the same or convertion function (bijective)"
(*function 
    | UseMetadata (mt,x) ->
        let _, collected_elts, ftvars = collect_type_mtype ~flag_tcvar:flag_tcvar parent_opt already_binded selector collector mt in
        Atom.Set.add x already_binded, collected_elts, ftvars
    | SetTimer (x) | SetFireTimer (x,_) -> Atom.Set.add x already_binded, [], []
*)
and collect_type_header flag_tcvar parent_opt already_binded selector collector st =       
    map0_place (collect_type_header_ flag_tcvar parent_opt already_binded selector collector) st 

and collect_type_headers flag_tcvar parent_opt already_binded selector collector  headers = 
    List.fold_left (fun (already_binded, acc0, acc1) header -> 
        let already_binded, collected_elts, ftvars = collect_type_header flag_tcvar parent_opt already_binded selector collector header in
        already_binded, collected_elts@acc0, ftvars@acc1
    ) (already_binded, [], []) headers 

and collect_type_aconstraint flag_tcvar parent_opt already_binded selector collector (headers, e_opt)= 
    let collect_expropt = function
    | None -> already_binded, [], [] 
    | Some e -> collect_type_expr flag_tcvar parent_opt already_binded selector collector  e
    in

    (* TODO FIXME (bug) session type should proagate the inner already_binded to its continuation *)
    let inner_already_binded, collected_elts1, ftvars1 = collect_type_headers flag_tcvar parent_opt already_binded selector collector headers in
    let _, collected_elts2, ftvars2 = collect_expropt e_opt in
    already_binded, collected_elts1@collected_elts2, ftvars1@ftvars2


and collect_type_aconstraints flag_tcvar parent_opt already_binded selector collector (aconstraints: applied_constraint list) = 
    List.fold_left (fun (acc0, acc1) aconstraint -> 
        let _, collected_elts, ftvars = collect_type_aconstraint flag_tcvar parent_opt already_binded selector collector aconstraint in
        collected_elts@acc0, ftvars@acc1
    ) ([], []) aconstraints 

and collect_type_structtype_ flag_tcvar parent_opt already_binded selector collector place= function
| CompTUid x when flag_tcvar -> already_binded, [], [x]
| CompTUid _ -> 
    (*Not a type variable but a component variable *)
    already_binded, [], []
| TPolyCVar _ | CompTBottom -> already_binded, [], [] 
| TStruct (_, sign) -> 
    let collected_elts, ftvars = collect_type_mtypes flag_tcvar parent_opt already_binded selector collector (List.map snd (List.of_seq (Atom.VMap.to_seq sign))) in
    already_binded, collected_elts, ftvars
and collect_type_structtype flag_tcvar parent_opt already_binded selector collector cmt =       
    map0_place (collect_type_structtype_ flag_tcvar  parent_opt already_binded selector collector) cmt 
and free_tvars_structtype flag_tcvar  (already_binded:Atom.Set.t) cmt = 
    let already_binded, _, ftvars = collect_type_structtype flag_tcvar None  already_binded (function e -> false) (fun parent_opt env e -> []) cmt in
    already_binded, Utils.deduplicate Fun.id ftvars

and collect_type_mtype_ flag_tcvar parent_opt already_binded selector collector place mt = 
    (* Collection *)
    let collected_elts0 = if selector mt then collector parent_opt already_binded {place; value=mt} else [] in 

    let _, collected_elts1, ftvars1 = match mt with
        | EmptyMainType -> already_binded, collected_elts0, []
        | CType ct -> 
            collect_type_ctype flag_tcvar parent_opt already_binded selector collector ct
        | SType st -> collect_type_stype flag_tcvar parent_opt already_binded selector collector st
        | CompType cmt -> collect_type_structtype flag_tcvar parent_opt already_binded selector collector cmt
        | ClType cmt -> collect_type_structtype flag_tcvar parent_opt already_binded selector collector cmt
        | ConstrainedType (mt,ac) -> 
            let _, collected_elts1, ftvars1 = collect_type_mtype ~flag_tcvar:flag_tcvar parent_opt already_binded selector collector mt in
            let _, collected_elts2, ftvars2 = collect_type_aconstraint flag_tcvar parent_opt already_binded selector collector ac in
            already_binded, collected_elts1@collected_elts2, ftvars1@ftvars2
    in
    already_binded, collected_elts0@collected_elts1, ftvars1

and collect_type_mtype ?(flag_tcvar=false) (parent_opt:Atom.atom option) already_binded selector collector mt =       
    map0_place (collect_type_mtype_ flag_tcvar  parent_opt already_binded selector collector) mt 

and collect_type_mtypes (flag_tcvar:bool) parent_opt already_binded selector collector =
    List.fold_left (fun (acc0, acc1) mt -> 
        let _, collected_elts, fvars = collect_type_mtype ~flag_tcvar:flag_tcvar parent_opt already_binded selector collector mt in
        collected_elts@acc0, fvars@acc1
    ) ([], []) 

and free_tvars_mtype ?(flag_tcvar=false) already_binded mt : Atom.Set.t * type_variable list =
    let already_binded, _, ftvars = collect_type_mtype ~flag_tcvar:flag_tcvar None  already_binded (function e -> false) (fun parent_opt env e -> []) mt in
    already_binded, Utils.deduplicate Fun.id ftvars 

and collect_type_expr_ flag_tcvar parent_opt already_binded selector collector place (e, mt) = 
    let collect_mtype = collect_type_mtype ~flag_tcvar:flag_tcvar parent_opt already_binded selector collector in
    let collect_expr = collect_type_expr flag_tcvar parent_opt already_binded selector collector in
    let collect_exprs (es:expr list) = 
        List.fold_left (fun (acc0, acc1) e -> 
            let _, collected_elts, ftvars = collect_expr e in
            collected_elts@acc0, ftvars@acc1
        ) ([], [])  es
    in
    let collect_expropt = function
    | None -> already_binded, [], [] 
    | Some e -> collect_expr e
    in

    let _, collected_elts1, ftvars1 = collect_mtype mt in
    let collected_elts2, ftvars2 = match e with
    | LitExpr {value = StaticBridge {protocol_name}} ->
        if None <> Atom.Set.find_opt protocol_name already_binded || Atom.is_builtin protocol_name then [], []
        else [], [protocol_name]
    | BridgeCall _ | EmptyExpr | VarExpr _ | ImplicitVarExpr _ | LitExpr _ | This | Self -> [], []
    | ActivationAccessExpr (_, e, _) | UnopExpr (_, e) -> 
        let _, collected_elts, ftvars = collect_expr e in
        collected_elts, ftvars
    | AccessExpr (e1, e2) | BinopExpr (e1, _, e2) -> 
        collect_exprs [e1; e2]
    | InterceptedActivationRef (actor_ref, interceptec_actor_ref) -> 
        let _, collected_elts1, ftvars1 = collect_expr actor_ref in
        let _, collected_elts2, ftvars2 = collect_expropt interceptec_actor_ref in
        collected_elts1@collected_elts2, ftvars1@ftvars2
    | LambdaExpr (params, e) ->
        let collected_elts1, ftvars1 =  
        List.fold_left (fun (collected_elts0, ftvars0) {value=mt, _} -> 
            let _, collected_elts1, ftvars1 =  collect_mtype mt in
            collected_elts0@collected_elts1, ftvars0@ftvars1
        ) ([], []) params in
        let _, collected_elts2, ftvars2 =  collect_expr e in
        collected_elts1@collected_elts2, ftvars1@ftvars2
    | CallExpr (e, es) | NewExpr (e, es) ->
        collect_exprs (e::es)
    | PolyApp _ -> failwith "Not yet supported in free_tvars_expr"
    | Spawn spawn -> 
        let _, collected_elts1, ftvars1 = collect_type_cexpr flag_tcvar parent_opt already_binded selector collector spawn.c in
        let collected_elts2, ftvars2 = collect_exprs spawn.args in
        let _, collected_elts3, ftvars3 = collect_expropt spawn.at in
        let _, collected_elts4, ftvars4 = collect_expropt spawn.inline_in in
        collected_elts1@collected_elts2@collected_elts3@collected_elts4, ftvars1@ftvars2@ftvars3@ftvars4
    | BoxCExpr _ -> failwith "Not yet supported in free_tvars_expr"
    | OptionExpr e_opt ->
        let _, collected_elts, ftvars = collect_expropt e_opt in
        collected_elts, ftvars
    | ResultExpr (e1_opt, e2_opt) ->
        let _, collected_elts1, ftvars1 = collect_expropt e1_opt in
        let _, collected_elts2, ftvars2 = collect_expropt e2_opt in
        collected_elts1@collected_elts2, ftvars1@ftvars2
    | BlockExpr (_, es) | Create {args=es} -> collect_exprs es
    | Block2Expr (_, ees) ->
        let es1, es2 = List.split ees in
        let collected_elts1, ftvars1 = collect_exprs  es1 in
        let collected_elts2, ftvars2 = collect_exprs  es2 in
        collected_elts1@collected_elts2, ftvars1@ftvars2
    | TernaryExpr (e1, e2, e3) -> 
        let _, collected_elts1, ftvars1 = collect_expr e1 in
        let _, collected_elts2, ftvars2 = collect_expr e2 in
        let _, collected_elts3, ftvars3 = collect_expr e3 in
        collected_elts1@collected_elts2@collected_elts3, ftvars1@ftvars2@ftvars3
    in
    already_binded, collected_elts1@collected_elts2, ftvars1@ftvars2 

and collect_type_expr flag_tcvar parent_opt already_binded selector collector e =       
    map0_place (collect_type_expr_ flag_tcvar parent_opt already_binded selector collector) e 

and free_tvars_expr flag_tcvar (already_binded:Atom.Set.t) (e:expr) = 
    let already_binded, _, ftvars = collect_type_expr flag_tcvar None  already_binded (function e -> false) (fun parent_opt env e -> []) e in
    already_binded, Utils.deduplicate Fun.id ftvars 

and collect_type_stmt_ flag_tcvar parent_opt already_binded selector collector place stmt = 
    (* N.B. stmt can not introduce new type variables *)
    let collect_expr = collect_type_expr flag_tcvar parent_opt already_binded selector collector in
    let collect_stmt = collect_type_stmt flag_tcvar parent_opt already_binded selector collector in
    let collect_mtype = collect_type_mtype ~flag_tcvar:flag_tcvar parent_opt already_binded selector collector in
    let collect_exprs exprs = 
        List.fold_left (fun (acc0, acc1) expr -> 
            let _, collected_elts, ftvars = collect_expr expr in
            collected_elts@acc0, ftvars@acc1
        ) ([], []) exprs 
    in
    let collect_stmts stmts = 
        List.fold_left (fun (acc0, acc1) stmt -> 
            let _, collected_elts, ftvars = collect_stmt stmt in
            collected_elts@acc0, ftvars@acc1
        ) ([], []) stmts 
    in
    let collect_stmtopt = function
    | None -> already_binded, [], [] 
    | Some stmt -> collect_stmt stmt
    in

    match stmt with
    | BreakStmt | CommentsStmt _ | ContinueStmt | ExitStmt _ | EmptyStmt -> already_binded, [], []
    | AssignExpr (_, e) | AssignThisExpr (_, e) | AssignSelfExpr (_, e) | ExpressionStmt e |  ReturnStmt e -> collect_expr e
    | BranchStmt {s; label; branches} -> 
        let _, collected_elts1, ftvars1 = collect_expr s in
        let _, collected_elts2, ftvars2 = collect_expr label in
        let collected_elts3, ftvars3 = List.split (List.map (function {branch_s; body} -> 
            (* branch_s is not a type binder *)
            let _, collect_elts, fvars = collect_type_stmt flag_tcvar parent_opt already_binded selector collector body in

            collect_elts, fvars
        ) branches)
        in

        already_binded, collected_elts1@collected_elts2@(List.flatten collected_elts3), ftvars1@ftvars2@(List.flatten ftvars3)
    | LetStmt (mt, _, e) ->
        let _, collected_elts1, ftvars1 = collect_mtype mt in
        let _, collected_elts2, ftvars2 = collect_expr e in
        already_binded, collected_elts1@collected_elts2, ftvars1@ftvars2
    | ForeachStmt (mt, _, e, stmt) -> 
        let _, collected_elts1, ftvars1 = collect_mtype mt in
        let _, collected_elts2, ftvars2 = collect_expr e in
        let _, collected_elts3, ftvars3 = collect_stmt stmt in
        already_binded, collected_elts1@collected_elts2@collected_elts3, ftvars1@ftvars2@ftvars3
    | IfStmt (e, stmt1, stmt2_opt) ->
        let _, collected_elts1, ftvars1 = collect_expr e in
        let _, collected_elts2, ftvars2 = collect_stmt stmt1 in
        let _, collected_elts3, ftvars3 = collect_stmtopt stmt2_opt in
        already_binded, collected_elts1@collected_elts2@collected_elts3, ftvars1@ftvars2@ftvars3
    | MatchStmt (e, branches) -> 
        let es, stmts = List.split branches in
        let _, collected_elts1, ftvars1 = collect_expr e in
        let collected_elts2, ftvars2 = collect_exprs es in
        let collected_elts3, ftvars3 = collect_stmts stmts in
        already_binded, collected_elts1@collected_elts2@collected_elts3, ftvars1@ftvars2@ftvars3
    | BlockStmt stmts -> 
        let collected_elts, ftvars = collect_stmts stmts in
        already_binded, collected_elts, ftvars
    | GhostStmt stmt -> collect_stmt stmt
    | WithContextStmt (_, _, e, stmts) ->
        let _, collected_elts1, ftvars1 = collect_expr e in
        let collected_elts2, ftvars2 = collect_stmts stmts in
        already_binded, collected_elts1@collected_elts2, ftvars1@ftvars2

and collect_type_stmt flag_tcvar (parent_opt:Atom.atom option) already_binded selector collector stmt =       
    map0_place (collect_type_stmt_ flag_tcvar parent_opt already_binded selector collector) stmt 

and free_tvars_stmt ?(flag_tcvar=false) (already_binded:Atom.Set.t) stmt = 
    let already_binded, _, ftvars = collect_type_stmt flag_tcvar None  already_binded (function e -> false) (fun parent_opt env e -> []) stmt in
    already_binded, Utils.deduplicate Fun.id ftvars 

and collect_type_cexpr flag_tcvar parent_opt already_binded selector collector cexpr = already_binded, [], [] (* TODO FIXME *)

let rec timers_of_headers = function
    | [] -> []
    | {value=UseMetadata _} ::headers-> timers_of_headers headers
    | {value=SetTimer x}::headers -> x::(timers_of_headers headers)
    | {value=SetFireTimer (x,_)}::headers -> raise (Error.DeadbranchError "SetFireTimer should exists before GuardTransform - not supported yet") 
and timers_of_st_ = function
| STEnd | STWildcard -> []
| STRecv ({value=ConstrainedType (_, (guard_headers, _))}, st) | STSend ({value=ConstrainedType (_, (guard_headers, _))}, st) -> 
    (timers_of_headers guard_headers) @ (timers_of_st st)
and timers_of_st st = timers_of_st_ st.value

let rec replace_type_place (replace_type_value : Error.place -> 'a -> 'a) ({ AstUtils.place ; AstUtils.value}: 'a AstUtils.placed) = 
    let value = replace_type_value place value in
    {AstUtils.place; AstUtils.value}

let rec _replace_type_composed_type x_to_replace ((replaceby_x_opt, _)as replaceby) place : _composed_type -> _composed_type = function
    | TActivationRef mt -> TActivationRef (replace_type_main_type x_to_replace replaceby mt) 
    | TArrow (mt1, mt2) -> TArrow (
        replace_type_main_type x_to_replace replaceby mt1,
        replace_type_main_type x_to_replace replaceby mt2
    ) 
    | TVar x when x = x_to_replace && replaceby_x_opt <> None -> TVar (Option.get replaceby_x_opt)
    | TPolyVar x when x = x_to_replace && replaceby_x_opt <> None -> TPolyVar (Option.get replaceby_x_opt)
    | (TVar _ as t) | (TBridge _ as t) | (TFlatType _ as t) | (TPolyVar _ as t) -> t
    | TArray mt -> TArray (replace_type_main_type x_to_replace replaceby mt)  
    | TDict (mt1, mt2) -> TDict (
        replace_type_main_type x_to_replace replaceby mt1,
        replace_type_main_type x_to_replace replaceby mt2
    ) 
    | TList mt -> TList (replace_type_main_type x_to_replace replaceby mt) 
    | TOption mt -> TOption (replace_type_main_type x_to_replace replaceby mt)
    | TResult (mt1, mt2) -> TResult (
        replace_type_main_type x_to_replace replaceby mt1,
        replace_type_main_type x_to_replace replaceby mt2
    ) 
    | TSet mt -> TSet (replace_type_main_type x_to_replace replaceby mt)
    | TTuple mts -> TTuple (
        List.map (
            replace_type_main_type x_to_replace replaceby
        ) mts
    ) 
    | TInductive mts -> TInductive (
        List.map (
            replace_type_main_type x_to_replace replaceby
        ) mts
    ) 
    | TVPlace mt -> TVPlace (replace_type_main_type x_to_replace replaceby mt)
    | TUnion (mt1, mt2) -> TUnion (
        replace_type_main_type x_to_replace replaceby mt1,
        replace_type_main_type x_to_replace replaceby mt2
    ) 
    | TForall (x, mt) when x = x_to_replace && replaceby_x_opt <> None -> TForall (Option.get replaceby_x_opt, replace_type_main_type x_to_replace replaceby mt)
    | TForall (x, mt) -> TForall (x, replace_type_main_type x_to_replace replaceby mt)
and replace_type_composed_type x_to_replace replaceby = replace_type_place (_replace_type_composed_type x_to_replace replaceby)

and _replace_type_session_type x_to_replace ((replaceby_x_opt, _)as replaceby) place = function
| STEnd -> STEnd 
| STVar x when x = x_to_replace && replaceby_x_opt <> None -> STVar (Option.get replaceby_x_opt) 
| STVar x -> STVar x 
| STSend (mt, st) -> STSend (
    replace_type_main_type x_to_replace replaceby mt,
    replace_type_session_type x_to_replace replaceby st
) 
| STRecv (mt, st) -> STRecv (
    replace_type_main_type x_to_replace replaceby mt,
    replace_type_session_type x_to_replace replaceby st
) 
| STBranch branches -> STBranch (
    List.map (function (l, st, guard_opt) ->
        l,
        replace_type_session_type x_to_replace replaceby st,
        guard_opt (*Not rewrite here for now TODO*)
    ) branches
)            
| STSelect branches -> STSelect (
    List.map (function (l, st, guard_opt) ->
        l,
        replace_type_session_type x_to_replace replaceby st,
        guard_opt
    ) branches
)               

| STRec (x, st) when x = x_to_replace && replaceby_x_opt <> None -> STRec (
    Option.get replaceby_x_opt,
    replace_type_session_type x_to_replace replaceby st
) 
| STRec (x, st) -> STRec (
    x,
    replace_type_session_type x_to_replace replaceby st
) 
| STInline x -> STInline x 
and replace_type_session_type x_to_replace replaceby = replace_type_place (_replace_type_session_type x_to_replace replaceby)

and _replace_type_main_type (x_to_replace:type_variable) ((replaceby_x, replaceby_e_opt)as replaceby) place = function
| CType {value=TVar x} when x = x_to_replace && replaceby_e_opt <> None -> 
    Option.get replaceby_e_opt
| CType {value=TPolyVar x} when x = x_to_replace && replaceby_e_opt <> None -> 
    Option.get replaceby_e_opt
| CType ct -> CType (replace_type_composed_type x_to_replace replaceby ct)
| SType st -> SType (replace_type_session_type x_to_replace replaceby st) 
and replace_type_main_type (x_to_replace:type_variable) (replaceby:type_variable option * _main_type option) = replace_type_place (_replace_type_main_type x_to_replace replaceby)


let rec _replace_stype_session_type x_to_replace ((replaceby_x_opt, replaceby_e_opt)as replaceby) place = function
| STEnd -> STEnd 
| STVar x when x = x_to_replace && replaceby_x_opt <> None -> STVar (Option.get replaceby_x_opt) 
| STVar x when x = x_to_replace && replaceby_e_opt <> None -> (Option.get replaceby_e_opt) 
| STVar x -> STVar x 
| STSend (mt, st) -> STSend (
    mt,
    replace_stype_session_type x_to_replace replaceby st
) 
| STRecv (mt, st) -> STRecv (
    mt,
    replace_stype_session_type x_to_replace replaceby st
) 
| STBranch branches -> STBranch (
    List.map (function (l, st, guard_opt) ->
        l,
        replace_stype_session_type x_to_replace replaceby st,
        guard_opt (*Not rewrite here for now TODO*)
    ) branches
)            
| STSelect branches -> STSelect (
    List.map (function (l, st, guard_opt) ->
        l,
        replace_stype_session_type x_to_replace replaceby st,
        guard_opt
    ) branches
)               

| STRec (x, st) when x = x_to_replace && replaceby_x_opt <> None -> STRec (
    Option.get replaceby_x_opt,
    replace_stype_session_type x_to_replace replaceby st
) 
| STRec (x, st) -> STRec (
    x,
    replace_stype_session_type x_to_replace replaceby st
) 
| STInline x -> STInline x 
and replace_stype_session_type x_to_replace replaceby = replace_type_place (_replace_stype_session_type x_to_replace replaceby)




(* FIXMe refactor replace by map_place ..*)
let rec rewrite_expr_place rewrite_expr_value { AstUtils.place ; AstUtils.value} = 
    let _value = rewrite_expr_value place (fst value) in
    {AstUtils.place; AstUtils.value = _value, snd value}

let rec _rewrite_expr_expr selector rewriter place (e, mt) = 
    let e = match e with
    | e when selector e -> 
        rewriter mt e
    | (BridgeCall _ as e) | (EmptyExpr as e) | (LitExpr _ as e) | (This as e) | (Self as e) | (VarExpr _ as e) | (ImplicitVarExpr _ as e) -> e
    | ActivationAccessExpr (cname, e, mname) ->
        ActivationAccessExpr(
            cname,
            rewrite_expr_expr selector rewriter e,
            mname
        )
    | AccessExpr (e1, e2) -> AccessExpr (
        rewrite_expr_expr selector rewriter e1,
        rewrite_expr_expr selector rewriter e2
    )
    | BinopExpr (e1, op, e2) -> BinopExpr (
        rewrite_expr_expr selector rewriter e1,
        op,
        rewrite_expr_expr selector rewriter e2
    )
    | LambdaExpr (params, e) -> LambdaExpr (
        params, (* WARNIN TODO FIXME replace in type predicates *)
        rewrite_expr_expr selector rewriter e
    )
    | UnopExpr (op, e) -> UnopExpr (op, rewrite_expr_expr selector rewriter e)
    | CallExpr (e, es) -> CallExpr(
        rewrite_expr_expr selector rewriter e,
        List.map (rewrite_expr_expr selector rewriter) es
    )
    | NewExpr (e, es) -> NewExpr(
        rewrite_expr_expr selector rewriter e,
        List.map (rewrite_expr_expr selector rewriter) es
    )
    | Create c -> Create { c with 
        args = List.map (rewrite_expr_expr selector rewriter) c.args;
    }
    | Spawn sp -> Spawn { sp with 
        args = List.map (rewrite_expr_expr selector rewriter) sp.args;
        at = Option.map (rewrite_expr_expr selector rewriter) sp.at;
        inline_in = Option.map (rewrite_expr_expr selector rewriter) sp.inline_in
    }
    | BoxCExpr _ as e -> e
    | OptionExpr e_opt -> OptionExpr (
        Option.map (rewrite_expr_expr selector rewriter) e_opt
    ) 
    | ResultExpr (e1_opt, e2_opt) -> ResultExpr (
        Option.map (rewrite_expr_expr selector rewriter) e1_opt,
        Option.map (rewrite_expr_expr selector rewriter) e2_opt
    ) 
    | BlockExpr(b, es) -> BlockExpr (b,
        List.map (rewrite_expr_expr selector rewriter) es 
    )
    | Block2Expr(b, ees) -> Block2Expr (b,
        List.map (function (e1,e2) ->
            rewrite_expr_expr selector rewriter e1,
            rewrite_expr_expr selector rewriter e2
        ) ees 
    )
    | TernaryExpr (e1, e2, e3) -> TernaryExpr(
        rewrite_expr_expr selector rewriter e1,
        rewrite_expr_expr selector rewriter e2,
        rewrite_expr_expr selector rewriter e3
    )
    | InterceptedActivationRef (e1, e2_opt) -> InterceptedActivationRef(
        rewrite_expr_expr selector rewriter e1,
        Option.map (rewrite_expr_expr selector rewriter) e2_opt
    )
    in 
    (e, mt)
and rewrite_expr_expr selector rewriter = map_place (_rewrite_expr_expr selector rewriter)

and _rewrite_expr_stmt selector rewriter place = function 
    | EmptyStmt -> EmptyStmt
    | AssignExpr (x, e) -> AssignExpr (x, rewrite_expr_expr selector rewriter e) 
    | AssignThisExpr (x, e) -> AssignThisExpr (x, rewrite_expr_expr selector rewriter e)
    | AssignSelfExpr (x, e) -> AssignSelfExpr (x, rewrite_expr_expr selector rewriter e)
    | LetStmt (mt, x, e) -> (* TODO FIXME expr in type are not yet concerned *)
        LetStmt (mt, x, rewrite_expr_expr selector rewriter e)
    | CommentsStmt c -> CommentsStmt c
    | BranchStmt {s; label; branches} -> begin 
        let rewrite_branche {branch_label; branch_s;body} =
            {   
                branch_label; 
                branch_s; 
                body = rewrite_expr_stmt selector rewriter body;
            }
        in
        BranchStmt{
            s       = rewrite_expr_expr selector rewriter s;
            label   = rewrite_expr_expr selector rewriter label;
            branches= List.map rewrite_branche branches
        }
    end
    | BreakStmt -> BreakStmt
    | ContinueStmt -> ContinueStmt
    | ExitStmt i -> ExitStmt i
    | ForeachStmt (mt, x, e, stmt) -> (* TODO FIXME expr in type are not yet concerned *)
        ForeachStmt(mt, x, 
            rewrite_expr_expr selector rewriter e,
            rewrite_expr_stmt selector rewriter stmt)
    | IfStmt (e, stmt1, stmt2_opt) ->
        IfStmt (
            rewrite_expr_expr selector rewriter e,
            rewrite_expr_stmt selector rewriter stmt1,
            Option.map (rewrite_expr_stmt selector rewriter) stmt2_opt
        )
    | MatchStmt (e, branches) ->
        MatchStmt (
            rewrite_expr_expr selector rewriter e,
            List.map (function (e, stmt) ->
                rewrite_expr_expr selector rewriter e,
                rewrite_expr_stmt selector rewriter stmt
            ) branches
        )
    | ReturnStmt e -> ReturnStmt (rewrite_expr_expr selector rewriter e) 
    | ExpressionStmt e -> ExpressionStmt (rewrite_expr_expr selector rewriter e) 
    | BlockStmt stmts -> BlockStmt (List.map (rewrite_expr_stmt selector rewriter) stmts) 
    | GhostStmt stmt -> GhostStmt (rewrite_expr_stmt selector rewriter stmt)
    | WithContextStmt (anonymous_mod, cname, e, stmts) -> WithContextStmt(
        anonymous_mod,
        cname,
        rewrite_expr_expr selector rewriter e,
        List.map (rewrite_expr_stmt selector rewriter) stmts
    )
and rewrite_expr_stmt selector rewriter = map_place (_rewrite_expr_stmt selector rewriter)
                
(* Warning do not replace ImplictVar !!! *)
let make x_to_replace ((replaceby_x_opt, replaceby_e_opt)as replaceby) = 
    let selector = function |VarExpr x when x = x_to_replace -> true | _ -> false in
    let rewriter e _ = match replaceby_x_opt with | Some x -> VarExpr x | None -> Option.get replaceby_e_opt in
    selector, rewriter
let replace_expr_expr x_to_replace replaceby = 
    let selector, rewriter = make x_to_replace replaceby in
    rewrite_expr_expr selector rewriter
let replace_expr_stmt x_to_replace replaceby = 
    let selector, rewriter = make x_to_replace replaceby in
    rewrite_expr_stmt selector rewriter

(*****************************************************)

let rec _rewrite_type_ctype selector rewriter place = 
    let rewrite_mtype = rewrite_type_mtype selector rewriter in    
function
    | TActivationRef mt -> TActivationRef (rewrite_mtype mt)
    | TArrow (mt1, mt2) -> TArrow (rewrite_mtype mt1, mt2) 
    | TVar x -> TVar x 
    | TObject x -> TObject x
    | TFlatType ft -> TFlatType ft 
    | TArray mt -> TArray (rewrite_mtype mt) 
    | TDict (mt1, mt2) -> TDict (rewrite_mtype mt1, rewrite_mtype mt2) 
    | TList mt -> TList (rewrite_mtype mt) 
    | TOption mt -> TOption (rewrite_mtype mt) 
    | TResult (mt1, mt2) -> TResult (rewrite_mtype mt1, rewrite_mtype mt2)
    | TSet mt -> TSet (rewrite_mtype mt) 
    | TTuple mts -> TTuple (List.map rewrite_mtype mts) 
    | TInductive mts -> TInductive (List.map rewrite_mtype mts) 
    | TVPlace mt -> TVPlace (rewrite_mtype mt) 
    | TUnion (mt1, mt2) -> TUnion (rewrite_mtype mt1, rewrite_mtype mt2) 
    | TBridge tb -> TBridge {
        in_type = rewrite_mtype tb.in_type;
        out_type = rewrite_mtype tb.out_type;
        protocol = rewrite_mtype tb.protocol;
    }
    | TInport mt -> TInport (rewrite_mtype mt) 
    | TEport mt -> TEport (rewrite_mtype mt) 
    | TOutport mt -> TOutport (rewrite_mtype mt)

    | TPolyVar x -> TPolyVar x
    | TForall (x, mt) -> TForall (x, rewrite_mtype mt) 
and rewrite_type_ctype selector rewriter = map_place (_rewrite_type_ctype selector rewriter)

and _rewrite_type_stype selector rewriter place = 
    let rewrite_mtype = rewrite_type_mtype selector rewriter in    
    let rewrite_stype = rewrite_type_stype selector rewriter in    
    let rewrite_branches branches = 
        List.map (function ((x, st, ac_opt) : (type_variable * session_type * applied_constraint option))->
            x,
            rewrite_stype st,
            Option.map (rewrite_type_aconstraint selector rewriter) ac_opt
        ) branches
    in
function
    | STEnd -> STEnd
    | STVar x -> STVar x 
    | STSend (mt, st) -> STSend (rewrite_mtype mt, rewrite_stype st) 
    | STRecv  (mt, st) -> STRecv (rewrite_mtype mt, rewrite_stype st)
    | STBranch branches -> STBranch (rewrite_branches branches)            
    | STSelect branches -> STSelect (rewrite_branches branches)               
    | STRec (x, st) -> STRec (x, rewrite_stype st) 
    | STInline x -> STInline x 
    | STPolyVar x -> STPolyVar x 
    | STDual st -> STDual (rewrite_stype st)
and rewrite_type_stype selector rewriter = map_place (_rewrite_type_stype selector rewriter)

and _rewrite_type_structtype selector rewriter place = function
| CompTUid x -> CompTUid x
| TStruct (x, sign) -> TStruct
(x, Atom.VMap.map (rewrite_type_mtype selector rewriter) sign)
| TPolyCVar x -> TPolyCVar x
| CompTBottom -> CompTBottom
and rewrite_type_structtype selector rewriter = map_place (_rewrite_type_structtype selector rewriter)

and _rewrite_type_mtype (selector : _main_type -> bool) rewriter place = function
| mt when selector mt -> rewriter mt
| EmptyMainType -> EmptyMainType
| CType ct -> CType (rewrite_type_ctype selector rewriter ct)
| SType st -> SType (rewrite_type_stype selector rewriter st)
| CompType cmt -> CompType (rewrite_type_structtype selector rewriter cmt)
| ClType cmt -> ClType (rewrite_type_structtype selector rewriter cmt)
| ConstrainedType (mt, ac) -> ConstrainedType (
    rewrite_type_mtype selector rewriter mt,
    rewrite_type_aconstraint selector rewriter ac
)
and rewrite_type_mtype selector rewriter = map_place (_rewrite_type_mtype selector rewriter)

and _rewrite_type_expr selector rewriter place (e, mt) = 
    let rewrite_mtype = rewrite_type_mtype selector rewriter in    
    let rewrite_expr = rewrite_type_expr selector rewriter in    
    logger#debug "rewrite_type expr (%s)" (show_main_type mt);

    let e = match e with
        | (VarExpr _ as e) | (ImplicitVarExpr _ as e) | (LitExpr _ as e) | (This as e) | (Self as e) | (BridgeCall _ as e) -> e
        | ActivationAccessExpr (x, e, y) -> ActivationAccessExpr (x, e, y)
        | AccessExpr (e1, e2) -> AccessExpr (rewrite_expr e1, rewrite_expr e2)
        | BinopExpr (e1, op, e2) -> BinopExpr (rewrite_expr e1, op, rewrite_expr e2)
        | TernaryExpr (e1, e2, e3) -> TernaryExpr (rewrite_expr e1,  rewrite_expr e2, rewrite_expr e3)
        | LambdaExpr (params, e) -> 
            LambdaExpr (
                List.map (map_place(fun place (mt,x) -> rewrite_mtype mt, x)) params,
                rewrite_expr e)
        | UnopExpr (op, e) -> UnopExpr (op, rewrite_expr e)
        | CallExpr (e, es) -> CallExpr (rewrite_expr e, List.map rewrite_expr es)
        | NewExpr (e, es) -> NewExpr (rewrite_expr e, List.map rewrite_expr es)
        | PolyApp (e, mts) -> PolyApp (rewrite_expr e, List.map rewrite_mtype mts)
        | Create create -> Create {
            c = create.c;
            args = create.args; 
        }
        | Spawn spawn -> Spawn {
            c = rewrite_type_cexpr selector rewriter spawn.c;
            args = spawn.args;
            at = Option.map rewrite_expr spawn.at;
            inline_in = Option.map rewrite_expr spawn.inline_in
        } 
        | BoxCExpr ce -> BoxCExpr (rewrite_type_cexpr selector rewriter ce)
        | OptionExpr e_opt -> OptionExpr (Option.map rewrite_expr e_opt)
        | InterceptedActivationRef (e1, e2_opt) -> InterceptedActivationRef (rewrite_expr e1, Option.map rewrite_expr e2_opt)
        | ResultExpr (e1_opt, e2_opt) -> ResultExpr (Option.map rewrite_expr e1_opt, Option.map rewrite_expr e2_opt)
        | BlockExpr (b, es) -> BlockExpr (b, List.map rewrite_expr es)
        | Block2Expr (b, ees) -> Block2Expr (b,
            List.map (function (e1, e2) ->
                rewrite_expr e1, rewrite_expr e2    
            ) ees
        )
    in
    e, rewrite_mtype mt
and rewrite_type_expr selector rewriter = map_place (_rewrite_type_expr selector rewriter)

and _rewrite_type_stmt selector rewriter place = 
    let rewrite_mtype = rewrite_type_mtype selector rewriter in    
    let rewrite_expr = rewrite_type_expr selector rewriter in    
    let rewrite_stmt = rewrite_type_stmt selector rewriter in    
    logger#debug "rewrite_type stmt";
function
| (BreakStmt as stmt) | (CommentsStmt _ as stmt) | (ContinueStmt as stmt) | (EmptyStmt as stmt) | (ExitStmt _ as stmt) -> stmt
| AssignExpr (x, e) -> AssignExpr (x, rewrite_expr e)
| AssignThisExpr (x, e) -> AssignThisExpr (x, rewrite_expr e)
| AssignSelfExpr (x, e) -> AssignSelfExpr (x, rewrite_expr e)
| LetStmt (mt, x, e) -> LetStmt (rewrite_mtype mt, x , rewrite_expr e)
| ForeachStmt (mt, x, e, stmt) -> ForeachStmt (
    rewrite_mtype mt,
    x, 
    rewrite_expr e,
    rewrite_stmt stmt 
) 
| IfStmt (e, stmt1, stmt2_opt) -> IfStmt (
    rewrite_expr e,
    rewrite_stmt stmt1,
    Option.map rewrite_stmt stmt2_opt
)
| MatchStmt (e, branches) -> MatchStmt (e,
    List.map (function (e,stmt) -> 
        rewrite_expr e, rewrite_stmt stmt
    ) branches
)
| ReturnStmt e -> ReturnStmt (rewrite_expr e)
| ExpressionStmt e -> ExpressionStmt (rewrite_expr e)
| BlockStmt stmts -> BlockStmt (List.map rewrite_stmt stmts)
| GhostStmt stmt -> GhostStmt (rewrite_stmt stmt)
| BranchStmt {s; label; branches} -> begin 
    let rewrite_branche {branch_label; branch_s;body} =
        {   
            branch_label; 
            branch_s; 
            body = rewrite_stmt body;
        }
    in
    BranchStmt{
        s       = rewrite_expr s;
        label   = rewrite_expr label;
        branches= List.map rewrite_branche branches
    }
end
| WithContextStmt (anonymous_mod, cname, e, stmts) -> 
    WithContextStmt(anonymous_mod, cname, rewrite_expr e, List.map rewrite_stmt stmts)
and rewrite_type_stmt selector rewriter = map_place (_rewrite_type_stmt selector rewriter)

and _rewrite_type_cexpr selector rewriter place (ce, mt) = 
    let rewrite_expr = rewrite_type_expr selector rewriter in    
    let rewrite_cexpr = rewrite_type_cexpr selector rewriter in    

    let ce = match ce with
        | (VarCExpr x as ce) -> ce
        | AppCExpr (ce1, ces) -> AppCExpr (rewrite_cexpr ce1, List.map rewrite_cexpr ces)
        | UnboxCExpr e -> UnboxCExpr (rewrite_expr e)
        | AnyExpr e -> AnyExpr (rewrite_expr e)
    in
    (ce, rewrite_type_mtype selector rewriter mt)
and rewrite_type_cexpr selector rewriter = map_place (_rewrite_type_cexpr selector rewriter)

and _rewrite_type_header selector rewriter place = function
| UseMetadata (mt, x) -> UseMetadata (rewrite_type_mtype selector rewriter mt, x)
| SetTimer x -> SetTimer x
| SetFireTimer (x, i) -> SetFireTimer (x, i)
and rewrite_type_header selector rewriter = map_place (_rewrite_type_header selector rewriter)

and rewrite_type_aconstraint selector rewriter (headers, e_opt)= 
    (
        List.map (rewrite_type_header selector rewriter) headers,
        Option.map (rewrite_type_expr selector rewriter) e_opt
    )

let rec _rewrite_stmt_stmt recurse selector rewriter place = 
    let auto_place smth = {place = place; value=smth} in
    let rewrite_stmt_stmt = rewrite_stmt_stmt recurse selector rewriter in

    (* TODO use it in all rewrite_XX_stmt that generates list of stmt *)
    let stmts2stmt : stmt list -> stmt = function 
        | [] -> auto_place EmptyStmt
        | [stmt] -> stmt
        | stmts -> auto_place (BlockStmt stmts) 
    in

    function 
    | stmt when recurse && selector stmt-> List.flatten (List.map (_rewrite_stmt_stmt recurse selector rewriter place) (rewriter place stmt)) (* recursive - try to rewrite until no more reductions *)
    | stmt when selector stmt-> rewriter place stmt
    | EmptyStmt -> [EmptyStmt]
    | AssignExpr (x, e) -> [AssignExpr (x, e)]
    | AssignThisExpr (x, e) -> [AssignThisExpr (x, e)]
    | AssignSelfExpr (x, e) -> [AssignSelfExpr (x, e)]
    | LetStmt (mt, x, e) -> [LetStmt (mt, x,  e)]
    | CommentsStmt c -> [CommentsStmt c]
    | BreakStmt -> [BreakStmt]
    | ContinueStmt -> [ContinueStmt]
    | ExitStmt i -> [ExitStmt i]
    | ForeachStmt (mt, x, e, stmt) ->
        [ForeachStmt(mt, x, e, auto_place (BlockStmt (rewrite_stmt_stmt stmt)))]
    | IfStmt (e, stmt1, stmt2_opt) ->
        [IfStmt (
            e,
            auto_place (BlockStmt (rewrite_stmt_stmt stmt1)),
            Option.map (function stmt -> auto_place (BlockStmt (rewrite_stmt_stmt stmt))) stmt2_opt
        )]
    | MatchStmt (e, branches) ->
        [MatchStmt (
                e,
            List.map (function (e, stmt) -> e, (auto_place (BlockStmt (rewrite_stmt_stmt stmt)))) branches
        )]
    | ReturnStmt e -> [ReturnStmt e]
    | ExpressionStmt e -> [ExpressionStmt e]
    | BlockStmt stmts -> [BlockStmt (List.flatten (List.map rewrite_stmt_stmt stmts))] 
    | GhostStmt stmt -> List.map (function stmt -> GhostStmt stmt) (rewrite_stmt_stmt stmt)
    | WithContextStmt (anonymous_mod, cname, e, stmts) -> [
        WithContextStmt(anonymous_mod, cname, e, List.flatten (List.map rewrite_stmt_stmt stmts))
    ]
and rewrite_stmt_stmt recurse selector (rewriter:Error.place -> _stmt -> _stmt list) = map_places (_rewrite_stmt_stmt recurse selector rewriter)


(*****************************************************)
let rec collect_stype_stype_ parent_opt already_binded selector collector place (st:_session_type) =
    let collect_stypes stypes = 
        List.fold_left (fun (acc0, acc1) stype -> 
            let _, collected_elts, ftvars = collect_stype_stype parent_opt already_binded selector collector stype in
            collected_elts@acc0, ftvars@acc1
        ) ([], []) stypes 
    in

    (* Collection *)
    let collected_elts0 = if selector st then collector parent_opt already_binded {place; value=st} else [] in 

    match st with
    | STEnd -> already_binded, collected_elts0, []
    | STVar x | STPolyVar x when Atom.Set.find_opt x already_binded <> None  -> already_binded, collected_elts0, []
    | STVar x | STPolyVar x -> already_binded, collected_elts0, [x]
    | STRecv (mt, st) | STSend (mt, st) -> 
    let _, collected_elts1, ftvars1 = collect_stype_mtype parent_opt already_binded selector collector mt in
    let _, collected_elts2, ftvars2 = collect_stype_stype parent_opt already_binded selector collector st in
    already_binded, collected_elts0@collected_elts1@collected_elts2, ftvars1@ftvars2
    | STBranch branches | STSelect branches -> 
    let sts =  List.map (function (_,st,_) -> st) branches in
    let acs =  List.map Option.get (List.filter (function x -> x <> None) (List.map (function (_,_,ac) -> ac) branches)) in
    let collected_elts1, ftvars1 = collect_stypes sts in
    let collected_elts2, ftvars2 = collect_stype_aconstraints parent_opt already_binded selector collector acs in
    already_binded, collected_elts0@collected_elts1@collected_elts2, ftvars1@ftvars2 
    | STRec (x, st) -> 
    let inner_already_binded = (Atom.Set.add x already_binded) in
    let _, collected_elts, ftvars = collect_stype_stype parent_opt inner_already_binded selector collector st in
    already_binded, collected_elts0@collected_elts, ftvars
    | STDual st -> 
    let _, collected_elts, ftvars = collect_stype_stype parent_opt already_binded selector collector st in
    already_binded, collected_elts0@collected_elts, ftvars
    | STInline x -> raise (Error.DeadbranchError (Printf.sprintf "STInline should have been resolved before running collect_stype_stype_ %s" (Atom.to_string x)))
and collect_stype_stype parent_opt already_binded selector collector st =       
map0_place (collect_stype_stype_ parent_opt already_binded selector collector) st 
and collect_stype_mtype parent_opt already_binded selector collector =
    collect_type_mtype ~flag_tcvar:false parent_opt already_binded 
        (function | SType _ -> true | _-> false) 
        (fun parent_opt already_binded -> 
            function | {value=SType st} -> 
                let _, elts, _ = collect_stype_stype parent_opt already_binded selector collector st in 
                elts
        )
and collect_stype_aconstraint parent_opt already_binded selector rewriter =
collect_type_aconstraint false parent_opt already_binded
    (function | SType _ -> true | _-> false) 
    (fun parent_opt already_binded -> 
        function | {value=SType st} -> 
            let _, elts, _ = collect_stype_stype parent_opt already_binded selector rewriter st in
            elts
    )
and collect_stype_aconstraints parent_opt already_binded selector collector (aconstraints: applied_constraint list) = 
    List.fold_left (fun (acc0, acc1) aconstraint -> 
        let _, collected_elts, ftvars = collect_stype_aconstraint parent_opt already_binded selector collector aconstraint in
        collected_elts@acc0, ftvars@acc1
    ) ([], []) aconstraints 

let rec _rewrite_stype_stype selector rewriter place = 
let rewrite_stype = rewrite_stype_stype selector rewriter in    
let rewrite_stype_branch (x, st, ac_opt) = (x, rewrite_stype st, Option.map (rewrite_stype_aconstraint selector rewriter) ac_opt) in
function 
| st when selector st -> rewriter st
| STEnd -> STEnd
| STInline x -> STInline x 
| STDual st -> STDual (rewrite_stype st )
| STBranch entries -> 
    STBranch (List.map rewrite_stype_branch entries) 
| STRec (x, st) -> STRec (x, rewrite_stype st) 
| STRecv (mt, st) -> 
    STRecv (
        rewrite_stype_mtype selector rewriter mt, 
        rewrite_stype st)
| STSelect entries -> 
    STSelect (List.map rewrite_stype_branch entries) 
| STSend (mt, st) -> 
    STSend (
        rewrite_stype_mtype selector rewriter mt, 
        rewrite_stype st)
| STVar x -> STVar x
| STPolyVar x -> STPolyVar x
and rewrite_stype_stype selector rewriter = map_place (_rewrite_stype_stype selector rewriter)
and rewrite_stype_mtype selector rewriter =
rewrite_type_mtype 
    (function | SType _ -> true | _-> false)
    (function 
        | SType st -> SType (rewrite_stype_stype selector rewriter st)
        | _ -> raise (Error.DeadbranchError "selector prevents accessing this branch")
    )
and rewrite_stype_aconstraint selector rewriter =
rewrite_type_aconstraint 
    (function | SType _ -> true | _-> false) 
    (function 
        | SType st -> SType (rewrite_stype_stype selector rewriter st)
        | _ -> raise (Error.DeadbranchError "selector prevents accessing this branch")
    )


(*****************************************************)
        let protect_renaming renaming = function x ->
            if Atom.is_builtin x then x 
            else renaming x

        let rec _rename_composed_type renaming place = 
            let rmt = rename_main_type renaming in   
        function  
        | TActivationRef mt -> TActivationRef (rmt mt)
        | TArrow (mt1, mt2) -> TArrow (rmt mt1, rmt mt2)
        | TVar x -> TVar (renaming x)
        | TFlatType _ as ct -> ct
        | TArray mt -> TArray (rmt mt)
        | TDict (mt1, mt2) -> TDict (rmt mt1, rmt mt2)
        | TList mt -> TList (rmt mt)
        | TOption mt -> TOption (rmt mt)
        | TResult (mt1, mt2) -> TResult (rmt mt1, rmt mt2)
        | TSet mt -> TSet (rmt mt)
        | TTuple (mts) -> TTuple (List.map rmt mts)
        | TVPlace mt -> TVPlace (rmt mt)
        | TUnion (mt1, mt2) -> TUnion (rmt mt1, rmt mt2)
        | TBridge tb -> TBridge {
            in_type = rmt tb.in_type;
            out_type = rmt tb.out_type;
            protocol = rmt tb.protocol;
        }
        | TInport mt -> TInport (rmt mt)
        | TEport mt -> TEport (rmt mt)
        | TOutport mt -> TOutport (rmt mt)
        | TForall (x, mt) -> TForall (renaming x, rmt mt)
        | TPolyVar x -> TPolyVar (renaming x)
        and rename_composed_type renaming = map_place (_rename_composed_type (protect_renaming renaming))

        and _rename_session_type renaming place = 
            let rmt = rename_main_type renaming in   
            let rst = rename_session_type renaming in   
            let rb (x, st, ac_opt) = (renaming x, rst st, Option.map (rename_applied_constraint renaming) ac_opt) in
        function  
        | STEnd | STWildcard -> STEnd
        | STVar x -> STVar (renaming x)
        | STSend (mt, st) -> STSend (rmt mt, rst st)
        | STRecv (mt, st) -> STRecv (rmt mt, rst st)
        | STBranch branches -> STBranch (List.map rb branches)
        | STSelect branches -> STSelect (List.map rb branches)
        | STRec (x, st) -> STRec (renaming x, rst st)
        | STInline x -> STInline (renaming x)
        | STPolyVar x -> STPolyVar (renaming x)
        | STDual st -> STDual (rst st)
        | STBottom  -> STBottom
        and rename_session_type renaming : session_type -> session_type = map_place (_rename_session_type (protect_renaming renaming))

        and _rename_struct_type renaming place = 
            let rmt = rename_main_type renaming in   
        function  
        | CompTUid x -> CompTUid (renaming x)
        | TStruct (x, sign) -> TStruct 
            (renaming x, Atom.VMap.fold (fun x mt acc -> Atom.VMap.add (renaming x) (rmt mt) acc) sign Atom.VMap.empty)
        | TPolyCVar x -> TPolyCVar (renaming x)
        and rename_struct_type renaming = map_place (_rename_struct_type renaming)

        and _rename_main_type renaming place = 
            let rmt = rename_main_type renaming in   
        function  
        | EmptyMainType -> EmptyMainType
        | CType ct -> CType (rename_composed_type renaming ct)
        | SType st -> SType (rename_session_type renaming st)
        | ClType st -> ClType (rename_struct_type renaming st)
        | CompType cmt -> CompType (rename_struct_type renaming cmt)
        | ConstrainedType (mt, ac) -> ConstrainedType (rmt mt, rename_applied_constraint renaming ac)
        and rename_main_type renaming = map_place (_rename_main_type (protect_renaming renaming))

        and _rename_constraint_header renaming place = function
        | UseMetadata (mt, x) -> UseMetadata (rename_main_type renaming mt, renaming x)
        | SetTimer x -> SetTimer (renaming x)
        | SetFireTimer (x,i) -> SetFireTimer (renaming x, i)
        and rename_constraint_header renaming = map_place (_rename_constraint_header (protect_renaming renaming))

        and rename_applied_constraint renaming (headers,e_opt) = 
            List.map (rename_constraint_header renaming) headers, Option.map (rename_expr true renaming) e_opt

        and _rename_literal flag_rename_type renaming place lit = 
        match lit with
        | VoidLit | BoolLit _ | FloatLit _ | IntLit _ | LabelLit _ | StringLit _ | ActivationRef _ | Place _  -> lit
        | BLabelLit x -> BLabelLit (if flag_rename_type then renaming x else x)
        | VPlace vp ->
            let rec rename_vp (vp:vplace) =  {
                name = renaming vp.name;
                nbr_instances = rename_expr true renaming vp.nbr_instances;
                features = vp.features;
                children = List.map rename_vp vp.children
            } in
            VPlace (rename_vp vp)
        | StaticBridge b -> StaticBridge {
            id = renaming b.id;
            protocol_name = renaming b.protocol_name
        }
        and rename_literal flag_rename_type renaming = map_place (_rename_literal flag_rename_type (protect_renaming renaming)) 

        and _rename_expr flag_rename_attribute flag_rename_type renaming place (e, mt_e) = 
            logger#debug "rename_expr [%b]" flag_rename_attribute;
            let re = rename_expr ~flag_rename_attribute:flag_rename_attribute flag_rename_type renaming in
            let rmt = if flag_rename_type then rename_main_type renaming else Fun.id in

            let rename_attribute = false in (* TODO expose argument, renaming attributes is wrong except if type schemas have been changed *)

            let e = match e with 
            | EmptyExpr -> EmptyExpr
            | VarExpr x -> VarExpr (renaming x)
            | ImplicitVarExpr x -> ImplicitVarExpr (renaming x)
            | InterceptedActivationRef (e1, e2_opt) -> InterceptedActivationRef (re e1, Option.map re e2_opt) 
            | ActivationAccessExpr (x, e, y) -> 
                ActivationAccessExpr (renaming x, re e, if flag_rename_attribute then renaming y else y)
            | AccessExpr (e1, ({value=VarExpr _,_} as e2)) -> 
                logger#debug "rename accessExpr [%b] \n" (flag_rename_attribute);
                AccessExpr (re e1, if flag_rename_attribute then re e2 else e2)
            | AccessExpr (e1, e2) -> 
                logger#debug "rename accessExpr \n%s" (show_expr e2);
                AccessExpr (re e1, re e2)
            | BinopExpr (e1, op, e2) -> BinopExpr (re e1, op, re e2)
            | LambdaExpr (params, e) -> 
                LambdaExpr (
                    List.map (map_place (fun _ (mt,x) -> rmt mt, renaming x)) params,
                    re e) 
            | LitExpr l -> LitExpr (rename_literal flag_rename_type renaming l)
            | UnopExpr (op, e) -> UnopExpr (op, re e)
            | CallExpr (e, es) -> 
                logger#debug "rename callExpr %b\n%s" flag_rename_attribute (show_expr e);
                CallExpr (re e, List.map re es)
            | NewExpr (e, es) -> NewExpr (re e, List.map re es)
            | PolyApp (e, mts) -> PolyApp (re e, List.map rmt mts)
            | BridgeCall{protocol_name} -> BridgeCall{
                protocol_name = renaming protocol_name;
            }
            | TernaryExpr (e1, e2, e3) -> TernaryExpr (re e1, re e2, re e3)
            | This -> This
            | Self -> Self
            | Spawn spawn -> Spawn {
                c = rename_component_expr renaming spawn.c;
                args = List.map re spawn.args;
                at = Option.map re spawn.at;
                inline_in = Option.map re spawn.inline_in;
            }
            | BoxCExpr ce -> BoxCExpr (rename_component_expr renaming ce)
            | OptionExpr e_opt -> OptionExpr (Option.map re e_opt)
            | ResultExpr (e1_opt, e2_opt) -> ResultExpr (Option.map re e1_opt, Option.map re e2_opt)
            | BlockExpr (b, es) -> BlockExpr (b, List.map re es)
            | Block2Expr (b, ees) -> Block2Expr (b,
                List.map (function (e1, e2) -> (re e1, re e2)) ees
            )
            in
            (e, rmt mt_e)
        and rename_expr ?(flag_rename_attribute=false) flag_rename_type renaming : expr -> expr = map_place (_rename_expr flag_rename_attribute flag_rename_type (protect_renaming renaming))

        and _rename_stmt flag_rename_attribute flag_rename_type renaming place = 
            logger#debug "rename_stmt [%b]" flag_rename_attribute;
            let re = rename_expr ~flag_rename_attribute:flag_rename_attribute flag_rename_type renaming in
            let rmt = if flag_rename_type then rename_main_type renaming else Fun.id in
            let rstmt = rename_stmt ~flag_rename_attribute:flag_rename_attribute flag_rename_type renaming in
        function
        | EmptyStmt -> EmptyStmt
        | AssignExpr (x, e) -> AssignExpr (renaming x, re e)
        | AssignThisExpr (x, e) -> AssignThisExpr (renaming x, re e)
        | AssignSelfExpr (x, e) -> AssignSelfExpr (renaming x, re e)
        | LetStmt (mt, x, e) -> LetStmt (rmt mt, renaming x, re e)
        | CommentsStmt _ as stmt -> stmt
        | BreakStmt -> BreakStmt
        | ContinueStmt -> ContinueStmt
        | ExitStmt i -> ExitStmt i
        | ForeachStmt (mt, x, e, stmt) -> ForeachStmt (rmt mt, renaming x, re e, rstmt stmt)
        | IfStmt (e, stmt1, stmt2_opt) -> IfStmt (re e, rstmt stmt1, Option.map rstmt stmt2_opt)
        | MatchStmt (e, branches) -> MatchStmt (re e, List.map (function (e, stmt) -> (re e, rstmt stmt)) branches) 
        | ReturnStmt e -> ReturnStmt (re e)
        | ExpressionStmt e -> ExpressionStmt (re e)
        | BlockStmt stmts -> BlockStmt (List.map rstmt stmts)
        | GhostStmt stmt -> GhostStmt (rstmt stmt)
        | WithContextStmt (flag, x, e, stmts) -> WithContextStmt (flag, renaming x, re e, List.map rstmt stmts)
        and rename_stmt ?(flag_rename_attribute=false) (flag_rename_type:bool) renaming = map_place (_rename_stmt flag_rename_attribute flag_rename_type (protect_renaming renaming))

        and _rename_param renaming place (mt, x) = (rename_main_type renaming mt, renaming x)
        and rename_param renaming = map_place (_rename_param (protect_renaming renaming))


        and _rename_port flag_rename_attribute renaming place ((p, mt_p): _port * main_type)  = ({
            name = renaming p.name;
            expecting_st = rename_main_type renaming p.expecting_st;
            _disable_session = p._disable_session;
            callback = rename_expr ~flag_rename_attribute:flag_rename_attribute true renaming p.callback;
            _children = List.map renaming p._children;
            _is_intermediate = p._is_intermediate;
            _receive_id = p._receive_id;
        }, rename_main_type renaming mt_p)
        and rename_port ?(flag_rename_attribute=false) renaming = map_place (_rename_port flag_rename_attribute (protect_renaming  renaming))

        and _rename_eport flag_rename_attribute renaming place ((p, mt_p): _eport * main_type)  = ({
            name = renaming p.name;
            expecting_mt = rename_main_type renaming p.expecting_mt;
            callback = rename_expr ~flag_rename_attribute:flag_rename_attribute true renaming p.callback;
        }, rename_main_type renaming mt_p)
        and rename_eport ?(flag_rename_attribute=false) renaming = map_place (_rename_eport flag_rename_attribute (protect_renaming  renaming))

        and _rename_outport flag_rename_attribute renaming place ((p, mt_p): _outport * main_type) = ({
            name = renaming p.name;
            protocol = rename_main_type renaming p.protocol;
            _children = List.map renaming p._children;
        }, rename_main_type renaming mt_p)
        and rename_outport ?(flag_rename_attribute=false) renaming = map_place (_rename_outport flag_rename_attribute (protect_renaming renaming))

        and _rename_component_expr flag_rename_attribute renaming place (ce, mt_ce) = 
            let rce = rename_component_expr renaming in
            let re = rename_expr ~flag_rename_attribute:flag_rename_attribute true renaming in
            let ce = match ce with
            | VarCExpr x -> VarCExpr (renaming x)
            | AppCExpr (ce, ces) -> AppCExpr (rce ce, List.map rce ces)
            | UnboxCExpr e -> UnboxCExpr (re e)
            | AnyExpr e -> AnyExpr (re e)
            in 
            (ce, rename_main_type renaming mt_ce)
        and rename_component_expr ?(flag_rename_attribute=false) renaming = map_place (_rename_component_expr flag_rename_attribute (protect_renaming renaming))
(*****************************************************)

(* elim place *)
let rec equal_place inner_equal (x1:'a AstUtils.placed) (x2:'a AstUtils.placed) = inner_equal (x1.value, x2.value)

let rec _equal_ctype = function 
| TActivationRef mt1, TActivationRef mt2 | TArray mt1, TArray mt2 | TList mt1, TList mt2 | TOption mt1, TOption mt2 | TSet mt1, TSet mt2 | TVPlace mt1, TVPlace mt2 ->
    equal_mtype mt1 mt2
| TArrow (mt1_a, mt1_b), TArrow (mt2_a, mt2_b) | TDict (mt1_a, mt1_b), TDict (mt2_a, mt2_b) | TResult (mt1_a, mt1_b), TResult (mt2_a, mt2_b) | TUnion (mt1_a, mt1_b), TUnion (mt2_a, mt2_b) ->
    equal_mtype mt1_a mt2_a && equal_mtype mt1_b mt1_b
| TVar x1, TVar x2 -> x1 = x2
| TFlatType TInt, TFlatType TTimer | TFlatType TTimer, TFlatType TInt -> true 
| TFlatType ft1, TFlatType ft2 -> ft1 = ft2
| TTuple mts1, TTuple mts2 -> List.equal equal_mtype mts1 mts2
| TInductive mts1, TInductive mts2 -> List.equal equal_mtype mts1 mts2
| TBridge b1, TBridge b2 ->
    equal_mtype b1.in_type b2.in_type &&
    equal_mtype b1.out_type b2.out_type &&
    equal_mtype b1.protocol b2.protocol
| TPolyVar x1, TPolyVar x2 -> x1 = x2
| TForall (x1, mt1), TForall (x2, mt2) ->
    let mt2' = replace_type_main_type x2 (Some x1, None) mt2 in
    equal_mtype mt1 mt2'
| _ -> false
and equal_ctype ct1 ct2 = 
(=) ct1 ct2 || (* optimization *)
equal_place _equal_ctype ct1 ct2

and _equal_stype = function
| STEnd, STEnd -> true 
| STSend (mt1,st1), STSend (mt2, st2) | STRecv (mt1,st1), STRecv (mt2, st2) ->
    equal_mtype mt1 mt2 && equal_stype st1 st2
| STSelect branches1, STSelect branches2 | STBranch branches1, STBranch branches2->
    List.equal (fun (x1, st1, guard_opt1) (x2, st2, guard_opt2) ->
        x1 = x2 &&
        equal_stype st1 st2 &&
        Option.equal equal_applied_constraint guard_opt1  guard_opt2
    ) branches1 branches2
| STVar x1, STVar x2 -> x1 = x2
| STRec (x1, st1), STRec (x2, st2) ->
    let st2' = replace_stype_session_type x2 (Some x1, None) st2 in
    equal_stype st1 st2'
| STInline x1, STInline x2 -> x1 = x2
| STPolyVar x1, STPolyVar x2 -> x1 = x2
| _ -> false
and equal_stype st1 st2 = 
(=) st1 st2 ||
equal_place _equal_stype st1 st2


and _equal_structtype = function
| CompTUid x1, CompTUid x2 -> x1 = x2
| TStruct (_, struct1), TStruct (_, struct2) -> 
    (* N.B names of schema does not account for equality nor subtyping *)
    failwith "FIXME TODO equality for TStruct i.e. replace list by hashtbl"
| TPolyCVar x1, TPolyCVar x2 -> x1 = x2
| _ -> false
and equal_structtype cmt1 cmt2 =
(=) cmt1 cmt2 ||
equal_place _equal_structtype cmt1 cmt2

and _equal_mtype = function
| EmptyMainType, EmptyMainType -> true 
| CType ct1, CType ct2 -> equal_ctype ct1 ct2
| SType st1, SType st2 -> equal_stype st1 st2
| CompType cmt1, CompType cmt2 -> equal_structtype cmt1 cmt2
| ConstrainedType (mt1, ac1), ConstrainedType (mt2, ac2) ->
    equal_mtype mt1 mt2 &&
    failwith "TODO equal guard not yet defined"
| _ -> false
and equal_mtype mt1 mt2 = 
(=) mt1 mt2 ||
equal_place _equal_mtype mt1 mt2

(* return flag * variable to replace for equality of the second argument*)
and _equal_header = function
| UseMetadata (mt1, x1), UseMetadata (mt2, x2) ->
    equal_mtype mt1 mt2, Some (x1,x2)
| SetTimer x1, SetTimer x2 -> true, Some (x1, x2)
| SetFireTimer (x1, i1), SetFireTimer (x2, i2) -> i1 = i2, Some (x1, x2)
| _ -> false, None
and equal_header (h1, h2) = 
equal_place _equal_header h1 h2

and equal_applied_constraint (hs1, g1_opt) (hs2, g2_opt) = 
    let tmp = List.map equal_header (List.combine hs1 hs2) in
    let flag = List.exists (function | (false,_)-> false | _ -> true) tmp in
    let vars = List.filter_map Fun.id (snd(List.split tmp)) in
    
    let g2_opt' : constraints option = Option.map (function g2 ->
        {
            place = g2.place;
            value = List.fold_left (fun (e2, mt) (x1, x2) -> 
            (replace_expr_expr x2 (Some x1, None) {place=g2.place;value=(e2, mt)}).value    
        ) g2.value vars
        }

    ) g2_opt in

    (* TODO FIXME renaming is not propgated to the remaining part of the protocol .......... Protocol definition (with headers should refactorized) 
    TODO WARNING
    *)


    flag &&
    Option.equal (fun {value=(e1,_)} {value=(e2, _)} -> _equal_expr (e1, e2)) g1_opt g2_opt'


and _equal_expr = function
| VarExpr x1, VarExpr x2 -> x1 = x2
| AccessExpr (e1_a, e1_b), AccessExpr (e2_a, e2_b) ->
    equal_expr e1_a e2_a &&
    equal_expr e1_b e2_b
| BinopExpr (e1_a, op1, e1_b), BinopExpr (e2_a, op2, e2_b) ->
    equal_expr e1_a e2_a &&
    op1 = op2 &&
    equal_expr e1_b e2_b
| LambdaExpr (params1, e1), LambdaExpr (params2, e2) -> 
    (* equality under alpha renaming *)
    if List.length params1 <> List.length params2 then
        false
    else begin
        let pparams = List.combine params1 params2 in

        let mt_flag = List.fold_left (fun acc ({value=mt1,_}, {value=mt2,_}) ->
            acc && equal_mtype mt1 mt2
        ) true pparams in

        (* alpha renaming *)
        let e2' = List.fold_left (fun e2 ({value=_, x1}, {value=_, x2}) ->
            replace_expr_expr x2 (Some x1, None) e2
        ) e2 pparams in

        mt_flag &&
        equal_expr e1 e2'
    end
| LitExpr l1, LitExpr l2 -> l1 = l2
| UnopExpr(op1, e1), UnopExpr (op2, e2) -> 
    op1 = op2 &&
    equal_expr e1 e2
| CallExpr (e1, es1), CallExpr (e2, es2) | NewExpr (e1, es1), NewExpr (e2, es2) ->
    equal_expr e1 e2 &&
    List.equal equal_expr es1 es2 
| This, This -> true
| Self, Self -> true
| Spawn sp1, Spawn sp2 ->
    equal_cexpr sp1.c sp2.c &&
    List.equal equal_expr sp1.args sp2.args &&
    Option.equal equal_expr sp1.at sp2.at &&
    Option.equal equal_expr sp1.inline_in sp2.inline_in
| BoxCExpr ce1, BoxCExpr ce2 -> equal_cexpr ce1 ce2
| OptionExpr e1_opt, OptionExpr e2_opt -> Option.equal equal_expr e1_opt e2_opt
| ResultExpr (e1a_opt, e1b_opt), ResultExpr (e2a_opt, e2b_opt) -> 
    Option.equal equal_expr e1a_opt e2a_opt &&
    Option.equal equal_expr e1b_opt e2b_opt
| BlockExpr (b1, es1), BlockExpr (b2, es2) ->
    b1 = b2 &&
    List.equal equal_expr es1 es2
| Block2Expr (b1, ees1), Block2Expr (b2, ees2) ->
    b1 = b2 &&
    List.equal (fun (e1a,e1b) (e2a, e2b) -> 
        equal_expr e1a e2a &&
        equal_expr e2a e2b
    ) ees1 ees2
| _ -> false
and equal_expr (e1:expr) (e2:expr) =
(=) e1 e2 ||
_equal_expr ((fst e1.value), (fst e2.value))

and _equal_cexpr = function
| VarCExpr x1, VarCExpr x2 -> x1 = x2
| AppCExpr (ce1a, cesa), AppCExpr (ce2a, cesb) ->
    equal_cexpr ce1a ce2a &&
    List.equal equal_cexpr cesa  cesb
| UnboxCExpr e1, UnboxCExpr e2 -> equal_expr e1 e2
| AnyExpr e1, AnyExpr e2 -> equal_expr e1 e2
| _ -> false 
and equal_cexpr ce1 ce2 = 
(=) ce1 ce2 ||
_equal_cexpr ((fst ce1.value), (fst ce2.value))