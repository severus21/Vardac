open Core 
open Core.Error
open Core.Builtin
open Fieldslib

(* The source calculus. *)
module S = Ast 
(* The target calculus. *)
module T = IR 

let fst3 (x,y,z) = x

(* Environments map strings to atoms. *)
module Env = Map.Make(String)

type cookenv = {
    components: IR.variable Env.t; 
    exprs:      IR.variable  Env.t; 
    this:       IR.variable  Env.t; 
    types:      IR.variable  Env.t} [@@deriving fields] 

let fresh_cookenv () = {
    components  = Env.empty;
    exprs       = Env.empty;
    this        = Env.empty; (* current state*)
    types       = Env.empty}

(*debug only*)
let print_cenv cenv =
    let print_keys env = Env.iter (fun x _ -> Printf.printf "%s;" x) env in

    print_newline ();
    print_string "Env = {";

    List.iter (
        function (name, l) -> print_string ("\t"^name^"\n\t\t"); print_keys (l cenv);print_newline (); 
    ) [
        ("components", components);
        ("exprs", exprs); 
        ("this", this);
        ("types", types) ];
    
    print_string "}";
    print_newline ()

(* [bind env x] creates a fresh atom [a] and extends the environment [env]
   with a mapping of [x] to [a]. *)

let bind_component cenv place x =
  if is_builtin_component x then
    error place "Component Keyword %s is reserved." x;

  let a = Atom.fresh x in
  {cenv with components=Env.add x a cenv.components}, a

let bind_expr cenv place x =
  if is_builtin_expr x then
    error place "Keyword %s is reserved." x;

  let a = Atom.fresh x in
  {cenv with exprs=Env.add x a cenv.exprs}, a

let bind_this cenv place x =
  let a = Atom.fresh x in
  {cenv with this=Env.add x a cenv.this}, a 

let bind_type cenv place x =
   if is_builtin_type x then
      error place "Type keyword %s is reserved." x;

  let a = Atom.fresh x in
  {cenv with types=Env.add x a cenv.types}, a

(* Built a cenv in order to link mutual binding between component and mutual binding methods *)
let rec shallow_scan_component_item cenv ({place; value}: S.component_item) : cookenv = 
match value with
| S.Term t -> shallow_scan_term cenv t
| S.Method m -> begin 
    let rec aux (m:S.method0) : cookenv =
    match m.value with 
    | CustomMethod m -> fst (bind_this cenv place m.name) 
    | OnStartup m | OnDestroy m -> aux m
    in aux m
end
| _ -> cenv
and shallow_scan_component_dcl cenv ({place; value}: S.component_dcl) : cookenv = 
(* We do not explore the body of a component *)
match value with
| S.ComponentStructure cdcl  -> 
    fst (bind_component cenv place cdcl.name)
| S.ComponentAssign cdcl -> 
    fst (bind_component cenv place cdcl.name)
and shallow_scan_term cenv ({place; value}: S.term) : cookenv = 
match value with
| S.Component c -> shallow_scan_component_dcl cenv c
| _ -> cenv

let rec cook_place cook_value cenv ({ Core.AstUtils.place ; Core.AstUtils.value}: 'a Core.AstUtils.placed) = 
    let cenv, value = cook_value cenv place value in
    cenv, {Core.AstUtils.place; Core.AstUtils.value}

let rec cook_var_expr cenv place x = 
    if is_builtin_expr x then
        Atom.fresh_builtin x
    else (
    try
        Env.find x cenv.exprs
    with Not_found ->
        error place "Unbound variable: %s" x
    )
and cook_var_type cenv place x = 
    if Str.string_match (Str.regexp "^[A-Z].*") x 0 then
        cook_var_component cenv place x (* hack, FIXME add a specific type for container ???*)
    else begin
        if is_builtin_type x then
            Atom.fresh_builtin x
        else (
        try
            Env.find x cenv.types
        with Not_found ->
            error place "Unbound type variable: %s" x
        )
    end
and cook_var_component cenv place x = 
    if is_builtin_component x then
        Atom.fresh_builtin x
    else (
    try
        Env.find x cenv.components
    with Not_found ->
        error place "Unbound component variable: %s" x
    )
and cook_var_this cenv place x = 
    try
        Env.find x cenv.this
    with Not_found ->
        error place "Unbound this variable: %s" x
(************************************ Types **********************************)
and cook_composed_type cenv place: S._composed_type -> cookenv * T._composed_type = function
| S.TArrow (mt1, mt2) -> cenv, T.TArrow (
    cmtype cenv mt1,
    cmtype cenv mt2
) 

| S.TVar x -> 
    let y = cook_var_type cenv place x in
    cenv, T.TVar y
| S.TFlatType ft -> cenv, T.TFlatType ft 

| S.TDict (mt1, mt2) -> cenv, T.TDict (
    cmtype cenv mt1,
    cmtype cenv mt1
) 
| S.TList mt -> cenv, T.TList (cmtype cenv mt) 
| S.TOption mt -> cenv, T.TOption (cmtype cenv mt)
| S.TResult (mt1, mt2) -> cenv, T.TResult (
    cmtype cenv mt1,
    cmtype cenv mt1
) 
| S.TSet mt -> cenv, T.TSet (cmtype cenv mt)
| S.TTuple mts -> cenv, T.TTuple (List.map (cmtype cenv) mts) 
| S.TBridge {in_type; out_type; protocol } -> 
    cenv, T.TBridge { 
        in_type     = cmtype cenv in_type; 
        out_type    = cmtype cenv out_type; 
        protocol    = cmtype cenv protocol; 
    }
and cctype cenv ct: T.composed_type = snd (cook_place cook_composed_type cenv ct)

and cook_session_type cenv place: S._session_type -> cookenv * T._session_type = function
| S.STEnd  -> cenv, T.STEnd
| S.STVar (x, aconst_opt) ->
    let y = cook_var_type cenv place x in
    cenv, T.STVar (y, Option.map (caconst cenv) aconst_opt)
| S.STSend (mt, st) -> 
    cenv, T.STSend ( cmtype cenv mt, cstype cenv st)
| S.STRecv (mt, st) -> 
    cenv, T.STRecv ( cmtype cenv mt, cstype cenv st)
| S.STBranch entries ->
    let aux (x, st, aconst_opt) = 
        let _, y = bind_type cenv place x in
        let st = cstype cenv st in
        let aconst_opt = Option.map (caconst cenv) aconst_opt in
        (y, st, aconst_opt)
    in
    cenv, T.STBranch (List.map aux entries)            
| S.STSelect entries ->               
    let aux (x, st, aconst_opt) = 
        let _, y = bind_type cenv place x in
        let st = cstype cenv st in
        let aconst_opt = Option.map (caconst cenv) aconst_opt in
        (y, st, aconst_opt)
    in
    cenv, T.STSelect (List.map aux entries)            
| S.STRec (x, st) ->
    let new_cenv, y = bind_type cenv place x in  
    cenv, T.STRec (y, cstype new_cenv st)
| S.STInline x ->  
    let y = cook_var_type cenv place x in  
    cenv, T.STInline y
and cstype cenv st: T.session_type = snd (cook_place cook_session_type cenv st)

and cook_component_type cenv place: S._component_type -> cookenv * T._component_type = function
| S.CompTUid x -> cenv, T.CompTUid (cook_var_type cenv place x)
and ccomptype cenv cmt : T.component_type = snd(cook_place cook_component_type cenv cmt)

and cook_mtype cenv place: S._main_type -> cookenv * T._main_type = function
| S.CType ct -> cenv, T.CType (cctype cenv ct)
| S.SType st -> cenv, T.SType (cstype cenv st)
| S.CompType cmt -> cenv, T.CompType (ccomptype cenv cmt)
| S.ConstrainedType (mt, aconst) ->
    cenv, T.ConstrainedType (cmtype cenv mt,  caconst cenv aconst)
and cmtype cenv mt : T.main_type = snd(cook_place cook_mtype cenv mt)

(******************************** Constraints ********************************)
and cook_constraint_header cenv place: S._constraint_header -> cookenv * T._constraint_header = function
| S.UseGlobal (mt, x) ->
    let new_env, y = bind_type cenv place x in
    new_env, T.UseGlobal (cmtype cenv mt, y)
| S.UseMetadata (mt, x) ->
    let new_env, y = bind_type cenv place x in
    new_env, T.UseGlobal (cmtype cenv mt, y)

and cook_constraint cenv place : S._constraints -> cookenv * T._constraints = function
| S.CExpr e -> cenv, T.CExpr (cexpr cenv e)
and cconst cenv const : T.constraints = snd(cook_place cook_constraint cenv const)

and caconst cenv (headers,const): T.applied_constraint = 
    let (new_cenv, new_headers) : (cookenv * T.constraint_header list )= List.fold_left_map (cook_place cook_constraint_header) cenv headers in
    let new_const = cconst new_cenv const in

    (new_headers, new_const)

(************************************* Literals ******************************)
and cook_literal cenv place : S._literal -> cookenv * T._literal = function
| S.EmptyLit -> cenv, T.EmptyLit
| S.BoolLit b -> cenv, T.BoolLit b
| S.FloatLit f -> cenv, T.FloatLit f 
| S.IntLit i -> cenv, T.IntLit i
| S.LabelLit l -> cenv, T.LabelLit l 
| S.StringLit s -> cenv, T.StringLit s

(** Activations *)
| S.ActivationInfo _ -> cenv, T.ActivationInfo () (* TODO *)
and cliteral cenv lit: T.literal = snd (cook_place cook_literal cenv lit)

and cook_expr cenv place : S._expr -> cookenv * T._expr = function 
(* No binding done in an expression can be propagated outside this expression *)
| S.VarExpr v -> (cenv, T.VarExpr (cook_var_expr cenv place v))
| S.AccessExpr ({place=p_t; value=S.This}, {place=p_v; value=S.VarExpr v}) -> cenv, T.AccessExpr (
    {place=p_t; value=T.This},
    {place=p_v; value= T.VarExpr (cook_var_this cenv p_v v)}) 
| S.AccessExpr ({place=_; value=S.This}, _) -> error place "Illformed [this] usage: should be this.<state name>"
| S.AccessExpr (e1, e2) -> (cenv, T.AccessExpr (
    cexpr cenv e1,
    cexpr cenv e2)) 
| S.BinopExpr (e1, op, e2) -> (cenv, T.BinopExpr (
    cexpr cenv e1,
    op,
    cexpr cenv e2)) 
| S.LambdaExpr (x, stmt) -> 
    let inner_cenv, y = bind_expr cenv place x in 
    (cenv, T.LambdaExpr (y, snd(cstmt inner_cenv stmt)))
| S.LitExpr l -> (cenv, T.LitExpr (cliteral cenv l))
| S.UnopExpr (op, e) -> (cenv, T.UnopExpr (op,
    cexpr cenv e)) 

| S.CallExpr (e1, es) -> (cenv, T.CallExpr (
    cexpr cenv e1,
    List.map (cexpr cenv) es))

| S.This -> cenv, T.This

| S.Spawn spawn -> (cenv, T.Spawn {
    c= snd (ccexpr cenv spawn.c);
    args= List.map (cexpr cenv) spawn.args;
    at= Option.map (cexpr cenv) spawn.at
})

| S.BoxCExpr ce -> (cenv, T.BoxCExpr (snd (ccexpr cenv ce))) 
| S.OptionExpr e_opt -> cenv, T.OptionExpr (Option.map (cexpr cenv) e_opt)
| S.ResultExpr (e1_opt, e2_opt) -> cenv, T.ResultExpr (
    Option.map (cexpr cenv) e1_opt,
    Option.map (cexpr cenv) e2_opt)
| S.BlockExpr (b, es) -> cenv, T.BlockExpr (b, List.map (cexpr cenv) es)
| S.Block2Expr (b, es) -> cenv, T.Block2Expr (b, List.map (function (e1,e2) -> (cexpr cenv e1, cexpr cenv e2)) es)
and cexpr cenv e: T.expr = snd (cook_place cook_expr cenv e)
and cook_expression : S.expr -> T.expr = function e -> 
    cexpr (fresh_cookenv ()) e

and cook_stmt cenv place: S._stmt -> cookenv * T._stmt = function
| S.EmptyStmt -> cenv, T.EmptyStmt
| S.AssignExpr (x, e) ->
    let y = cook_var_expr cenv place x in
    cenv, T.AssignExpr (y, cexpr cenv e) 
| S.AssignThisExpr (x, e) ->
    let y = cook_var_this cenv place x in
    cenv, T.AssignThisExpr (y, cexpr cenv e) 
| S.LetExpr (mt, x, e) ->
    (* new cenv applies to e but also to further stmt *)
    let new_cenv, y = bind_expr cenv place x in
    new_cenv, T.LetExpr (cmtype cenv mt, y, cexpr new_cenv e)
| S.CommentsStmt c -> cenv, T.CommentsStmt c
| S.BreakStmt -> cenv, T.BreakStmt
| S.ContinueStmt -> cenv, T.ContinueStmt
| S.ExitStmt i -> cenv, T.ExitStmt i
| S.ForStmt (x, e, stmt) ->
    (* [new cenv] applies to [stmt] only and [stmt_cenv] does not applies outside the for*)
    let new_cenv, y = bind_expr cenv place x in
    cenv, T.ForStmt (y, cexpr cenv e, snd (cstmt new_cenv stmt))
| S.IfStmt (e, stmt1, stmt2_opt) ->
    (*[stmt_cenv] does not applies outside the if*)
    cenv, T.IfStmt (
        cexpr cenv e,
        snd(cstmt cenv stmt1),
        Option.map (function x -> snd (cstmt cenv x)) stmt2_opt)
| S.MatchStmt (e, entries) ->
    (*[stmt_cenv] does not applies outside the match*)
    cenv, T.MatchStmt (
        cexpr cenv e,
        List.map (function (x,y) -> (
            cexpr cenv x, (*TODO should be a pattern ie bind stuff*)
            snd (cstmt cenv y))
        ) entries
    )
| S.ReturnStmt e -> cenv, T.ReturnStmt (cexpr cenv e)
| S.ExpressionStmt e -> cenv, T.ExpressionStmt (cexpr cenv e)
| S.BlockStmt stmts -> cenv, T.BlockStmt (snd (List.fold_left_map (fun inner_cenv stmt -> cstmt inner_cenv stmt) cenv stmts))
| S.GhostStmt stmt -> 
    let new_cenv, new_stmt = cstmt cenv stmt in
    new_cenv, T.GhostStmt new_stmt
and cstmt cenv : S.stmt -> cookenv * T.stmt = cook_place cook_stmt cenv

(************************************ Component *****************************)
and cook_state cenv place : S._state -> cookenv * T._state = function
| S.StateDcl sdcl ->
    let new_cenv, y = bind_this cenv place sdcl.name in
    new_cenv, T.StateDcl {
                ghost   = sdcl.ghost; 
                kind    = sdcl.kind; 
                type0   = cmtype cenv sdcl.type0;
                name    = y;
                body = Option.map (cexpr cenv) sdcl.init_opt}
| S.StateAlias _ -> failwith "cook: state alias not yet supported" (*TODO*)
and cstate cenv: S.state -> cookenv * T.state = cook_place cook_state cenv


and cook_param cenv place (mt, x) : cookenv * T._param = 
    let new_cenv, y = bind_expr cenv place x in
    new_cenv, (cmtype cenv mt, y)
and cparam cenv: S.param -> cookenv * T.param = cook_place cook_param cenv

and cook_contract cenv place (contract:S._contract): cookenv * T._contract =
    let method_name = cook_var_this cenv place contract.method_name in
    let aux_binder cenv (mt, x, e) =
        let new_cenv, y = bind_expr cenv place x in
        new_cenv, (cmtype cenv mt, y, cexpr cenv e)
    in
    let inner_cenv, pre_binders = List.fold_left_map aux_binder cenv contract.pre_binders in
   
    (* Goal: invariant should be added to ensures and to returns *)
    let invariant = (Option.map (cexpr inner_cenv) contract.invariant) in
    let concat_opt (predicat_opt1:T.expr option) predicat_opt2 =
        match predicat_opt1, predicat_opt2 with 
        | None, None -> None
        | None, Some _ -> predicat_opt2
        | Some _, None -> predicat_opt1
        | Some p1, Some p2 -> Some {
            place = p1.place @ p2.place;
            value = T.BinopExpr (p1, T.And, p2) 
        }
    in
    let ensures = concat_opt invariant (Option.map (cexpr inner_cenv) contract.ensures) in
    let returns = concat_opt invariant (Option.map (cexpr inner_cenv) contract.returns) in
    
    cenv, { method_name; 
            pre_binders; 
            ensures;
            returns}

and ccontract cenv: S.contract -> cookenv * T.contract = cook_place cook_contract cenv


and cook_method0 cenv place : S._method0 -> cookenv * T._method0 = function
| S.CustomMethod m ->
    let new_cenv, name = bind_this cenv place m.name in 
    let inner_cenv, args = List.fold_left_map cparam cenv m.args in

    new_cenv, T.CustomMethod {
        ghost = m.ghost;
        ret_type = cmtype cenv m.ret_type;
        name;
        args;
        contract_opt = None; (* Pairing between contract and method0 is done during a next pass, see. Core.PartialEval.ml *)
        body = match m.abstract_impl with
                            | {place=_;value=S.EmptyStmt} -> None
                            | stmt -> Some (snd(cstmt inner_cenv stmt))
    } 
| S.OnStartup m -> 
    let new_cenv, new_m = cmethod0 cenv m in
    new_cenv, T.OnStartup new_m
| S.OnDestroy m -> 
    let new_cenv, new_m = cmethod0 cenv m in
    new_cenv, T.OnDestroy new_m

and cmethod0 cenv: S.method0 -> cookenv * T.method0 = cook_place cook_method0 cenv



and cook_port cenv place (port:S._port) : cookenv * T._port =
    let new_cenv, name = bind_this cenv place port.name in
    new_cenv, {
                name; 
                input = cexpr cenv port.input;
                expecting_st = cmtype cenv port.expecting_st;
                callback = cexpr cenv port.callback}
and cport cenv: S.port -> cookenv * T.port = cook_place cook_port cenv



and cook_component_item cenv _ : S._component_item -> cookenv * T._component_item = function
| S.State s ->
    let new_cenv, new_s = cstate cenv s in
    new_cenv, T.State new_s
| S.Method m ->
    let new_cenv, new_m = cmethod0 cenv m in
    new_cenv, T.Method new_m
| S.Contract c -> 
    cenv, T.Contract (snd( ccontract cenv c))
| S.Port p ->
    let new_cenv, new_p = cport cenv p in
    new_cenv, T.Port new_p
| S.Term t ->
    let new_cenv, new_t = cterm cenv t in
    new_cenv, T.Term new_t
| S.Include ce -> 
    cenv, T.Include (snd (ccexpr cenv ce))
and ccitem cenv: S.component_item -> cookenv * T.component_item = cook_place cook_component_item cenv

and cook_component_dcl cenv place : S._component_dcl -> cookenv * T._component_dcl = function
| S.ComponentStructure cdcl -> 
    let new_cenv, name = bind_component cenv place cdcl.name in
    let inner_cenv, args = List.fold_left_map cparam cenv cdcl.args in

    (* Prepare env for mutual binding between components *)
    let inner_cenv = List.fold_left shallow_scan_component_item inner_cenv cdcl.body in

    new_cenv, T.ComponentStructure {name;
                                    args;
                                    body = snd (List.fold_left_map ccitem inner_cenv cdcl.body)}
| S.ComponentAssign cdcl -> 
    let new_cenv, name = bind_component cenv place cdcl.name in
    let inner_cenv, args = List.fold_left_map cparam cenv cdcl.args in

    new_cenv, T.ComponentAssign {name;
                                    args;
                                    value = snd (ccexpr inner_cenv cdcl.value) }
and ccdcl cenv: S.component_dcl -> cookenv * T.component_dcl = cook_place cook_component_dcl cenv

(********************** Manipulating component structure *********************)
and cook_component_expr cenv place : S._component_expr -> cookenv * T._component_expr = function
| S.VarCExpr x ->
    let y = cook_var_component cenv place x in
    cenv, T.VarCExpr y
| S.AppCExpr (ce1,ce2) -> cenv, T.AppCExpr (
    snd (ccexpr cenv ce1),
    snd (ccexpr cenv ce2)
)
| S.UnboxCExpr e -> cenv, T.UnboxCExpr (cexpr cenv e)
| S.AnyExpr e -> cenv, T.AnyExpr (cexpr cenv e)
and ccexpr cenv : S.component_expr -> cookenv * T.component_expr = cook_place cook_component_expr cenv

(********************** Signatures *********************)

(************************************ Program *****************************)
and cook_term cenv place : S._term -> cookenv * T._term = function
| S.Comments c -> cenv, T.Comments c
| S.PPTerm _ -> raise (DeadbranchError "No preprocessing term should remains when cooking the AST.")

| S.Stmt stmt ->
    let new_cenv, new_stmt = cstmt cenv stmt in
    new_cenv, T.Stmt new_stmt

| S.Component c ->
    let new_cenv, new_c = ccdcl cenv c in
    new_cenv, T.Component new_c

| S.Typedef (x, mt_opt) ->
    let new_cenv, y = bind_type cenv place x in
    (* Deriving type constructor *)
    let new_cenv, _ = bind_expr new_cenv place (String.lowercase_ascii x) in
    new_cenv, T.Typedef (y, Option.map (cmtype cenv) mt_opt)
and cterm cenv: S.term -> cookenv * T.term = cook_place cook_term cenv

let cook_program places terms =    
    let toplevel_cenv = List.fold_left shallow_scan_term (fresh_cookenv ()) terms in
    snd (List.fold_left_map cterm toplevel_cenv terms)  