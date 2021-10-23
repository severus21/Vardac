(*  resolve preprocessed commands
    - use: resolve + cycle detection
  
    ghost:
    - remove ghost annotations or remove ghost item 
*)
open Core.Error

(* The source calculus. *)
module S = Ast 
(* The target calculus. *)
module T = Ast 

module UseSet = Set.Make(String)
let use_env = ref UseSet.empty 

let rec resolve_placed resolve_value ({ Core.AstUtils.place ; Core.AstUtils.value}: 'a Core.AstUtils.placed) = 
    let value =  resolve_value value in
    {Core.AstUtils.place; Core.AstUtils.value}

let rec resolve_list_placed resolve_value ({ Core.AstUtils.place ; Core.AstUtils.value}: 'a Core.AstUtils.placed) = 
    let value =  resolve_value value in
    List.map (function value -> {Core.AstUtils.place; Core.AstUtils.value}) value

let rec resolve_expr : S._expr -> T._expr = 
function
| S.LambdaExpr (x, mt, e) -> T.LambdaExpr (x, mt, resolve_placed resolve_expr e)
(* Unconcerned constructors *)
| S.AccessExpr (e1,e2) -> S.AccessExpr (rexpr e1, rexpr e2) 
| S.BinopExpr (e1, op, e2) -> S.BinopExpr (rexpr e1, op, rexpr e2) 
| S.UnopExpr (op, e1) -> S.UnopExpr (op, rexpr e1) 
| S.CallExpr (e, args) -> S.CallExpr (e, List.map rexpr args) 
| S.Spawn spawn -> S.Spawn {
        c= resolve_placed resolve_component_expr spawn.c;
        args= List.map rexpr spawn.args;
        at= Option.map rexpr spawn.at
    } 
| S.BoxCExpr cexpr -> S.BoxCExpr (resolve_placed resolve_component_expr cexpr) 
| S.OptionExpr e_opt -> S.OptionExpr (Option.map rexpr e_opt)
| S.ResultExpr (e1_opt, e2_opt) -> S.ResultExpr (Option.map rexpr e1_opt, Option.map rexpr e2_opt)
| S.BlockExpr (b, es) -> S.BlockExpr (b, List.map rexpr es)
| S.Block2Expr (b, e2s) -> S.Block2Expr (b, List.map (function (x,y) -> (rexpr x, rexpr y)) e2s)

| x -> x
and rexpr e = resolve_placed resolve_expr e

and resolve_stmt : S._stmt -> T._stmt = function 
| S.AssignExpr (v, e) -> T.AssignExpr (v, rexpr e)
| S.AssignThisExpr (v, e) -> T.AssignThisExpr (v, rexpr e)
| S.LetExpr (mt, v, e) -> T.LetExpr (mt, v, rexpr e)
| S.ForStmt (mt, v, e, stmt) -> T.ForStmt (mt, v, rexpr e, rstmt stmt)
| S.IfStmt (e, stmt1, stmt2_opt) -> T.IfStmt (rexpr e, rstmt stmt1,  Option.map rstmt stmt2_opt)
| S.MatchStmt (e, entries) -> T.MatchStmt (rexpr e, List.map (function (e1, stmt) -> (e1, rstmt stmt)) entries)
| S.ReturnStmt e -> T.ReturnStmt (rexpr e)
| S.BlockStmt stmts -> T.BlockStmt ((List.map rstmt) stmts)
| S.ExpressionStmt e -> T.ExpressionStmt (rexpr e)
| S.GhostStmt gstmt -> begin
    if Core.Config.keep_ghost () then
        (rstmt gstmt).value
    else
        T.EmptyStmt
end
| x -> x
and rstmt stmt = resolve_placed resolve_stmt stmt

and resolve_state : S._state -> T._state = function 
| S.StateDcl sdcl -> T.StateDcl {sdcl with init_opt= Option.map rexpr sdcl.init_opt}
| x -> x

and resolve_contract (c:S._contract) : T._contract = 
{ c with    pre_binders= (List.map (function (mt,v,e) -> (mt,v, rexpr e)) c.pre_binders);  
            ensures= Option.map rexpr c.ensures; 
            returns= Option.map rexpr c.returns;
}

and resolve_method (m: S._method0) : T._method0 =
    { m with
        abstract_impl= List.map rstmt m.abstract_impl   
    }

and rmethod m = resolve_placed resolve_method m

and resolve_port (port:S._port) : T._port =
{ port with input= rexpr port.input;
            callback= rexpr port.callback }

and resolve_component_item : S._component_item -> T._component_item list = function
| S.State s -> [ T.State (resolve_placed resolve_state s) ]
| S.Method m -> [ T.Method (resolve_placed resolve_method m) ]
| S.Contract c -> [ T.Contract (resolve_placed resolve_contract c) ]
| S.Port p -> [ T.Port (resolve_placed resolve_port p) ]
| S.Term t -> List.map (function x -> T.Term x) (resolve_term t)
| S.Include cexpr -> [ T.Include (resolve_placed resolve_component_expr cexpr) ]

and resolve_component_dcl : S._component_dcl -> T._component_dcl = function 
| S. ComponentStructure cstruct -> T.ComponentStructure {cstruct with body = List.flatten (List.map (resolve_list_placed resolve_component_item) cstruct.body)}
| S.ComponentAssign cassign -> T.ComponentAssign {cassign with value = resolve_placed resolve_component_expr cassign.value}

and resolve_component_expr : S._component_expr -> T._component_expr = function 
| S.AppCExpr (ce1,ce2) -> T.AppCExpr (rcexpr ce1, rcexpr ce2)
| S.UnboxCExpr e -> T.UnboxCExpr (rexpr e)
| S.AnyExpr e -> T.AnyExpr (rexpr e)
| x -> x
and rcexpr cexpr = resolve_placed resolve_component_expr cexpr

and resolve_ppterm {Core.AstUtils.place ; Core.AstUtils.value}: T.term list= 
match value with
| S.UsePP xs -> 
    let libfilename = List.fold_left (fun tmp x -> Filename.concat tmp x) "" xs in (* FIXME *)
    match UseSet.find_opt libfilename (!use_env)  with
    | None -> 
        use_env := UseSet.add libfilename !use_env ;
        resolve_program (Parse.read libfilename)
    | Some _ -> error place "Cyclic import detected: %s" libfilename

and resolve_term {Core.AstUtils.place ; Core.AstUtils.value} : T.term list =
match value with 
| S.PPTerm ppt -> resolve_ppterm ppt 
| S.Stmt stmt -> [ {place; value=T.Stmt (resolve_placed resolve_stmt stmt)} ]
| S.Component cdcl -> [ {place; value=T.Component (resolve_placed resolve_component_dcl cdcl)} ]
| x -> [ {place; value=x} ]

and resolve_program (prog: S.program) : T.program =
    List.flatten (List.map resolve_term prog)
