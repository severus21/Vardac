(*  resolve preprocessed commands
    - use: resolve + cycle detection
  
    ghost:
    - remove ghost annotations or remove ghost item 
*)
open Core.Error
open Core.AstUtils

(* The source calculus. *)
module S = Ast 
(* The target calculus. *)
module T = Ast 

module UseSet = Set.Make(String)
let use_env = ref UseSet.empty 

let rec resolve_expr place : S._expr -> T._expr = 
function
| S.LambdaExpr (x, mt, e) -> T.LambdaExpr (x, mt, rexpr e)
(* Unconcerned constructors *)
| S.AccessExpr (e1,e2) -> S.AccessExpr (rexpr e1, rexpr e2) 
| S.BinopExpr (e1, op, e2) -> S.BinopExpr (rexpr e1, op, rexpr e2) 
| S.UnopExpr (op, e1) -> S.UnopExpr (op, rexpr e1) 
| S.CallExpr (e, args) -> S.CallExpr (e, List.map rexpr args) 
| S.Spawn spawn -> S.Spawn {
        c= rcexpr spawn.c;
        args= List.map rexpr spawn.args;
        at= Option.map rexpr spawn.at
    } 
| S.BoxCExpr cexpr -> S.BoxCExpr (rcexpr cexpr) 
| S.OptionExpr e_opt -> S.OptionExpr (Option.map rexpr e_opt)
| S.ResultExpr (e1_opt, e2_opt) -> S.ResultExpr (Option.map rexpr e1_opt, Option.map rexpr e2_opt)
| S.BlockExpr (b, es) -> S.BlockExpr (b, List.map rexpr es)
| S.Block2Expr (b, e2s) -> S.Block2Expr (b, List.map (function (x,y) -> (rexpr x, rexpr y)) e2s)

| x -> x
and rexpr e = map_place resolve_expr e

and resolve_stmt place : S._stmt -> T._stmt = function 
| S.AssignExpr (v, e) -> T.AssignExpr (v, rexpr e)
| S.AssignThisExpr (v, e) -> T.AssignThisExpr (v, rexpr e)
| S.LetStmt (mt, v, e) -> T.LetStmt (mt, v, rexpr e)
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
and rstmt stmt = map_place resolve_stmt stmt

and resolve_state place : S._state -> T._state = function 
| S.StateDcl sdcl -> T.StateDcl {sdcl with init_opt= Option.map rexpr sdcl.init_opt}
| x -> x

and resolve_contract place (c:S._contract) : T._contract = 
{ c with    pre_binders= (List.map (function (mt,v,e) -> (mt,v, rexpr e)) c.pre_binders);  
            ensures= Option.map rexpr c.ensures; 
            returns= Option.map rexpr c.returns;
}

and resolve_method place (m: S._method0) : T._method0 =
    { m with
        abstract_impl= List.map rstmt m.abstract_impl   
    }

and rmethod m = map_place resolve_method m

and resolve_port place (port:S._port) : T._port =
{ port with input= rexpr port.input;
            callback= rexpr port.callback }

and resolve_outport place (outport:S._outport) : T._outport =
{ outport with input= rexpr outport.input; }

and resolve_component_item place : S._component_item -> T._component_item list = function
| S.State s -> [ T.State (map_place resolve_state s) ]
| S.Method m -> [ T.Method (map_place resolve_method m) ]
| S.Contract c -> [ T.Contract (map_place resolve_contract c) ]
| S.Port p -> [ T.Port (map_place resolve_port p) ]
| S.Outport p -> [ T.Outport (map_place resolve_outport p) ]
| S.Term t -> List.map (function x -> T.Term x) (resolve_term t)
| S.Include cexpr -> [ T.Include (rcexpr cexpr) ]
and rcitem citem =  List.map (function x -> {place=citem.place; value = x}) (map0_place resolve_component_item citem)
 

and resolve_component_dcl place : S._component_dcl -> T._component_dcl = function 
| S. ComponentStructure cstruct -> T.ComponentStructure {cstruct with body = List.flatten (List.map  rcitem cstruct.body)}
| S.ComponentAssign cassign -> T.ComponentAssign {cassign with value =  rcexpr cassign.value}

and resolve_component_expr place : S._component_expr -> T._component_expr = function 
| S.AppCExpr (ce1, ces) -> T.AppCExpr (rcexpr ce1, List.map rcexpr ces)
| S.UnboxCExpr e -> T.UnboxCExpr (rexpr e)
| S.AnyExpr e -> T.AnyExpr (rexpr e)
| x -> x
and rcexpr cexpr = map_place resolve_component_expr cexpr

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
| S.Stmt stmt -> [ {place; value=T.Stmt (map_place resolve_stmt stmt)} ]
| S.Component cdcl -> [ {place; value=T.Component (map_place resolve_component_dcl cdcl)} ]
| x -> [ {place; value=x} ]

and resolve_program (prog: S.program) : T.program =
    List.flatten (List.map resolve_term prog)


(**********************************************************)

let displayed_pass_shortdescription = "AST is resolved"
let displayed_ast_name = "ResolvedAst"
let show_ast = true
let precondition program = program
let postcondition program = program
let apply_program = resolve_program