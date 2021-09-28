open Core
open Utils
open AstUtils

(* Helper name *)
let a_ASTStype_of (s:string) =
    if s <> "" then
        Atom.fresh_builtin ("Protocol.ASTStype.Base."^s)
    else
        Atom.fresh_builtin "Protocol.ASTStype.Base"
(*let a_protocol_inner_bridge = Atom.fresh_builtin "Bridge" *)

(* Helper types *)
let a_command = 
    Atom.fresh_builtin "Command"
    
let t_command_of_actor place actor_name = 
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.TAccess (
        auto_place (Ast.TVar actor_name),
        auto_place (Ast.TVar a_command) 
    ))
let t_behavior_of_actor place actor_name = 
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.TParam (
        auto_place (Ast.TVar (Atom.fresh_builtin "Behavior")),
        [ t_command_of_actor place actor_name ]
    ))

(* Helper exprs *)
let e_get_context place = 
    let auto_place smth = {place; value=smth} in
    auto_place ( Ast.CallExpr (
        auto_place ( Ast.VarExpr (Atom.fresh_builtin "getContext")),
        []
    ))
let e_get_self place context = 
    let auto_place smth = {place; value=smth} in
    auto_place ( Ast.AccessExpr (
        context,
        auto_place ( Ast.CallExpr (
            auto_place ( Ast.VarExpr (Atom.fresh_builtin "getSelf")),
            []
        ))
    ))

let e_session_of_protocol place protocol = 
    let auto_place smth = {place; value=smth} in
    auto_place ( Ast.AccessExpr (
        protocol,
        auto_place ( Ast.VarExpr (Atom.fresh_builtin "st"))
    ))