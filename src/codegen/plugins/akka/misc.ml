open Core
open Utils
open AstUtils

(* Helper name *)
let a_ASTStype_of (s:string) =
    if s <> "" then
        Atom.fresh_builtin ("Protocol.ASTSType.Base."^s)
    else
        Atom.fresh_builtin "Protocol.ASTSType.Base"
let a_protocol_inner_bridge = Atom.fresh_builtin "Bridge" 

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