open Core
open Utils
open AstUtils

let lg4dc_package = "com.lg4dc"

(* Helper name *)
let a_ASTStype_of = function
| "" | "Base" -> Atom.fresh_builtin "ASTStype.Base"
| "MsgT" as s-> Atom.fresh_builtin ("ASTStype."^s)
| s -> Atom.fresh_builtin ("ASTStype."^s)

let a_SType_of = function
| "" | "SType" -> Atom.fresh_builtin "SType.SType"
| "STLabel" as s-> Atom.fresh_builtin ("SType."^s)
| s -> Atom.fresh_builtin ("SType."^s)

        
(*let a_protocol_inner_bridge = Atom.fresh_builtin "Bridge" *)

(* Helper types *)
let a_command = 
    Atom.fresh_builtin "Command"
let a_create_method = 
    Atom.fresh_builtin "create"
let a_context = 
    Atom.fresh_builtin "context"
    

let t_SType_of place name tparam1 tparam2 = 
    let auto_place smth = {place; value=smth} in
    Ast.TParam (
        auto_place (Ast.TVar (a_SType_of name)),
        [tparam1; tparam2]
    )
let t_actor_context place actor_name_opt = 
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.TParam (
        auto_place (Ast.TVar (Atom.fresh_builtin "ActorContext")),
        [ 
            match actor_name_opt with
            | None -> auto_place (Ast.TVar a_command)
            | Some actor_name -> actor_name 
        ]
    ))

let t_lg4dc_protocol place =
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.TAccess (
        auto_place (Ast.TVar (Atom.fresh_builtin lg4dc_package)),
        auto_place (Ast.TVar (Atom.fresh_builtin "Protocol")) 
    ))

let t_lg4dc_bridge place =
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.TAccess (
        auto_place (Ast.TVar (Atom.fresh_builtin lg4dc_package)),
        auto_place (Ast.TVar (Atom.fresh_builtin "Bridge")) 
    ))

let t_lg4dc_nometadata place =
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.TAccess (
        auto_place (Ast.TVar (Atom.fresh_builtin lg4dc_package)),
        auto_place (Ast.TVar (Atom.fresh_builtin "NoMetadata")) 
    ))

let t_lg4dc_event place metadata_opt =
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.TParam(
        auto_place (Ast.TAccess (
            auto_place (Ast.TVar (Atom.fresh_builtin lg4dc_package)),
            auto_place (Ast.TVar (Atom.fresh_builtin "Event")) 
        )),
        [
            match metadata_opt with 
            | None -> t_lg4dc_nometadata place
            | Some ct -> ct
        ]
    ))
let t_lg4dc_session place =
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.TAccess (
        auto_place (Ast.TVar (Atom.fresh_builtin lg4dc_package)),
        auto_place (Ast.TVar (Atom.fresh_builtin "Session")) 
    ))
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

let t_receive_of_actor place actor_name = 
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.TParam (
        auto_place (Ast.TVar (Atom.fresh_builtin "Receive")),
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

let e_lg4dc_session place =
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.AccessExpr (
        auto_place (Ast.VarExpr (Atom.fresh_builtin lg4dc_package)),
        auto_place (Ast.VarExpr (Atom.fresh_builtin "Session")) 
    ))
let e_session_of_protocol place protocol = 
    let auto_place smth = {place; value=smth} in
    auto_place ( Ast.CallExpr (
        auto_place ( Ast.AccessExpr (
            auto_place (Ast.NewExpr (protocol, [])),
            auto_place ( Ast.VarExpr (Atom.fresh_builtin "get_st"))
        )),
        []
    ))

let e_setup_behaviors place args =
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.CallExpr (
        auto_place (Ast.AccessExpr (
            auto_place (Ast.VarExpr (Atom.fresh_builtin "Behaviors")),
            auto_place (Ast.VarExpr (Atom.fresh_builtin "setup"))
        )),
        args
    ))
let e_logger_of_context place context = 
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.CallExpr (
        auto_place (Ast.AccessExpr (
            context,
            auto_place (Ast.VarExpr (Atom.fresh_builtin "getLog"))
        )),
        []
    ))
let e_debug_of place (context:Ast.expr) (args:Ast.expr list) : Ast.expr =
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.CallExpr (
        auto_place (Ast.AccessExpr (
            e_logger_of_context place context,
            auto_place (Ast.VarExpr (Atom.fresh_builtin "debug"))
        )),
        args
    ))

let e_super place args = 
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.CallExpr (
        auto_place (Ast.VarExpr (Atom.fresh_builtin "super")),
        args
    ))

let e_ASTStype_MsgT_of place (e:Ast.expr) =
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.NewExpr (
        auto_place (Ast.VarExpr (a_ASTStype_of "MsgT")),
       [e] 
    ))

let e_bridge_of_protocol place (protocol_e:Ast.expr) =
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.NewExpr (
        auto_place (Ast.VarExpr (Atom.fresh_builtin "Bridge")),
       [ auto_place (Ast.NewExpr (protocol_e,[]))] 
    ))