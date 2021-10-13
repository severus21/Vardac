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

let a_cborserializable = 
    Atom.fresh_builtin "CborSerializable"
let a_command = 
    Atom.fresh_builtin "Command"
let a_create_method = 
    Atom.fresh_builtin "create"
let a_context = 
    Atom.fresh_builtin "context"

let a_timers = 
    Atom.fresh_builtin "timers"


let t_cborserializable place= 
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.TVar a_cborserializable) 

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
let t_actor_timer place actor_name_opt = 
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.TParam (
        auto_place (Ast.TVar (Atom.fresh_builtin "TimerScheduler")),
        [ 
            match actor_name_opt with
            | None -> auto_place (Ast.TVar a_command)
            | Some actor_name -> actor_name 
        ]
    ))

let t_lg4dc_abstract_system place =
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.TAccess (
        auto_place (Ast.TVar (Atom.fresh_builtin lg4dc_package)),
        auto_place (Ast.TVar (Atom.fresh_builtin "AbstractSystem")) 
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
let t_akka_member place =
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.TVar (Atom.fresh_builtin "Member")) 

let t_akka_cluster place =
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.TVar (Atom.fresh_builtin "Cluster")) 

let t_lg4dc_nometadata place =
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.TAccess (
        auto_place (Ast.TVar (Atom.fresh_builtin lg4dc_package)),
        auto_place (Ast.TVar (Atom.fresh_builtin "NoMetadata")) 
    ))

let t_lg4dc_vplace place =
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.TAccess (
        auto_place (Ast.TVar (Atom.fresh_builtin lg4dc_package)),
        auto_place (Ast.TVar (Atom.fresh_builtin "VPlace")) 
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
let t_lg4dc_place place =
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.TAccess (
        auto_place (Ast.TVar (Atom.fresh_builtin lg4dc_package)),
        auto_place (Ast.TVar (Atom.fresh_builtin "Place")) 
    ))
let t_lg4dc_vplace place =
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.TAccess (
        auto_place (Ast.TVar (Atom.fresh_builtin lg4dc_package)),
        auto_place (Ast.TVar (Atom.fresh_builtin "VPlace")) 
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
let t_behavior_of_spawnprotocol place = 
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.TParam (
        auto_place (Ast.TVar (Atom.fresh_builtin "Behavior")),
        [ t_command_of_actor place (Atom.fresh_builtin "SpawnProtocol") ]
    ))

let t_receive_of_actor place actor_name = 
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.TParam (
        auto_place (Ast.TVar (Atom.fresh_builtin "Receive")),
        [ t_command_of_actor place actor_name ]
    ))

(* Helper exprs *)


let e_id_of_session place session = 
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.AccessExpr (
        session,
        auto_place (Ast.VarExpr (Atom.fresh_builtin "session_id"))
    ))
let e_headers_of_session place session = 
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.AccessExpr (
        session,
        auto_place (Ast.RawExpr "st.continuations")
    ))


let e_setid_of_session place name =
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.AccessExpr (
        auto_place (Ast.VarExpr name),
        auto_place (Ast.VarExpr (Atom.fresh_builtin "set_id")) 
    ))
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
let e_this_timers place =
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.AccessExpr(
        auto_place Ast.This,
        auto_place (Ast.VarExpr a_timers)
    ))

let e_this_frozen_sessions place =
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.AccessExpr(
        auto_place Ast.This,
        auto_place (Ast.VarExpr (Atom.fresh_builtin "frozen_sessions"))
    ))
let e_this_dead_sessions place =
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.AccessExpr(
        auto_place Ast.This,
        auto_place (Ast.VarExpr (Atom.fresh_builtin "dead_sessions"))
    ))
let e_cast place name e2 =
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.CastExpr(
        auto_place (Ast.TVar (Atom.fresh_builtin name)),
        e2
    ))

let e_this_intermediate_states place =
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.AccessExpr(
        auto_place Ast.This,
        auto_place (Ast.VarExpr (Atom.fresh_builtin "intermediate_states"))
    ))

let e_behaviors_same place =
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.CallExpr ( 
        auto_place (Ast.AccessExpr (
            auto_place (Ast.VarExpr (Atom.fresh_builtin "Behaviors")),
            auto_place (Ast.VarExpr (Atom.fresh_builtin "same"))
        )),
        []
    ))
let e_behaviors_with_timers place =
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.AccessExpr (
        auto_place (Ast.VarExpr (Atom.fresh_builtin "Behaviors")),
        auto_place (Ast.VarExpr (Atom.fresh_builtin "withTimers"))
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
let e_ASTStype_TimerHeader_of place (name:Ast.variable) (value:int) =
    let auto_place smth = {place; value=smth} in
    let n = String.length (Atom.hint name) in
    let kind = 
        auto_place (Ast.AccessExpr( 
            auto_place (Ast.VarExpr( a_ASTStype_of "TimerKind")),
            (* TODO swith to Ocaml 4.13 
            if String.ends_with "lb" (Atom.hint name) then
            *)
            if n>3 && String.sub (Atom.hint name) (n-3) 3 = "_lb" then
                auto_place (Ast.VarExpr (Atom.fresh_builtin "LB"))
            else
                auto_place (Ast.VarExpr (Atom.fresh_builtin "HB"))
        ))
    in
    auto_place (Ast.NewExpr (
        auto_place (Ast.VarExpr (a_ASTStype_of "TimerHeader")),
       [
           kind;
           auto_place (Ast.LitExpr( auto_place (Ast.StringLit (Atom.to_string name))));
           auto_place (Ast.LitExpr (auto_place(Ast.IntLit value)))
        ] 
    ))

let e_bridge_of_protocol place (protocol_e:Ast.expr) =
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.NewExpr (
        auto_place (Ast.VarExpr (Atom.fresh_builtin "Bridge")),
       [ auto_place (Ast.NewExpr (protocol_e,[]))] 
    ))

let e_apply_headers place (session:Ast.expr)=
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.CallExpr(
        auto_place (Ast.AccessExpr (
            auto_place (Ast.VarExpr (a_ASTStype_of "TimerHeader")),
            auto_place (Ast.VarExpr (Atom.fresh_builtin "apply_headers")) 
        )),
        [
            e_get_context place;
            e_this_timers place;
            e_this_frozen_sessions place;
            e_this_dead_sessions place;
            session;
        ]
    ))

let e_is_instance place cl obj = 
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.CallExpr(
        auto_place (Ast.AccessExpr (
            auto_place (Ast.AccessExpr (
                cl,
                auto_place (Ast.VarExpr (Atom.fresh_builtin "class")) 
            )),
            auto_place (Ast.VarExpr (Atom.fresh_builtin "isInstance")) 
        )),
        [ obj ]
    ))
let e_lg4dc_places place =
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.CallExpr(
        auto_place (Ast.AccessExpr (
            auto_place (Ast.VarExpr (Atom.fresh_builtin lg4dc_package)),
            auto_place (Ast.RawExpr ("Place.places")) 
        )),
        [
            e_get_context place
        ]
    ))
let e_lg4dc_current_place place =
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.CallExpr(
        auto_place (Ast.AccessExpr (
            auto_place (Ast.VarExpr (Atom.fresh_builtin lg4dc_package)),
            auto_place (Ast.RawExpr ("Place.currentPlace")) 
        )),
        [
            e_get_context place
        ]
    ))
let e_lg4dc_select_places place vp predicate =
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.CallExpr(
        auto_place (Ast.AccessExpr (
            auto_place (Ast.VarExpr (Atom.fresh_builtin lg4dc_package)),
            auto_place (Ast.RawExpr ("Place.places")) 
        )),
        [
            e_get_context place;
            vp;
            predicate
        ]
    ))