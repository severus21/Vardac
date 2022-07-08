open Core
open Utils
open AstUtils

let system_name = "system"^(String.capitalize_ascii (Config.project_name ())) 
let lg4dc_package = "com.lg4dc"


let fplace = (Error.forge_place ("plg.Akka.Misc") 0 0) 
include Ast.AstUtil2.Make(struct let fplace = fplace end)
let this_actor parent_opt =
    match parent_opt with 
    | Some _, Some _ -> Ast.AccessExpr(e2_e Ast.This, e2var (Atom.builtin "parent_this"))
    | None, _ -> Ast.This
    | _ -> raise (Error.DeadbranchError "if cl is set in parent_opt then component should be set also")

(* Helper name *)
let a_ASTStype_of = function
| "" | "Base" -> Atom.builtin "ASTStype.Base"
| "MsgT" as s-> Atom.builtin ("ASTStype."^s)
| s -> Atom.builtin ("ASTStype."^s)

let a_SType_of = function
| "" | "SType" -> Atom.builtin "SType.SType"
| "STLabel" as s-> Atom.builtin ("SType."^s)
| s -> Atom.builtin ("SType."^s)

        
(*let a_protocol_inner_bridge = Atom.builtin "Bridge" *)

(* Helper types *)

let a_cborserializable = 
    Atom.builtin "CborSerializable"
let a_command = 
    Atom.builtin "Command"
let a_create_method = 
    Atom.builtin "create"
let a_context = 
    Atom.builtin "context"

let a_timers = 
    Atom.builtin "timers"

let a_guardian = 
    Atom.builtin "guardian"
let a_receptionist_adapter = Atom.builtin "receptionist_adapter"

let t_UUID place= 
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.TRaw "UUID") 

let t_continuations place= 
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.TRaw "List<Tuple3<MsgT, List<TimerHeader>, Base>>") 

let t_context place= 
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.TRaw "ActorContext") 
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
        auto_place (Ast.TVar (Atom.builtin "ActorContext")),
        [ 
            match actor_name_opt with
            | None -> auto_place (Ast.TVar a_command)
            | Some actor_name -> actor_name 
        ]
    ))
let t_actor_timer place is_guardian actor_name_opt = 
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.TParam (
        auto_place (Ast.TVar (Atom.builtin "TimerScheduler")),
        [ 
            if is_guardian then 
                auto_place(Ast.Atomic "SpawnProtocol.Command")
            else
            match actor_name_opt with
            | None -> auto_place (Ast.TVar a_command)
            | Some actor_name -> actor_name 
        ]
    ))

let t_actor_guardian place = 
    let auto_place smth = {place; value=smth} in
    auto_place(Ast.ActorRef(
        auto_place(Ast.Atomic "SpawnProtocol.Command")
    ))

let t_lg4dc_abstract_system place =
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.TAccess (
        auto_place (Ast.TVar (Atom.builtin lg4dc_package)),
        auto_place (Ast.TVar (Atom.builtin "AbstractSystem")) 
    ))
let t_lg4dc_abstract_component place =
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.TAccess (
        auto_place (Ast.TVar (Atom.builtin lg4dc_package)),
        auto_place (Ast.TVar (Atom.builtin "AbstractComponent")) 
    ))
let t_lg4dc_protocol place =
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.TAccess (
        auto_place (Ast.TVar (Atom.builtin lg4dc_package)),
        auto_place (Ast.TVar (Atom.builtin "Protocol")) 
    ))

let t_lg4dc_bridge place =
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.TAccess (
        auto_place (Ast.TVar (Atom.builtin lg4dc_package)),
        auto_place (Ast.TVar (Atom.builtin "Bridge")) 
    ))
let t_akka_member place =
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.TVar (Atom.builtin "Member")) 

let t_akka_cluster place =
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.TVar (Atom.builtin "Cluster")) 

let t_lg4dc_nometadata place =
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.TAccess (
        auto_place (Ast.TVar (Atom.builtin lg4dc_package)),
        auto_place (Ast.TVar (Atom.builtin "metadata.NoMetadata")) 
    ))

let t_lg4dc_vplace place =
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.TAccess (
        auto_place (Ast.TVar (Atom.builtin lg4dc_package)),
        auto_place (Ast.TVar (Atom.builtin "VPlace")) 
    ))
let t_lg4dc_event place metadata_opt =
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.TParam(
        auto_place (Ast.TAccess (
            auto_place (Ast.TVar (Atom.builtin lg4dc_package)),
            auto_place (Ast.TVar (Atom.builtin "Event")) 
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
        auto_place (Ast.TVar (Atom.builtin lg4dc_package)),
        auto_place (Ast.TVar (Atom.builtin "Session")) 
    ))
let t_lg4dc_place place =
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.TAccess (
        auto_place (Ast.TVar (Atom.builtin lg4dc_package)),
        auto_place (Ast.TVar (Atom.builtin "Place")) 
    ))
let t_lg4dc_vplace place =
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.TAccess (
        auto_place (Ast.TVar (Atom.builtin lg4dc_package)),
        auto_place (Ast.TVar (Atom.builtin "VPlace")) 
    ))

let t_command_of place ct = 
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.TAccess (
        ct,
        auto_place (Ast.TVar a_command) 
    ))
let t_command_of_actor place actor_name = 
    let auto_place smth = {place; value=smth} in
    t_command_of place (auto_place (Ast.TVar actor_name))
let t_behavior_of_actor place actor_name = 
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.TParam (
        auto_place (Ast.TVar (Atom.builtin "Behavior")),
        [ t_command_of_actor place actor_name ]
    ))
let t_behavior_of_spawnprotocol place = 
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.TParam (
        auto_place (Ast.TVar (Atom.builtin "Behavior")),
        [ t_command_of_actor place (Atom.builtin "SpawnProtocol") ]
    ))

let t_receive_of_actor place actor_name = 
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.TParam (
        auto_place (Ast.TVar (Atom.builtin "Receive")),
        [ t_command_of_actor place actor_name ]
    ))

(* Helper exprs *)

let e_none place = 
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.CallExpr (
        auto_place (Ast.VarExpr (Atom.builtin "Optional.empty"), auto_place Ast.TUnknown),
        []
    ), auto_place Ast.TUnknown)  
let e_some place e = 
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.CallExpr (
        auto_place (Ast.VarExpr (Atom.builtin "Optional.of"), auto_place Ast.TUnknown),
        [ e ]
    ), auto_place Ast.TUnknown)  

let e_id_of_session place session = 
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.AccessExpr (
        session,
        auto_place (Ast.VarExpr (Atom.builtin "session_id"), t_UUID place)
    ))
let e_headers_of_session place session = 
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.AccessExpr (
        session,
        auto_place (Ast.RawExpr "st.continuations", t_continuations place)
    ))


let e_setid_of_session place name =
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.AccessExpr (
        auto_place (Ast.VarExpr name, auto_place Ast.TUnknown),
        auto_place (Ast.VarExpr (Atom.builtin "set_id"), auto_place Ast.TUnknown) 
    ), auto_place Ast.TUnknown)
let e_get_context place = 
    let auto_place smth = {place; value=smth} in
    auto_place ( Ast.CallExpr (
        auto_place ( Ast.VarExpr (Atom.builtin "getContext"), auto_place Ast.TUnknown),
        []
    ), t_context place)
let e_get_self_actor place context : Ast.expr= 
    let auto_place smth = {place; value=smth} in
    auto_place ( Ast.AccessExpr (
        context,
        auto_place ( Ast.CallExpr (
            auto_place ( Ast.VarExpr (Atom.builtin "getSelf"), auto_place Ast.TUnknown),
            []
        ), auto_place Ast.TUnknown)
    ), auto_place Ast.TUnknown)

let e_get_self_activation place context : Ast.expr = 
    let auto_place smth = {place; value=smth} in

    auto_place (Ast.ActivationRef{
        schema = auto_place (Ast.AccessExpr (
            auto_place (Ast.This, auto_place Ast.TUnknown), 
            auto_place ((Ast.VarExpr (Atom.builtin "schema"), auto_place Ast.TUnknown))
        ), auto_place Ast.TUnknown);
        actor_ref = e_get_self_actor place context;
    }, auto_place Ast.TUnknown)


let e_outport_of place bridge = 
    let auto_place smth = {place; value=smth} in
    auto_place ( Ast.NewExpr (
        auto_place ( Ast.VarExpr (Atom.builtin "OutPort"), auto_place Ast.TUnknown),
        [
            bridge
        ]
    ), auto_place Ast.TUnknown)
let e_this_timers this place =
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.AccessExpr(
        auto_place (this, auto_place Ast.TUnknown),
        auto_place (Ast.VarExpr a_timers, auto_place Ast.TUnknown)
    ), auto_place Ast.TUnknown)

let e_this_guardian this place =
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.AccessExpr(
        auto_place (this, auto_place Ast.TUnknown),
        auto_place (Ast.VarExpr a_guardian, auto_place Ast.TUnknown)
    ), auto_place Ast.TUnknown)
    
let e_this_receptionist_adapter this place =
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.AccessExpr(
        auto_place (this, auto_place Ast.TUnknown),
        auto_place (Ast.VarExpr a_receptionist_adapter, auto_place Ast.TUnknown)
    ), auto_place Ast.TUnknown)

let e_this_frozen_sessions this place =
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.AccessExpr(
        auto_place (this, auto_place Ast.TUnknown),
        auto_place (Ast.VarExpr (Atom.builtin "frozen_sessions"), auto_place Ast.TUnknown)
    ), auto_place Ast.TUnknown)
let e_this_dead_sessions this place =
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.AccessExpr(
        auto_place (this, auto_place Ast.TUnknown),
        auto_place (Ast.VarExpr (Atom.builtin "dead_sessions"), auto_place Ast.TUnknown)
    ), auto_place Ast.TUnknown)
let e_cast place name e2 =
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.CastExpr(
        auto_place (Ast.TVar (Atom.builtin name)),
        e2
    ), auto_place Ast.TUnknown)

let e_this_intermediate_states this place =
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.AccessExpr(
        auto_place (this, auto_place Ast.TUnknown),
        auto_place (Ast.VarExpr (Atom.builtin "intermediate_states"), auto_place Ast.TUnknown)
    ), auto_place Ast.TUnknown)

let e_behaviors_same place =
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.CallExpr ( 
        auto_place (Ast.AccessExpr (
            auto_place (Ast.VarExpr (Atom.builtin "Behaviors"), auto_place Ast.TUnknown),
            auto_place (Ast.VarExpr (Atom.builtin "same"), auto_place Ast.TUnknown)
        ), auto_place Ast.TUnknown),
        []
    ), auto_place Ast.TUnknown)
let e_behaviors_with_timers place =
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.AccessExpr (
        auto_place (Ast.VarExpr (Atom.builtin "Behaviors"), auto_place Ast.TUnknown),
        auto_place (Ast.VarExpr (Atom.builtin "withTimers"), auto_place Ast.TUnknown)
    ), auto_place Ast.TUnknown)


let e_lg4dc_session place =
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.AccessExpr (
        auto_place (Ast.VarExpr (Atom.builtin lg4dc_package), auto_place Ast.TUnknown),
        auto_place (Ast.VarExpr (Atom.builtin "Session"), auto_place Ast.TUnknown) 
    ), auto_place Ast.TUnknown)

let e_lg4dc_spawnat place =
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.AccessExpr (
        auto_place (Ast.VarExpr (Atom.builtin lg4dc_package), auto_place Ast.TUnknown),
        auto_place (Ast.RawExpr "PlaceDiscovery.spawnAt", auto_place Ast.TUnknown) 
    ), auto_place Ast.TUnknown)
let e_lg4dc_componentsat place =
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.AccessExpr (
        auto_place (Ast.VarExpr (Atom.builtin lg4dc_package), auto_place Ast.TUnknown),
        auto_place (Ast.RawExpr "PlaceDiscovery.componentsAt", auto_place Ast.TUnknown) 
    ), auto_place Ast.TUnknown)
let e_lg4dc_placeof place =
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.AccessExpr (
        auto_place (Ast.VarExpr (Atom.builtin lg4dc_package), auto_place Ast.TUnknown),
        auto_place (Ast.RawExpr "Place.of_actor_ref", auto_place Ast.TUnknown)
    ), auto_place Ast.TUnknown)
let e_session_of_protocol place protocol = 
    let auto_place smth = {place; value=smth} in
    auto_place ( Ast.CallExpr (
        auto_place ( Ast.AccessExpr (
            auto_place (Ast.NewExpr (protocol, []), auto_place Ast.TUnknown),
            auto_place ( Ast.VarExpr (Atom.builtin "get_st"), auto_place Ast.TUnknown)
        ), auto_place Ast.TUnknown),
        []
    ), auto_place Ast.TUnknown)

let e_setup_behaviors place args =
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.CallExpr (
        auto_place (Ast.AccessExpr (
            auto_place (Ast.VarExpr (Atom.builtin "Behaviors"), auto_place Ast.TUnknown),
            auto_place (Ast.VarExpr (Atom.builtin "setup"), auto_place Ast.TUnknown)
        ), auto_place Ast.TUnknown),
        args
    ), auto_place Ast.TUnknown)
let e_logger_of_context place context = 
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.CallExpr (
        auto_place (Ast.AccessExpr (
            context,
            auto_place (Ast.VarExpr (Atom.builtin "getLog"), auto_place Ast.TUnknown)
        ), auto_place Ast.TUnknown),
        []
    ), auto_place Ast.TUnknown)
let e_debug_of place (context:Ast.expr) (args:Ast.expr list) : Ast.expr =
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.CallExpr (
        auto_place (Ast.AccessExpr (
            e_logger_of_context place context,
            auto_place (Ast.VarExpr (Atom.builtin "debug"), auto_place Ast.TUnknown)
        ), auto_place Ast.TUnknown),
        args
    ), auto_place Ast.TUnknown)
let e_error_of place (context:Ast.expr) (args:Ast.expr list) : Ast.expr =
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.CallExpr (
        auto_place (Ast.AccessExpr (
            e_logger_of_context place context,
            auto_place (Ast.VarExpr (Atom.builtin "error"), auto_place Ast.TUnknown)
        ), auto_place Ast.TUnknown),
        [List.fold_left (fun a b -> auto_place (Ast.BinopExpr (a, Ast.Plus, b), auto_place Ast.TUnknown)) (auto_place (Ast.RawExpr "\"\"", auto_place Ast.TUnknown)) args]
    ), auto_place Ast.TUnknown)

let e_super place args = 
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.CallExpr (
        auto_place (Ast.VarExpr (Atom.builtin "super"), auto_place Ast.TUnknown),
        args
    ), auto_place Ast.TUnknown)

let e_ASTStype_MsgT_of place (e:Ast.expr) =
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.NewExpr (
        auto_place (Ast.VarExpr (a_ASTStype_of "MsgT"), auto_place Ast.TUnknown),
       [e] 
    ), auto_place Ast.TUnknown)
let e_ASTStype_TimerHeader_of place (name:Ast.variable) (value:int) =
    let auto_place smth = {place; value=smth} in
    let n = String.length (Atom.hint name) in
    let kind = 
        auto_place (Ast.AccessExpr( 
            auto_place (Ast.VarExpr( a_ASTStype_of "TimerKind"), auto_place Ast.TUnknown),
            (* TODO swith to Ocaml 4.13 
            if String.ends_with "lb" (Atom.hint name) then
            *)
            if n>3 && String.sub (Atom.hint name) (n-3) 3 = "_lb" then
                auto_place (Ast.VarExpr (Atom.builtin "LB"), auto_place Ast.TUnknown)
            else
                auto_place (Ast.VarExpr (Atom.builtin "HB"), auto_place Ast.TUnknown)
        ), auto_place Ast.TUnknown)
    in
    auto_place (Ast.NewExpr (
        auto_place (Ast.VarExpr (a_ASTStype_of "TimerHeader"), auto_place Ast.TUnknown),
       [
           kind;
           auto_place (Ast.LitExpr( auto_place (Ast.StringLit (Atom.to_string name))), auto_place Ast.TUnknown);
           auto_place (Ast.LitExpr (auto_place(Ast.IntLit value)), auto_place Ast.TUnknown)
        ] 
    ), auto_place Ast.TUnknown)

let e_bridge_of_protocol place (protocol_e:Ast.expr) =
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.NewExpr (
        auto_place (Ast.VarExpr (Atom.builtin "Bridge"), auto_place Ast.TUnknown),
       [ auto_place (Ast.NewExpr (protocol_e,[]), auto_place Ast.TUnknown)] 
    ), auto_place Ast.TUnknown)
let e_static_bridge_of_protocol place (protocol_e:Ast.expr) (id: Atom.atom)=
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.NewExpr (
        auto_place (Ast.VarExpr (Atom.builtin "Bridge"), auto_place Ast.TUnknown),
       [ 
           auto_place (Ast.NewExpr (protocol_e,[]), auto_place Ast.TUnknown);
           auto_place (Ast.LitExpr (auto_place(Ast.StringLit (Atom.to_string id))), auto_place (Ast.Atomic "String"))
        ] 
    ), auto_place Ast.TUnknown)

let e_apply_headers this place (session:Ast.expr)=
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.CallExpr(
        auto_place (Ast.AccessExpr (
            auto_place (Ast.VarExpr (a_ASTStype_of "TimerHeader"), auto_place Ast.TUnknown),
            auto_place (Ast.VarExpr (Atom.builtin "apply_headers"), auto_place Ast.TUnknown) 
        ), auto_place Ast.TUnknown),
        [
            e_get_context place;
            e_this_timers this place;
            e_this_frozen_sessions this place;
            e_this_dead_sessions this place;
            session;
        ]
    ), auto_place Ast.TUnknown)

let e_is_instance place cl obj = 
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.CallExpr(
        auto_place (Ast.AccessExpr (
            auto_place (Ast.AccessExpr (
                cl,
                auto_place (Ast.VarExpr (Atom.builtin "class"), auto_place Ast.TUnknown) 
            ), auto_place Ast.TUnknown),
            auto_place (Ast.VarExpr (Atom.builtin "isInstance"), auto_place Ast.TUnknown) 
        ), auto_place Ast.TUnknown),
        [ obj ]
    ), auto_place Ast.TUnknown)
let e_lg4dc_places place =
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.CallExpr(
        auto_place (Ast.AccessExpr (
            auto_place (Ast.VarExpr (Atom.builtin lg4dc_package), auto_place Ast.TUnknown),
            auto_place (Ast.RawExpr ("Place.places"), auto_place Ast.TUnknown) 
        ), auto_place Ast.TUnknown),
        [
            e_get_context place
        ]
    ), auto_place Ast.TUnknown)
let e_lg4dc_current_place place =
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.CallExpr(
        auto_place (Ast.AccessExpr (
            auto_place (Ast.VarExpr (Atom.builtin lg4dc_package), auto_place Ast.TUnknown),
            auto_place (Ast.RawExpr ("Place.currentPlace"), auto_place Ast.TUnknown) 
        ), auto_place Ast.TUnknown),
        [
            e_get_context place
        ]
    ), auto_place Ast.TUnknown)

let e_lg4dc_select_places place vp predicate =
    let auto_place smth = {place; value=smth} in
    auto_place (Ast.CallExpr(
        auto_place (Ast.AccessExpr (
            auto_place (Ast.VarExpr (Atom.builtin lg4dc_package), auto_place Ast.TUnknown),
            auto_place (Ast.RawExpr ("Place.places"), auto_place Ast.TUnknown) 
        ), auto_place Ast.TUnknown),
        [
            e_get_context place;
            vp;
            predicate
        ]
    ), auto_place Ast.TUnknown)