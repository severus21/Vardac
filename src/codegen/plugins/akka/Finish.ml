open Core
open Utils
open AstUtils
open Easy_logging
open Fieldslib
open Misc

(* Function composition, TODO put it in some Core file*)
let plg_name = "Akka"
let logger = Logging.make_logger ("_1_ compspec.plg."^plg_name) Debug [];;

(* The source calculus. *)
module S = IRI 
(* The target calculus. *)
module T = Ast 



(*** Global state *)
type collected_state = {
    event2receptionists : (Atom.t, Atom.t list) Hashtbl.t; 
    collected_components: Atom.Set.t ref;
    guardian_components: Atom.Set.t ref
}
 
let print_cstate cstate = 
    Format.fprintf Format.std_formatter "Cstate.event2receptionists\n";
    Hashtbl.iter (fun k v -> 
        Format.fprintf 
            Format.std_formatter "+ %s -> @[<hv>%a@]\n" 
            (Atom.to_string k) 
            (Error.pp_list ";" (fun out x-> Format.fprintf out "%s" (Atom.to_string x))) 
            v
    ) cstate.event2receptionists

let empty_cstate () : collected_state = {
    event2receptionists = Hashtbl.create 0;
    collected_components = ref Atom.Set.empty;
    guardian_components = ref Atom.Set.empty
}

module Make () = struct

    (* Use to remove type alias introduced by *.impl. Varda type aliasing have been compiled away during UntypedCleansing pass *)
    let typealias = Hashtbl.create 32

    let to_capitalize_variables = Hashtbl.create 64
    let make_capitalize_renaming = function x ->
        match Hashtbl.find_opt to_capitalize_variables x with 
        | None -> x
        | Some _ -> Atom.refresh_hint x (String.capitalize_ascii (Atom.hint x))
    let collected_components = ref Atom.Set.empty
    (*
        event -> list of components that can receive it 
    *)
    let event2receptionists : (Atom.t, Atom.t list) Hashtbl.t= Hashtbl.create 64
    let add_event_e2rs event component : unit = 
        let vs = 
            try
                Hashtbl.find event2receptionists event
            with Not_found -> []
        in
        Hashtbl.add event2receptionists event (component::vs)

    let rename_collected_state renaming = 
        let e2rs = Hashtbl.to_seq event2receptionists in
        let e2rs = List.of_seq (Seq.map (function (k,v) -> renaming k, List.map renaming v) e2rs) in

        (* Since we change the keys *)
        Hashtbl.reset event2receptionists; 
        Hashtbl.add_seq event2receptionists (List.to_seq e2rs)
    (*****)

    (* The translation of a complete program. *)

    let fst3 (x,y,z) = x


    (* Environments map strings to atoms. *)
    module AtomEnv = Atom.VMap 
    module LabelsEnv = Atom.AtomsMap 

    type finish_env = {
        labels: T.variable LabelsEnv.t; 
        events: T.event AtomEnv.t; 
    } [@@deriving fields] 

    let fresh_fenv () = {
        labels      = LabelsEnv.empty;
        events      = AtomEnv.empty;
    }

    let bind_event (env:finish_env) key value =
        { env with events = AtomEnv.add key value env.events }

    (* FIXME: do we need to make fenv depend of the scope, if yes follow the cook architecture *)
    let general_fenv = ref (fresh_fenv())

    (* XXXX *)

    type items_grps = { 
        methods: S.method0 list; 
        eventdefs: S.typedef list;
        states: S.state list; 
        nested: S.component_dcl list; 
        ports: S.port list;
        outports: S.outport list;
        others: S.term list
    }

    let fresh_items_grp () = { 
        methods     = [];
        eventdefs    = [];
        states      = [];
        nested      = [];
        ports       = [];
        outports    = [];
        others      = [];
    }


    let group_cdcl_by (citems:  S.component_item list) : items_grps =
        let dispatch grp (citem: S.component_item) = match citem.value with
            | S.Contract _ -> raise (Core.Error.PlacedDeadbranchError  (citem.place, "Contract term should have been remove from AST by the cook pass and binded to a method"))
            | S.Include _ -> Core.Error.error citem.place "Include is not yet supported in Akka plg"
            | S.Method  m-> {grp with methods=m::grp.methods}
            | S.State f-> {grp with states=f::grp.states}
            | S.Inport p -> {grp with ports=p::grp.ports}
            | S.Outport p -> {grp with outports=p::grp.outports}
            (* Shallow search of Typealias, FIXME do we need deep search ?*)
            | S.Term {place; value=S.Component cdcl} -> {grp with nested=cdcl::grp.nested}
            | S.Term {place; value=S.Typedef ({value=EventDef _;_} as edef)} -> {grp with eventdefs=edef::grp.eventdefs}
            | S.Term t -> {grp with others=t::grp.others}
        in

        let grp = (List.fold_left  dispatch (fresh_items_grp ()) citems) in 
            {
                methods=(List.rev grp.methods);
                eventdefs=(List.rev grp.eventdefs);
                states=(List.rev grp.states) ; 
                nested=(List.rev grp.nested) ; 
                ports=(List.rev grp.ports) ; 
                outports=(List.rev grp.outports) ; 
                others=(List.rev grp.others)
            }

    (************************************* Base types ****************************)

    (************************************ Types **********************************)
    let rec finish_ctype place : S._composed_type ->  T._ctype = function
        | S.TActivationRef mt -> T.TActivationRef (fmtype mt) 
        | S.TArrow (m1, m2) -> T.TFunction (
            fmtype m1,
            fmtype m2
        )

        | S.TVar x ->  begin
            (* Remove type alias introduced by *.impl if any *)
            match Hashtbl.find_opt typealias x with
            | None -> T.TVar x
            | Some bb -> T.TBB (fbbterm bb) 
        end
        | S.TFlatType ft -> begin match ft with  
            (* When using Tuyple, Map, ... we need object so for ease we box atomic type in objects everywhere *)
            | AstUtils.TActivationID -> T.Atomic "UUID"
            | AstUtils.TBool -> T.Atomic "Boolean"
            | AstUtils.TInt -> T.Atomic "Integer"
            | AstUtils.TFloat -> T.Atomic "Float"
            | AstUtils.TSessionID -> T.Atomic "UUID"
            | AstUtils.TStr -> T.Atomic "String"
            | AstUtils.TVoid -> T.Atomic "Void" 
            | AstUtils.TUUID -> T.Atomic "UUID" 
            | AstUtils.TWildcard -> T.Atomic "Object"
            | AstUtils.TPlace -> (t_lg4dc_place place).value
            | AstUtils.TBLabel -> T.Atomic "LabelEvent"
        end
        | S.TArray mt -> T.TArray (fmtype mt)
        | S.TDict (m1, m2) -> T.TMap (fmtype m1, fmtype m2)
        | S.TList mt -> T.TList (fmtype mt)
        | S.TOption mt -> T.TOption (fmtype mt)
        | S.TResult (m1, m2) -> T.TResult (fmtype m1, fmtype m2)
        | S.TSet mt -> T.TSet (fmtype mt)
        | S.TTuple mts ->  T.TTuple (List.map (fun x -> (fmtype x)) mts)
        | S.TVPlace mt -> (t_lg4dc_vplace place).value
        | S.TBridge b -> (t_lg4dc_bridge place).value
        | S.TUnion _-> T.TRaw "Object" (* TODO maybe a better solution*)
        | S.TForall _ -> T.TUnknown (* TODO maybe encode it as class <T> ... { <T> } *)
        | S.TPolyVar _ -> T.TUnknown (* TODO maybe encode it as class <T> ... { <T> } *)
        | S.TOutport _ -> T.TUnknown (* as long as port are static we do not care of this inside Akka plg*) 
        | S.TInport _ -> T.TUnknown (* as long as port are static we do not care of this inside Akka plg*) 
    and fctype ct :  T.ctype = map_place finish_ctype ct

    (* Represent an ST object in Java type *)
    and finish_stype place : S._session_type -> T._ctype = function 
        | S.STEnd -> T.TVar (Atom.builtin "Protocol.STEnd") 
        | (S.STSend (mt, st) as st0) | (S.STRecv (mt, st) as st0) -> begin 
            match fmtype mt with
            | ct ->
                T.TParam (
                    {
                        place;
                        value =  T.TVar (Atom.builtin (match st0 with | S.STSend _ -> "Protocol.STSend" | STRecv _ -> "Protocol.STRecv"))
                    },
                    [ct; fstype st]
                )
            | _ -> raise (Core.Error.PlacedDeadbranchError (mt.place, "finish_stype : STSend/STRecv type should not be a session type."))
        end
        | (S.STBranch xs as st0) | (S.STSelect xs as st0) ->
            let rec built_t_hlist = function
                | [] -> {place; value = T.TVar (Atom.builtin "Protocol.HNil")}
                | st::sts -> { place; value = T.TParam( 
                    { 
                        place = st.place; 
                        value = T.TVar (Atom.builtin "Protocol.HCons")
                    }, [
                        {place = place; value = T.TParam( 
                            { 
                                place = st.place;
                                value = T.TVar (Atom.builtin "Protocol.STEntry")
                            },
                            [fstype st]
                        )}
                    ] @ [(built_t_hlist sts)] 
                )}
            in

            let continuation_st = built_t_hlist (List.map (fun (_, st, _) -> st) xs) in

            T.TParam (
                { 
                    place;
                    value = T.TVar (Atom.builtin (match st0 with | S.STBranch _ -> "Protocol.STBranch" | S.STSelect _ -> "Protocol.STSelect"))
                },
                [continuation_st]
            )
        | S.STVar _ -> T.TVar (Atom.builtin ("Protocol.STVar"))
        | S.STRec (_,st) ->
            T.TParam (
                { 
                    place;
                    value = T.TVar (Atom.builtin "Protocol.STRec")
                },
                [ fstype st]
            )

        | S.STInline x -> 
            raise (Error.PlacedDeadbranchError (place, "STInline should remains outside the codegen part, it should have been resolve during the partial evaluation pass."))
    and fstype st : T.ctype = map_place finish_stype st

    (* Represent an ST object in Java value *)
    and finishv_stype place : S._session_type -> T._expr * T.ctype = 
    (****** Helpers *****)
    let fplace = place@(Error.forge_place "Plg=Akka/finishv_stype" 0 0) in
    let auto_place smth = {place = fplace; value=smth} in

    let rec encode_guard_header_ place = function
    | S.UseMetadata _ -> failwith "UseMetadata/Global not yet supported by Akka"
    | S.SetTimer _ -> raise (Error.PlacedDeadbranchError (place, "SetTimer should have been replace by SetFireTimer before the Akka.Finish since Akka needs a delay value to create a timer"))
    | S.SetFireTimer (x, i) -> e_ASTStype_TimerHeader_of place x i 
    and encode_guard_header header : T.expr = encode_guard_header_ header.place header.value in 

    let encodectype = function
        | {value=T.TVar x; place} -> {value=T.VarExpr x, auto_place T.TUnknown; place} 
        | {value=T.Atomic x; place} -> {value=T.VarExpr (Atom.builtin x), auto_place T.TUnknown; place} 
        | {value=T.TTuple xs} -> {value=T.VarExpr (Atom.builtin (Printf.sprintf "io.vavr.Tuple%d" (List.length xs))), auto_place T.TUnknown; place}
        | ct -> failwith "TODO encodectype_Finish"
    in

    function 
        | S.STEnd -> T.NewExpr (auto_place (T.VarExpr (a_ASTStype_of "End"), auto_place T.TUnknown), []), auto_place T.TUnknown
        (* With guard *)
        | (S.STSend ({place=p_mt; value=S.ConstrainedType (mt, (guard_headers, guard_opt))}, st) as st0) | (S.STRecv ({place=p_mt; value=S.ConstrainedType (mt, (guard_headers, guard_opt))}, st) as st0) -> begin 

            let encoded_headers = List.map encode_guard_header guard_headers in
            (* TODO remove timer binop from guard_opt -> fully manage by through header in Akka *)

            match fmtype mt with
            | ct ->
                let constructor = auto_place ((match st0 with 
                | S.STSend _ -> T.VarExpr (a_ASTStype_of "Send")
                | STRecv _ -> T.VarExpr (a_ASTStype_of "Receive")
                ), auto_place T.TUnknown) in

                T.NewExpr (
                    constructor,
                    [
                        e_ASTStype_MsgT_of place (
                            auto_place(
                                T.CallExpr(
                                    auto_place (T.AccessExpr(
                                        auto_place (T.AccessExpr (encodectype ct, auto_place (T.VarExpr (Atom.builtin "class"), auto_place T.TUnknown)), auto_place T.TUnknown),
                                        auto_place (T.RawExpr "toString", auto_place T.TUnknown)
                                    ), auto_place T.TUnknown),
                                    []
                                ), auto_place T.TUnknown
                            )
                        );
                        auto_place (Encode.encode_list place encoded_headers, auto_place T.TUnknown);  
                        fvstype st
                    ]
                ), auto_place T.TUnknown 
            | _ -> raise (Core.Error.PlacedDeadbranchError (mt.place, "finish_stype : STSend/STRecv type should not be a session type."))
        end

        (* Without guard *)
        | (S.STSend (mt, st) as st0) | (S.STRecv (mt, st) as st0) -> begin 
            match fmtype mt with
            | ct ->
                let constructor = auto_place ((match st0 with 
                | S.STSend _ -> T.VarExpr (a_ASTStype_of "Send")
                | STRecv _ -> T.VarExpr (a_ASTStype_of "Receive")
                ), auto_place T.TUnknown) in

                T.NewExpr (
                    constructor,
                    [
                        e_ASTStype_MsgT_of place (
                            auto_place(
                                T.CallExpr(
                                    auto_place (T.AccessExpr(
                                        auto_place (T.AccessExpr (encodectype ct, auto_place (T.VarExpr (Atom.builtin "class"), auto_place T.TUnknown)), auto_place T.TUnknown),
                                        auto_place (T.RawExpr "toString", auto_place T.TUnknown)
                                    ), auto_place T.TUnknown),
                                    []
                                ), auto_place T.TUnknown
                            )
                        );
                        auto_place (Encode.encode_list place [], auto_place T.TUnknown);  
                        fvstype st
                    ]
                ), auto_place T.TUnknown
            | _ -> raise (Core.Error.PlacedDeadbranchError (mt.place, "finish_stype : STSend/STRecv type should not be a session type."))
        end
        | (S.STBranch xs as st0) | (S.STSelect xs as st0) ->
            let constructor = auto_place ((match st0 with 
            | S.STBranch _ -> T.VarExpr (a_ASTStype_of "Branch")
            | STSelect _ -> T.VarExpr (a_ASTStype_of "Select")
            ), auto_place T.TUnknown
            ) in

            T.NewExpr (
                constructor,
                [
                    auto_place (T.BlockExpr (
                        AstUtils.List,
                        List.map (function (label, st, _) -> auto_place (T.BlockExpr (
                            AstUtils.Tuple, 
                            [ 
                                e_ASTStype_MsgT_of place (
                                    auto_place (T.LitExpr (auto_place (T.StringLit (Atom.to_string label))), auto_place T.TUnknown) 
                                );
                                auto_place (Encode.encode_list place [], auto_place T.TUnknown);  
                                fvstype st 
                            ]
                        ), auto_place T.TUnknown)) xs
                    ), auto_place T.TUnknown)
                ]
            ), auto_place T.TUnknown
        | S.STVar _ -> failwith "Not yet supported" 
        | S.STRec (_,st) -> failwith "Not yet supported"
        | S.STInline x -> 
            raise (Error.PlacedDeadbranchError (place, "STInline should remains outside the codegen part, it should have been resolve during the partial evaluation pass."))
        | S.STWildcard -> 
            (* FIXME TODO WARNING dirty hack since we do not have working mgu to unify receive bridge type with concrete type
            then temporaly we return STEnd encoding *)
            T.NewExpr (auto_place (T.VarExpr (a_ASTStype_of "End"), auto_place T.TUnknown), []), auto_place T.TUnknown
            (* The correct behaviour is *)
            (* raise (Error.PlacedDeadbranchError (place, "STWildcard should have been concretized during type inference.")) *)
    and fvstype st : T.expr = map_place finishv_stype st

    and finish_component_type place : S._component_type -> T._ctype = function
    | S.CompTUid x -> T.TVar x 
    | S.TStruct (x, _) -> 
        (* Structural types can not be encoded easily in Java *)
        T.TVar x
    | S.TPolyCVar x -> Error.error place "TPolyCVar should have been reduce before reaching Akka ???"
    and fcctype ct : T.ctype = map_place finish_component_type ct

    and finish_mtype place : S._main_type -> T.ctype = 
    let fplace = place@(Error.forge_place "Plg=Akka/finish_mtype" 0 0) in
    function
    | S.CType ct -> fctype ct 
    (* TODO FIXME URGENT
            Type de session dans Akka 
            actuellement encapsuler dans le protocol pb pour la creation après
            puisque dans le language on utilise le protocol que pour le initiate_sesion et les autres types de sessions sont indépendants du protocol.
    *)
    | S.SType {value=S.STInline _} -> t_lg4dc_session place (* FIXME x not used *)
    | S.SType st -> {
        place; 
        value = T.TParam (
            t_lg4dc_session fplace,
            [ ] (* FIXME at this point we do not parametrize session with session types since 
                    do not compile yet
                    if we can exhange session by message-passing Java loose the generic parameter types dynamically 
                    [fstype st] *)
        )
    }
    | S.CompType ct -> fcctype ct
    | S.EmptyMainType -> {place; value=T.TUnknown}
    and fmtype : S.main_type ->  T.ctype = function mt -> finish_mtype mt.place mt.value

    (************************************ Literals *****************************)

    and finish_literal place : S._literal -> T._literal = function
        | S.VoidLit -> T.VoidLit
        | S.BoolLit b -> T.BoolLit b
        | S.FloatLit f -> T.FloatLit f
        | S.IntLit i -> T.IntLit i
        | S.LabelLit l -> Core.Error.error place "Label are not yet supported"
        | S.StringLit str -> T.StringLit str

        | S.ActivationRef _ -> failwith "Activation info is not yet supported"

        | S.Place _ -> failwith "Place is not yet supported"
        | S.StaticBridge _ -> raise (Error.DeadbranchError "Bridge should have been process by the finish_expr (returns an expr)")
        | x -> failwith (S.show__literal x)
    and fliteral lit : T.literal = map_place finish_literal lit

    (************************************ Expr & Stmt *****************************)

    and finish_expr place (e, mt): T._expr * T.ctype =
    let fplace = place@(Error.forge_place "Plg=Akka/finish_expr" 0 0) in
    let auto_place smth = {place = fplace; value=smth} in
    (match e with
        | S.VarExpr x -> T.VarExpr x
        | S.AccessExpr (e1, {value=S.VarExpr x, _}) when Atom.is_builtin x -> Encode.encode_builtin_access place (fexpr e1) (Atom.value x)
        | S.AccessExpr (e1, e2) -> T.AccessExpr (fexpr e1, fexpr e2)
        | S.BinopExpr (t1, op, t2) -> T.BinopExpr (fexpr t1, op, fexpr t2)
        | S.LambdaExpr (params, e) -> 
            T.LambdaExpr (
                List.map (map0_place (fun _ (mt, x) -> fmtype mt, x)) params,
                auto_place (T.ReturnStmt (fexpr e))
            ) 
        | S.BridgeCall b -> 
        fst (e_bridge_of_protocol place (auto_place (T.VarExpr b.protocol_name, auto_place T.TUnknown))).value 
        | S.LitExpr {value=S.BLabelLit l; place=lit_place} -> 
            T.NewExpr( 
                auto_place(T.RawExpr "LabelEvent", auto_place T.TUnknown), 
                [ auto_place (T.LitExpr (auto_place (T.StringLit (Atom.to_string l))), auto_place T.TUnknown)]
            )
        | S.LitExpr {value=S.StaticBridge b; place=lit_place} -> 
            fst (e_static_bridge_of_protocol place (auto_place (T.VarExpr b.protocol_name, auto_place T.TUnknown)) b.id).value
        | S.LitExpr {value=S.VPlace vp} -> begin 
            T.CallExpr (
                auto_place(T.VarExpr (Atom.builtin "VPlaces.get"), auto_place T.TUnknown),
                [
                    auto_place (T.LitExpr(auto_place(T.StringLit (Atom.hint vp.name))), auto_place T.TUnknown)
                ]
            )
        end
        | S.LitExpr lit -> T.LitExpr (fliteral lit)
        | S.UnopExpr (op, e) -> T.UnopExpr (op, fexpr e)

        | S.CallExpr (e1, es) -> begin 
            match e1.value with 
            | S.VarExpr x,_ when Atom.is_builtin x ->
                Encode.encode_builtin_fct e1.place (Atom.value x) (List.map fexpr es)
            | _ -> T.CallExpr (fexpr e1, List.map fexpr es)
        end
        | S.NewExpr (e1, es) -> begin 
            match e1.value with 
            | S.VarExpr x,_ when Atom.is_builtin x ->
                Encode.encode_builtin_fct e1.place (Atom.value x) (List.map fexpr es)
            | _ -> T.NewExpr (fexpr e1, List.map fexpr es)
        end
        | S.This -> T.This
        | S.Spawn {c; args; at=None} ->
            T.ActivationRef{
                schema = auto_place (T.LitExpr ( auto_place (T.StringLit (Atom.to_string (IRMisc.schema_of c)))), auto_place T.TUnknown);
                actor_ref = auto_place (T.Spawn {
                    context = auto_place (T.CurrentContext, auto_place T.TUnknown);  
                    actor_expr= auto_place (T.CallExpr(
                        auto_place (T.VarExpr (Atom.builtin "spawn")
                        , auto_place T.TUnknown),
                        [auto_place (T.CallExpr (
                            auto_place (T.AccessExpr (
                                    fcexpr c,
                                    auto_place (T.VarExpr (Atom.builtin "create")
                                    , auto_place T.TUnknown)
                                ), auto_place T.TUnknown),
                                e_this_guardian fplace
                                :: List.map fexpr args
                            )
                        , auto_place T.TUnknown)] @ [ auto_place (T.LitExpr (auto_place (T.StringLit (Atom.to_string (Atom.fresh "actor_name")))), auto_place T.TUnknown)]
                    ), auto_place T.TUnknown)
                }, auto_place T.TUnknown)
            }
        | S.Spawn {c; args; at=Some at} ->
            (*
            TO BE adapter and co should be managed inside spawn at 
            ActorRef<author.project_name.a14.C33.Command> c54 = PlaceDiscovery.spawnAt(
            (ActorRef) getContext(),
            x -> author.project_name.a14.C33.create(),
            "toto",
            null,
            Place.currentPlace(getContext())
        ); 
            *)

            (* Local variable for lambda - should be unique to avoid name clash with constructor arguments. *)
            let a_context = Atom.fresh "context" in
            let a_guardian = Atom.fresh "guardian" in
            let a_timers = Atom.fresh "timers" in
            let schema = IRMisc.schema_of c in

            (* TODO ?? DUplicated with AkkaJAva [arg_lambda] ?? *)
            let runnable = 
            auto_place (T.LambdaExpr (
                [auto_place T.TUnknown, a_guardian],
                auto_place (T.BlockStmt [
                    auto_place (T.ReturnStmt (
                        auto_place (T.LambdaExpr (
                            [
                                (* ActorContext<author.project_name.a14.C33.Command> *)
                                auto_place (
                                    T.TParam(
                                        t_context place,
                                        [
                                            match (fmtype mt).value with
                                            | T.TActivationRef ct -> 
                                                (* NB a TUnknwon here can come from a CompType {TStruct} (see fcctype) or EmptyMainType*)
                                                assert( ct.value <> T.TUnknown);
                                                t_command_of place ct
                                        ]
                                    )
                                ),
                                a_context
                            ],
                            auto_place (T.BlockStmt [
                                auto_place (T.ReturnStmt (
                                    auto_place(T.CallExpr(
                                        e_behaviors_with_timers fplace,
                                        [
                                            auto_place (T.LambdaExpr (
                                                [auto_place T.TUnknown, a_timers],
                                                auto_place (T.BlockStmt [
                                                    auto_place (T.ExpressionStmt (
                                                    e_debug_of 
                                                        place 
                                                        (auto_place (T.VarExpr a_context, auto_place T.TUnknown)) 
                                                        [
                                                            auto_place (T.LitExpr (auto_place (T.StringLit ("SpawnAT::create"))), auto_place T.TUnknown)
                                                        ]
                                                    ));
                                                    auto_place (T.ReturnStmt (auto_place (
                                                        T.NewExpr (
                                                            fcexpr c,
                                                            (
                                                                List.map (function x -> auto_place (T.VarExpr x, auto_place T.TUnknown)) (a_context
                                                                ::a_timers
                                                                ::a_guardian
                                                                ::[]
                                                            )
                                                            @(List.map fexpr args))

                                                        ), auto_place T.TUnknown
                                                    )));
                                                ])
                                            ), auto_place T.TUnknown)
                                        ]
                                ), auto_place T.TUnknown)))
                            ])
                        ), auto_place T.TUnknown)
                    ))
                ])
            ), auto_place T.TUnknown) in
        
            T.ActivationRef {
                schema = auto_place (T.LitExpr ( auto_place (T.StringLit (Atom.to_string (IRMisc.schema_of c)))), auto_place T.TUnknown);
                actor_ref = 
                auto_place(T.CallExpr(
                    e_lg4dc_spawnat fplace,
                    [
                        e_get_context place;
                        e_this_guardian fplace;
                        runnable;
                        auto_place (T.LitExpr (auto_place (T.StringLit (Atom.to_string (Atom.fresh "actor_name")))), auto_place T.TUnknown);
                        auto_place (T.LitExpr (auto_place T.VoidLit), auto_place T.TUnknown);
                        fexpr at;
                    ]
                ), auto_place T.TUnknown)
            }
        | S.BoxCExpr _ -> failwith "finish_expr BoxCexpr is not yet supported"
                
        | S.OptionExpr None -> T.CallExpr (
            auto_place (T.VarExpr (Atom.builtin "Optional.empty"), auto_place T.TUnknown),
            []
        )  
        | S.OptionExpr (Some e) -> T.CallExpr (
            auto_place (T.VarExpr (Atom.builtin "Optional.of"), auto_place T.TUnknown),
            [fexpr e]
        )  
        | S.ResultExpr (None, Some err) ->  T.CallExpr (
            auto_place (T.VarExpr (Atom.builtin "Either.left"), auto_place T.TUnknown),
            [fexpr err]
        ) 
        | S.ResultExpr (Some ok, None) -> T.CallExpr (
            auto_place (T.VarExpr (Atom.builtin "Either.right"), auto_place T.TUnknown),
            [fexpr ok]
        ) 
        | S.ResultExpr (_,_) -> raise (Core.Error.PlacedDeadbranchError (place, "finish_expr : a result expr can not be Ok and Err at the same time."))
        | S.BlockExpr (b, es) -> T.BlockExpr(b, List.map fexpr es)
        | S.Block2Expr (b, ees) -> T.Block2Expr(b, List.map (function (e1, e2) -> fexpr e1, fexpr e2) ees) 
        | S.InterceptedActivationRef (e1, e2_opt) -> 
            T.InterceptedActivationRef{
                actor_ref = fexpr e1;
                intercepted_actor_ref = Option.map fexpr e2_opt
            }

        | S.TernaryExpr (e1, e2, e3) -> T.TernaryExpr (fexpr e1, fexpr e2, fexpr e3) 
    ), fmtype mt
    and fexpr e : T.expr = map_place finish_expr e

    and finish_stmt place : S._stmt -> T._stmt = 
    let fplace = place@(Error.forge_place "Plg=Akka/finish_stmt" 0 0) in
    let auto_place smth = {place = fplace; value=smth} in
    function
        | S.EmptyStmt -> T.CommentsStmt (AstUtils.LineComment "Empty Statement")

        (*S.* Binders *)
        | S.AssignExpr (x, e) -> T.AssignExpr (auto_place (T.VarExpr x, auto_place T.TUnknown), fexpr e)
        | S.AssignThisExpr (x, e) -> 
            T.AssignExpr ( 
                auto_place (T.AccessExpr (
                    auto_place (T.This, auto_place T.TUnknown),
                    auto_place (T.VarExpr x, auto_place T.TUnknown))
                , auto_place T.TUnknown),
                fexpr e)        
        | S.LetStmt (mt, x, e) ->  
            (* 
                Tmp fix, since right handside type of a let is never a TUnknown we use it for e;
                Once e= (_, not unknown) we could remove the following line
                FIXME    
            *)
            let e = {e with value = (fst e.value, mt)} in
            T.LetStmt (fmtype mt, x, Some (fexpr e))                             

        (*S.* Comments *)
        | S.CommentsStmt comments -> T.CommentsStmt comments.value

        (*S.* Control flow *)
        | S.BreakStmt -> T.BreakStmt
        | S.ContinueStmt -> T.ContinueStmt
        | S.ExitStmt _ -> failwith "Exist is not yet supported"
        | S.ForStmt (mt,x,e,stmt) -> T.ForStmt(fmtype mt, x, fexpr e, fstmt stmt)
        | S.IfStmt (e, s1, s2_opt) -> T.IfStmt (fexpr e, fstmt s1, Option.map fstmt s2_opt)
        | S.MatchStmt (_,_) -> Core.Error.error place "Match is not yet supported"
        | S.ReturnStmt e -> T.ReturnStmt (fexpr e) 

        (*S.*type name, type definition*)
        | S.ExpressionStmt {value=S.CallExpr({value=S.VarExpr x, _}, args), _} when Atom.is_builtin x && Encode.is_stmt_builtin (Atom.hint x) -> 
            Encode.encode_builtin_fct_as_stmt place (Atom.value x) (List.map fexpr args)
        | S.ExpressionStmt e -> T.ExpressionStmt (fexpr e) 
        | S.BlockStmt stmts -> T.BlockStmt (List.map fstmt stmts)
        
        | S.GhostStmt _ -> raise (Core.Error.PlacedDeadbranchError (place, "finish_stype : GhostStmt should have been remove by a previous compilation pass."))
    and fstmt stmt : T.stmt = map_place finish_stmt stmt

    and finish_eventdef (inner_place:Error.place) (name, mts, body) : T.event =
    match body with  
    | None -> { 
                place = inner_place; 
                value = {
                    T.vis=T.Public; 
                    T.name= name;
                    T.kind=T.Event; 
                    T.args=List.mapi ( fun i mt ->
                        fmtype mt, Atom.builtin ("value"^(string_of_int i))
                    ) mts
                }
            }

    and finish_function place : S._function_dcl -> T.method0 list = function
        | f ->
            let body = match f.body with
                | S.AbstractImpl stmts -> T.AbstractImpl (List.map fstmt stmts)
                | S.BBImpl bbterm -> 
                    T.BBImpl { 
                        place = bbterm.place; 
                        value = {
                            language=bbterm.value.language; 
                            body = (List.map (function
                                | S.Text t -> T.Text t 
                                | S.Varda e -> T.Varda (fexpr e)
                            ) bbterm.value.body)
                        }
                    }
            in

            let new_function : T.method0 = {
                place;
                value = { 
                    decorators      = [];
                    annotations     = [ T.Visibility T.Public ]; 
                    v = {
                        ret_type        = fmtype f.ret_type;
                        name            = f.name;
                        args            = (List.map fparam f.args);
                        body            = body; 
                        is_constructor  = false 
                    }
                }
            } in

            [new_function]
    and ffunction : S.function_dcl -> T.method0 list = function m -> finish_function m.place m.value
    (************************************ Component *****************************)


    (* return type is T._expr for now, since we built only one state with all the variable inside FIXME *)
    and finish_state place : S._state -> T._stmt = 
    let fplace = (Error.forge_place "Plg=Akka/finish_state" 0 0) in
    let auto_place smth = {place = fplace; value=smth} in
    function 
        | S.StateDcl {ghost; type0; name; body = S.InitExpr e} -> 
            T.LetStmt (fmtype type0, name, Some (fexpr e))
        | S.StateDcl {ghost; type0; name; body = S.InitBB bb_term} -> 
            T.LetStmt (
                fmtype type0, 
                name, 
                Some {
                    place = bb_term.place;
                    value = T.BBExpr (fbbterm bb_term), auto_place T.TUnknown
                })
        (*use global x as y;*)
        | S.StateDcl { ghost; type0; name; body = S.NoInit} ->
            T.LetStmt (fmtype type0, name, None)
    and fstate s : T.stmt = map_place finish_state s


    and finish_param place : S._param -> (T.ctype * T.variable) = function
    | mt, x -> fmtype mt, x
    and fparam : S.param -> (T.ctype * T.variable) = function p -> finish_param p.place p.value


    and finish_contract place (method0 : T.method0) (contract : S._contract) : T.method0 list =
        let fplace = (Error.forge_place "Plg=Akka/finish_contract" 0 0) in
        let auto_place smth = {place = fplace; value=smth} in

        (* Inner logic of the method *)
        let inner_name = Atom.fresh ((Atom.hint contract.method_name)^"_inner") in
        let inner_method = {
            place = method0.place;
            value = { method0.value with 
                annotations = [T.Visibility T.Private];
                v = {method0.value.v with name = inner_name; }
            }
        } in

        (* Pre binders *)
        let with_params = List.map (function (x,y,_) -> finish_param place (x,y)) contract.pre_binders in
        let with_stmts : S.stmt list = List.map (function (x,y,z) -> {place; value=S.LetStmt (x,y,z)}) contract.pre_binders in 
        let with_stmts : T.stmt list = List.map fstmt with_stmts in
        (*let with_body : T.stmt = T.BlockStmt with_stmts in*)

        (* Pre-condition *)
        let ensures_methods, ensures_stmts = match contract.ensures with
        | None -> [], [] 
        | Some ensures_expr -> begin
            let ensures_name    = Atom.fresh ((Atom.hint contract.method_name)^"_ensures") in
            let ensures_params  = method0.value.v.args @ with_params in

            let ensures_method : T.method0 = {
                place = ensures_expr.place;
                value = {
                    decorators       = [];
                    annotations     = [ T.Visibility T.Private ];
                    v = {
                        ret_type        = { place; value=T.Atomic "boolean"};
                        name            = ensures_name;
                        body            =  T.AbstractImpl ([{ 
                            place= ensures_expr.place;
                            value = T.ReturnStmt (fexpr ensures_expr)
                        }]);
                        args            = ensures_params;
                        is_constructor  = false
                    }
                }
            } in

            [ensures_method], [ 
                { place = ensures_expr.place; value= T.IfStmt (
                    {place = ensures_expr.place; value = T.UnopExpr ( 
                        AstUtils.Not,
                        {place = ensures_expr.place; value = T.CallExpr ( 
                            {place = ensures_expr.place; value = T.VarExpr ensures_name, auto_place T.TUnknown}, 
                            List.map (function param -> {place = ensures_expr.place; value =T.VarExpr (snd param), auto_place T.TUnknown}) ensures_params 
                        ), auto_place T.TUnknown}
                    ), auto_place T.TUnknown},
                    {place = ensures_expr.place; value = T.ExpressionStmt (
                        {place = ensures_expr.place; value = T.AssertExpr (
                            {place = ensures_expr.place; value = T.LitExpr (
                                {place = ensures_expr.place; value = T.BoolLit false}), auto_place T.TUnknown}
                        ), auto_place T.TUnknown}
                    )}, (*TODO refine*)
                    None
                )}
            ]
        end in 

        (* Post-condition *)
        let returns_methods, returns_stmts = match contract.returns with
        | None -> [], [
            {place; value = T.ReturnStmt ( 
                {place; value = T.CallExpr (
                    {place; value=T.VarExpr inner_name, auto_place T.TUnknown},
                    List.map (function param -> {place; value=T.VarExpr (snd param), auto_place T.TUnknown}) method0.value.v.args
                ), auto_place T.TUnknown})
            }
        ] 
        | Some returns_expr -> begin
            let returns_name    = Atom.fresh ((Atom.hint contract.method_name)^"_returns") in
            let ret_type_param  = (method0.value.v.ret_type, Atom.fresh "res") in 
            let returns_params  = ret_type_param :: method0.value.v.args @ with_params in

            let returns_method  : T.method0 = {
                place = returns_expr.place;
                value = {
                    decorators      = [];
                    annotations     = [ T.Visibility T.Private ];
                    v = {
                        ret_type        = { place = returns_expr.place; value=T.Atomic "boolean"};
                        name            = returns_name;
                        body            = T.AbstractImpl ([
                            {place = returns_expr.place; value= T.ReturnStmt (
                                {place = returns_expr.place; value=T.CallExpr(
                                    fexpr returns_expr,
                                    [{place = returns_expr.place; value=T.VarExpr (snd ret_type_param), auto_place T.TUnknown}]
                                    ), auto_place T.TUnknown
                                })
                            }
                        ]);
                        args            = returns_params;
                        is_constructor  = false
                    }
                }
            } in

            [returns_method], [ 
                {place; value= T.LetStmt (
                    method0.value.v.ret_type,
                    (snd ret_type_param),
                    Some ({place; value=T.CallExpr (
                        {place; value=T.VarExpr inner_name, auto_place T.TUnknown},
                        List.map (function param -> {place; value=T.VarExpr (snd param), auto_place T.TUnknown}) method0.value.v.args
                    ), auto_place T.TUnknown})
                )};
                {place; value=T.IfStmt (
                    {place; value=T.UnopExpr ( 
                        AstUtils.Not,
                        {place; value=T.CallExpr ( 
                            {place; value=T.VarExpr returns_name, auto_place T.TUnknown}, 
                            List.map (function param -> {place; value=T.VarExpr (snd param), auto_place T.TUnknown}) returns_params 
                        ), auto_place T.TUnknown}
                    ), auto_place T.TUnknown},
                    {place; value=T.ExpressionStmt (
                        {place; value=T.AssertExpr (
                            {place; value=T.LitExpr (
                                {place; value=T.BoolLit false}
                            ), auto_place T.TUnknown}
                        ), auto_place T.TUnknown}
                    )}, (*TODO refine*)
                    None
                )};
                {place; value=T.ReturnStmt (
                    {place; value=T.VarExpr (snd ret_type_param), auto_place T.TUnknown}
                )}
            ]
        end in

        let main_stmts =
            with_stmts @
            ensures_stmts @
            returns_stmts
        in

        let main_method : T.method0 = {
            place = method0.place@place;
            value = {
                decorators      = method0.value.decorators;
                annotations     = method0.value.annotations;
                v = {
                    ret_type        = method0.value.v.ret_type;
                    name            = method0.value.v.name;
                    body            = T.AbstractImpl main_stmts;
                    args            = method0.value.v.args;
                    is_constructor  = method0.value.v.is_constructor 
                }
            }
        } in 

        let methods =
            [inner_method] @
            ensures_methods @
            returns_methods @
            [main_method]
        in

        methods
    and fcontract actor_name (method0 : T.method0) : S.contract -> T.method0 list = function m -> finish_contract m.place method0 m.value
        
    and finish_method place actor_name (m0 : S._method0) : T.method0 list = 
        assert( false = m0.on_destroy); (* TODO not yet supported*)
        let body = match m0.body with
            | S.AbstractImpl stmts -> T.AbstractImpl (List.map fstmt stmts)
            | S.BBImpl body -> T.BBImpl (fbbterm body)
        in

        let new_method : T.method0 = {
            place;
            value = { 
                decorators      = []; 
                annotations     = [ T.Visibility T.Public ]; 
                v = {
                    ret_type        = fmtype m0.ret_type;
                    name            = if m0.on_startup then actor_name else m0.name;
                    args            = (List.map fparam m0.args);
                    body            = body; 
                    is_constructor  = m0.on_startup 
                }
            }
        } in

        begin
            match m0.contract_opt with
            | None -> [new_method]
            | Some contract -> fcontract actor_name new_method contract 
        end
    and fmethod actor_name : S.method0 -> T.method0 list = function m -> finish_method m.place actor_name m.value

    and finish_bbterm place {S.language; body} = 
    {
        T.language;
        body = List.map (
            function 
            | S.Text t -> T.Text t
            | S.Varda e -> T.Varda (fexpr e)
        ) body
    }
    and fbbterm bbterm: T.blackbox_term = (map_place finish_bbterm) bbterm



    and finish_component_dcl place : S._component_dcl -> T.actor list = function
    | S.ComponentStructure {name; body} -> begin 
        (* Registration *)
        Hashtbl.add to_capitalize_variables name ();
        collected_components := Atom.Set.add name !collected_components;

        (****** Helpers *****)
        let fplace = (Error.forge_place "Plg=Akka/finish_term/protocoldef" 0 0) in
        let auto_place smth = {place = fplace; value=smth} in
        let expr2stmt e : T.stmt = auto_place (T.ExpressionStmt e) in
        let exprs2stmts es : T.stmt list = List.map expr2stmt es in

        (***** Processing *****)

        (* Group by kind the elmts : method, state, ...
            in order to display well stuctured code at the end (and since the order of definition do not matter)
        *)
        let grp_items = group_cdcl_by body in
        List.iter (function x -> 
            (* Ensure that x is static otherwise it should be a state *)
            match x.value with
            | S.Typedef _ -> ()
            | S.Comments _ -> () (* TODO needs to get ride of grp_items to support comments at the right place *)
            | _ -> raise (Error.PlacedDeadbranchError (x.place, "Non static term in others"));
        ) grp_items.others;

        
        (*** Building events ***)
        (* Events that should be defined inside the Actor *)
        let is_stype = function
            | _, Some ({Core.AstUtils.place; Core.AstUtils.value = S.SType st}) -> false
            | _ -> true
        in
        let events = List.map ( function
            | {value=S.EventDef (name, mts, body); place} ->
                finish_eventdef place (name, mts, body)
        ) grp_items.eventdefs in
        (* 
            List.flatten (List.map (function x -> snd (fmtype x)) (List.filter_map (function x -> match snd x with |S.AbstractTypealias mt -> Some mt | _ -> None) (List.map (function |{value=S.EventDef (x, mts,body); _} -> (x, mts, body)) grp_items.eventdefs))) in*)
        

        (*** Building states ***)
        let states : T.state list = List.map (
            function state -> {
                place = state.place; 
                value = {T.persistent=false; stmts= [fstate state] }
            } (* TODO handle persistency*)
        ) grp_items.states in 

        let a_schema = Atom.builtin "schema" in
        let a_intermediate_states = Atom.builtin "intermediate_states" in
        let a_frozen_sessions = Atom.builtin "frozen_sessions" in
        let a_dead_sesison = Atom.builtin "dead_sessions" in

        let states = [
            (* String schema = "schema_of ";*)
            auto_place {
                T.persistent = false; (*TODO persistence True ??*)
                stmts = [ auto_place(T.LetStmt (
                    auto_place (T.Atomic "String"),
                    a_schema,
                    Some (auto_place(T.LitExpr (auto_place (T.StringLit (Atom.to_string name))), auto_place T.TUnknown))
                ))]
            };
            (* Set<UUID> frozen_sessions = new HashSet();*)
            auto_place {   T.persistent = false; (*TODO persistence True ??*)
                stmts = [ auto_place(T.LetStmt (
                    auto_place (T.TSet(auto_place( T.Atomic "UUID"))),
                    a_frozen_sessions,
                    Some (auto_place(T.BlockExpr(Core.AstUtils.Set, []), auto_place T.TUnknown))
                ))]
            };
            (* Set<UUID> dead_sessions = new HashSet() *)
            auto_place {   T.persistent = false;(*TODO persistence True ??*)
                stmts = [ auto_place(T.LetStmt (
                    auto_place (T.TSet(auto_place( T.Atomic "UUID"))),
                    a_dead_sesison,
                    Some (auto_place(T.BlockExpr(Core.AstUtils.Set, []), auto_place T.TUnknown))
                ))]
            }
        ] @ states in

        (*** Outports ***)
        (* outport p on bridge => state of type outport => Outport<P> p = Outport(bridge);
        *)
        let states = 
            List.map (function p -> 
                let _p : S._outport = fst p.value in
                auto_place {   
                    T.persistent = false; (*TODO persistence True ??*)
                    stmts = [ auto_place(T.LetStmt (
                        auto_place( T.Atomic "OutPort"),
                        _p.name,
                        Some (e_outport_of p.place (fexpr _p.input))
                    ))]
                }
            ) grp_items.outports
            @ states 
        in




        (*** Building receiver ***)
        (* Step0 - name of receiver param (event) *)
        let l_event_name : Atom.atom = (Atom.fresh "e") in
        let l_event : T.expr = auto_place (T.VarExpr l_event_name, auto_place T.TUnknown) in

        (* Step1 - create {event_name: {(bridge_expr, st, remaining_step i.e st) ->  callbak}} *)
        let env : (Atom.atom, (T.expr * S.session_type * S.session_type, T.expr) Hashtbl.t) Hashtbl.t = Hashtbl.create 16 in
        let hydrate_env (p: S.port) = 
            let expecting_st, (msg_type, remaining_st) = match (fst p.value).expecting_st.value with 
            | S.SType st -> begin
                let t_msg, st_continuation = IRMisc.msgcont_of_st st in
                
                (* Convert to event *)
                let event_name = match t_msg.value with
                    | S.CType {value=S.TVar event_name;} -> event_name
                    | S.CType {value=S.TFlatType AstUtils.TBLabel} -> Atom.builtin "LabelEvent"
                in

                (
                    match (IRMisc.unfold_st_star st).value with
                    | S.STBranch _ | S.STRecv _ -> () 
                    | st -> 
                        logger#error "%s" (S.show__session_type st);
                        Core.Error.error (fst p.value).expecting_st.place "%s plugin: expecting type can only start by the reception of a message or of a label" plg_name
                );

                st, (event_name, st_continuation)
            end 
            | _ -> Core.Error.error (fst p.value).expecting_st.place "%s plugin do not support main type for port expecting" plg_name  
            in
        
            let inner_env : (T.expr * S.session_type * S.session_type, T.expr) Hashtbl.t= begin 
                try 
                    Hashtbl.find env msg_type 
                with Not_found -> let _inner_env = Hashtbl.create 8 in Hashtbl.add env msg_type _inner_env; _inner_env
            end in

            let key = (fexpr (fst p.value).input, expecting_st, remaining_st) in

            (* check that key are not duplicated for the current event *)
            try
                ignore (Hashtbl.find inner_env key);
                Error.error (place@p.place) "Tuple (bridge, st) is not unique for the component %s" (Atom.hint name)
            with Not_found -> Hashtbl.add inner_env key (fexpr (fst p.value).callback)
        in

        List.iter hydrate_env grp_items.ports;

        (* Step 2 - Generate a receiver per event *)
        let generate_event_receiver (event_name:Atom.atom) (inner_env:(T.expr * S.session_type * S.session_type, T.expr) Hashtbl.t) : T.stmt list =
            (* Helpers *)
            let bridgeid (bridge: T.expr) = auto_place( T.CallExpr(
                auto_place (T.AccessExpr (
                    bridge,
                    auto_place (T.VarExpr (Atom.builtin "get_id"), auto_place T.TUnknown)
                ), auto_place T.TUnknown),
                []
            ), auto_place T.TUnknown) in
            let e_bridgeid e = auto_place (T.AccessExpr (
                e,
                auto_place (T.VarExpr (Atom.builtin "bridge_id"), auto_place T.TUnknown)
            ), auto_place T.TUnknown) in
            let e_sessionid e = auto_place (T.AccessExpr (
                e,
                auto_place (T.VarExpr (Atom.builtin "session_id"), auto_place T.TUnknown)
            ), auto_place T.TUnknown) in
            let e_replyto e = auto_place (T.AccessExpr (
                e,
                auto_place (T.VarExpr (Atom.builtin "replyTo"), auto_place T.TUnknown)
            ), auto_place T.TUnknown) in
            let e_remaining_step e = auto_place (T.AccessExpr (
                e,
                auto_place (T.VarExpr (Atom.builtin "st"), auto_place T.TUnknown)
            ), auto_place T.TUnknown) in
            let e_init_stage e = auto_place (T.AccessExpr (
                e,
                auto_place (T.VarExpr (Atom.builtin "init_stage"), auto_place T.TUnknown)
            ), auto_place T.TUnknown) in
            let e_hidden_right e = auto_place (T.AccessExpr (
                e,
                auto_place (T.VarExpr (Atom.builtin "hidden_right"), auto_place T.TUnknown)
            ), auto_place T.TUnknown) in


            (* Handle frozen/timeout session
                if this.dead_sessions.contains(e.session_id) {
                    context.getLog().info(String.format("Receive message belonging to a timeout session %s : drop.", e.sesion_id));
                    e.replyTo.tell(new SessionHasTimeout(e.session_id));
                    return Behaviors.same();
                }
                (* per event dispatcher *)
                if this.frozen_sessions.contains(e.session_id) {
                    context.getLog().info(String.format("Receive message belonging to a frozen session %s : drop.", e.sesion_id));
                    e.replyTo.tell(SessionIsFrozen(e.session_id));
                    return Behaviors.same();
                }
            *)
            let add_check_session_validity ()=
                auto_place (T.IfStmt(
                    auto_place(T.UnopExpr(
                        Core.AstUtils.Not,    
                        auto_place (T.CallExpr(
                            auto_place (T.AccessExpr(
                                auto_place (T.VarExpr (Atom.builtin "Handlers"), auto_place T.TUnknown),
                                auto_place (T.VarExpr (Atom.builtin "is_session_alive"), auto_place T.TUnknown)
                            ), auto_place T.TUnknown),
                            [ 
                                e_cast fplace "ActorContext" (e_get_context fplace);
                                e_cast fplace "ActivationRef" (e_get_self_activation fplace (e_get_context fplace));
                                e_this_frozen_sessions fplace; 
                                e_this_dead_sessions fplace; 
                                e_sessionid l_event; 
                                e_replyto l_event;
                            ]
                        ), auto_place T.TUnknown)
                    ), auto_place T.TUnknown),
                    auto_place(T.BlockStmt [
                        auto_place(T.ReturnStmt (e_behaviors_same fplace)) 
                    ]),
                    None 
                ))
            in

            (* Creating the statement*)
            (* TODO do it with a switch ??? *)
            (*
                if(e.bridge_id == author.project_name.Stage219.b36.get_id() && e.st == current_aststype){
                    author.project_name.Session<?> s = new Session(e.bridge_id, getContext().getSelf(), e.replyTo, e.st);
                    s.set_id(e.session_id);
                    ASTStype.TimerHeader.apply_headers(getContext(), this.timers, s.session_id, s.continuations._2);
                    
            
                    this.handle_ping61(s);
                }else{
                    ...
                }
            *)

            let a_session = Atom.builtin "s" in
            let l_session = auto_place (T.VarExpr a_session, auto_place T.TUnknown) in

            let add_case (bridge, st, remaining_st) (callback:T.expr) acc : T.stmt =
                auto_place (T.IfStmt (
                    auto_place (T.BinopExpr(
                        auto_place (T.BinopExpr (e_bridgeid l_event, AstUtils.StructuralEqual, bridgeid bridge), auto_place T.TUnknown),
                        AstUtils.And,
                        auto_place (T.BinopExpr (e_remaining_step l_event, AstUtils.StructuralEqual, fvstype (IRMisc.dual st)), auto_place T.TUnknown)
                    ), auto_place T.TUnknown),
                    auto_place (T.BlockStmt [
                        auto_place (T.LetStmt (
                            t_lg4dc_session place,
                            a_session,
                            Some (auto_place (T.NewExpr(
                                e_lg4dc_session place,
                                [
                                    e_bridgeid l_event;
                                    auto_place (T.CastExpr(
                                        auto_place (T.TVar (Atom.builtin "ActivationRef")),
                                        e_get_self_activation place (e_get_context place)
                                    ), auto_place T.TUnknown);
                                    e_replyto l_event;
                                    fvstype remaining_st;
                                    e_init_stage l_event;
                                    e_hidden_right l_event;
                                ]
                            ), auto_place T.TUnknown))
                        ));
                        auto_place (T.ExpressionStmt (auto_place (
                            T.CallExpr(
                                e_setid_of_session fplace a_session,
                                [ e_sessionid l_event]
                            ), auto_place T.TUnknown
                        )));
                        auto_place (T.ExpressionStmt (
                            e_apply_headers fplace l_session
                        ));
                        auto_place (T.ExpressionStmt (auto_place (
                            T.CallExpr(
                                callback,
                                [ l_event; l_session ]
                            ), auto_place T.TUnknown
                        )))
                    ]),
                    Some acc
                ))
            in

            (* return Behaviors.same(); *)
            let ret_stmt = T.ReturnStmt (e_behaviors_same fplace) in

            [
                add_check_session_validity ();
                Hashtbl.fold add_case inner_env (auto_place (T.ExpressionStmt ((e_error_of fplace (e_get_context fplace) [
                    auto_place (T.LitExpr (auto_place(T.StringLit"Dispatcher does not caught message ")), auto_place T.TUnknown);
                    auto_place (T.CallExpr (
                        auto_place(T.AccessExpr(
                            l_event,
                            auto_place (T.RawExpr "toString", auto_place T.TUnknown)
                        ), auto_place T.TUnknown),
                        []
                    ), auto_place T.TUnknown);
                ]))));
                auto_place ret_stmt
            ]
        in

        (* Step3 - Generate the component receiver *)
        let generate_component_receiver () = 
            let init_receiver_expr : T.expr = {place; value=T.CallExpr(
                {place; value=T.VarExpr (
                    Atom.builtin "newReceiveBuilder"
                ), auto_place T.TUnknown}, 
                []
            ), auto_place T.TUnknown} in

            let add_timer_case acc (event_name, handler) =
                {place; value=T.AccessExpr(
                    acc, 
                    {place; value=T.CallExpr(
                        {place; value=T.VarExpr (Atom.builtin "onMessage"), auto_place T.TUnknown}, 
                        [
                            {place; value=T.ClassOf (auto_place (T.TVar (Atom.builtin event_name))), auto_place T.TUnknown};
                            auto_place (T.LambdaExpr (
                                [
                                    auto_place T.TUnknown, l_event_name 
                                ],
                                auto_place(T.BlockStmt [
                                    auto_place(T.ExpressionStmt(auto_place(T.CallExpr(
                                        auto_place (T.VarExpr (Atom.builtin handler), auto_place T.TUnknown),
                                        [
                                            e_get_context fplace;
                                            e_get_self_activation place (e_get_context fplace);
                                            (*Rt.Misc.e_this_timers;*)
                                            e_this_frozen_sessions fplace;
                                            e_this_dead_sessions fplace;
                                            e_this_intermediate_states fplace;
                                            l_event
                                        ]
                                    ), auto_place T.TUnknown)));
                                    auto_place(T.ReturnStmt(
                                        e_behaviors_same fplace
                                    ))
                                ])
                            ), auto_place T.TUnknown)
                        ]
                    ), auto_place T.TUnknown}
                ), auto_place T.TUnknown}
            in
            let init_receiver_expr = List.fold_left add_timer_case init_receiver_expr [
                "HBSessionTimer", "Handlers.onHBTimer";
                "LBSessionTimer", "Handlers.onLBTimer";
                "SessionIsDead", "Handlers.onSessionIsDead";
                "AckDeadSession", "Handlers.onAckDeadSession";
            ] in

            let add_case event_name inner_env (acc, acc_methods) =
                let _m_name = Atom.fresh "event_dispatcher" in
                let _m : T.method0 = auto_place {
                    T.decorators = [];
                    annotations = [T.Visibility T.Public];
                    v = {
                        T.ret_type = t_behavior_of_actor fplace name;
                        name = _m_name;
                        body = AbstractImpl (generate_event_receiver event_name inner_env);
                        args = [
                            (
                                auto_place (T.TParam (
                                    auto_place(T.TVar event_name),
                                    [ ] (* FIXME for now metadata is fixed per type of event and cannot be changed in a per channel basis TODO t_lg4dc_nometadata fplace ]*)
                                )), 
                                l_event_name
                            )

                        ];
                        is_constructor = false;
                    }
                } in

                ({place; value=T.AccessExpr(
                    acc, 
                    {place; value=T.CallExpr(
                        {place; value=T.VarExpr (Atom.builtin "onMessage"), auto_place T.TUnknown}, 
                        [
                            {place; value=T.ClassOf (auto_place (T.TVar event_name)), auto_place T.TUnknown};
                            auto_place (T.AccessMethod (
                                auto_place (T.This, auto_place T.TUnknown),
                                _m_name
                            ), auto_place T.TUnknown)
                        ]
                    ), auto_place T.TUnknown}
                ), auto_place T.TUnknown}, _m::acc_methods)
            in
            Hashtbl.fold add_case env (init_receiver_expr, [])
        in

        let (receiver_body, receiver_methods) = generate_component_receiver () in

        let receiver_expr = {place; value=T.AccessExpr(
            receiver_body, 
            { place; value=T.CallExpr(
                {place; value=T.VarExpr (Atom.builtin "build"), auto_place T.TUnknown},
                []
            ), auto_place T.TUnknown}), auto_place T.TUnknown}
        in

        let receiver : T.method0 = { 
            place;
            value = {
                annotations     = [ T.Visibility T.Public ];
                decorators      = [Override];
                v = {
                    args            = [];
                    body            = T.AbstractImpl ([
                        {place; value=T.ReturnStmt receiver_expr}
                    ]);
                    name            = Atom.builtin "createReceive";
                    ret_type        = t_receive_of_actor place name;
                    is_constructor  = false
                }
            }
        } in

        (* Step 4 - Prepare parent_env for updating event definition in order to
            event Pong implements C.Command for all C that can receive a Pong event
        *)

        Hashtbl.iter (fun event _ -> logger#warning ">>> %s %s" (Atom.to_string event) (Atom.to_string name);add_event_e2rs event name) env;
        
        (***** Building methods *****)
        let methods = receiver_methods @ (List.flatten (List.map (fmethod name) grp_items.methods)) in 
        

        (***** Sumup *****)
        [{
            place;
            value = {   
                T.extended_types = [];
                implemented_types = [];
                is_guardian = false;
                name;
                methods; 
                states;
                events;
                nested_items= List.map 
                    (function (x: T.actor) -> { 
                        place = x.place; 
                        value={ 
                            T.annotations=[ T.Visibility T.Public ];
                            decorators= [];
                            v = T.Actor {
                                place=x.place; 
                                value=x.value
                            }
                        }
                    })
                    (List.flatten (List.map fcdcl grp_items.nested)
                ); 
                static_items = List.flatten (List.map fterm grp_items.others); 
                receiver = receiver
            }
        }]
    end 
    | S.ComponentAssign _ -> failwith "Component expr are not yet supported" 

    and fcdcl  : S.component_dcl -> T.actor list = function cdcl -> finish_component_dcl cdcl.place cdcl.value 

    (********************** Manipulating component structure *********************)
    and finish_component_expr place = function
        | S.VarCExpr x, mt -> T.VarExpr x, fmtype mt
        | _ -> failwith "Akka plg do not support yet advance component expr" 
    and fcexpr ce : T.expr = map_place finish_component_expr ce

    (************************************ Program *****************************)

    (* generate the _0_, ..., _n_ getters *)
    and make_getters args = 
        let fplace = (Error.forge_place "Plg=Akka/make_getter" 0 0) in
        let auto_fplace smth = {place = fplace; value=smth} in
        let make_getter i (ct, name) = auto_fplace {
            T.annotations = [];
            decorators = [];
            v = T.MethodDeclaration (auto_fplace {
                T.annotations = [T.Visibility T.Public];
                decorators = [];
                v = {
                    T.ret_type = ct;
                    name = Atom.builtin (Printf.sprintf "_%d_" i);
                    body = T.AbstractImpl [
                        auto_fplace (T.ReturnStmt (
                            auto_fplace (T.AccessExpr(
                                auto_fplace (T.This, auto_fplace T.TUnknown),
                                auto_fplace (T.VarExpr name, auto_fplace T.TUnknown)
                            ), ct)
                        ))
                    ];
                    args = [];
                    is_constructor = false; 
                }
            })
        } in
        List.mapi make_getter args

    and finish_term place : S._term -> T.term list = 
    let fplace = place@(Error.forge_place "Plg=Akka/finish_term" 0 0) in
    let auto_place smth = {place = fplace; value=smth} in
    function
    | S.Comments c -> [{
        place;
        value= {
            T.annotations = [];
            decorators = [];
            v = T.Comments c.value
        }
    }]
    | S.Component cdcl -> List.map (function a -> {
        place=a.place; 
        value= {
            T.annotations = [];
            decorators = [];
            v=T.Actor a
        }
    }) (fcdcl cdcl)
    | S.Stmt stmt -> [{
        place; 
        value= {
            T.annotations = [];
            decorators = [];
            v = T.Stmt (fstmt stmt)
        }
    }]
    | S.Function f -> List.map (function m -> {
        place; 
        value= {
            T.annotations = [];
            decorators = [];
            v = T.MethodDeclaration m
        }
    }) (ffunction f)
    | S.Typedef {value=S.ProtocolDef (name, {value=S.SType st; _});_} -> 
        (*** Helpers ***)
        let fplace = (Error.forge_place "Plg=Akka/finish_term/protocoldef" 0 0) in
        let auto_place smth = {place = fplace; value=smth} in
        let expr2stmt e : T.stmt = auto_place (T.ExpressionStmt e) in
        let exprs2stmts es : T.stmt list = List.map expr2stmt es in

        (* Registration *)
        Hashtbl.add to_capitalize_variables name ();


        (* TODO generalize the usage of auto place *)

        (*** Processing ***)
        (* case protocol definition *)
        let rec extract_events place k = function
        | S.STEnd | S.STVar _ -> []
        | S.STInline _ -> raise (Error.PlacedDeadbranchError (place, "STInline should have been removed by the partial evaluation pass"))
        | S.STSend ({value=S.CType {value=S.TVar name;_};_}, st_next) | S.STRecv ({value=S.CType{value=S.TVar name;_};_}, st_next) -> 
            { 
                place = place; 
                (*
        goal public static final class Pong implements Event {}
                *)
                value = {
                    T.vis=T.Public; 
                    T.name= name;
                    T.kind=T.Event; 
                    T.args= []
                }
            }:: (extract_events st_next.place (k+1) st_next.value)
        | (S.STSend _ as t)| (STRecv _ as t)-> failwith "toto"
        | S.STBranch entries | STSelect entries -> begin
            let aux_entry (label, st, _) = 
                { 
                    place; 
                    value = {
                        T.vis = T.Public; 
                        T.name = label;
                        T.kind = T.Event; 
                        T.args = []
                    }
                }
            in
            List.map aux_entry entries
        end
        | S.STRec (x, st_next) -> extract_events st_next.place k st_next.value
        in

        (*let events = extract_events st.place 0 st.value in
        let events = List.map (function e -> {place=e.place@fplace; value=T.Event e}) events in*)

        (*** Helpers ***)
        let l_st = Atom.builtin "st" in
        let this_st = T.AccessExpr (
            auto_place (T.This, auto_place T.TUnknown), 
            auto_place (T.VarExpr l_st, auto_place T.TUnknown)
        ) in

        (* com.lg4dc.Protocol *)
        let m_get_st = {
            T.decorators = [];
            annotations = [T.Visibility T.Public];
            v = {
                T.ret_type = auto_place (T.TVar (a_ASTStype_of ""));
                name = Atom.builtin "get_st";
                body = AbstractImpl [
                    auto_place (T.ReturnStmt (
                        fvstype st
                    ))
                ];
                args = [];
                is_constructor = false;
            }
        } in

        (*** Body assembly ***)
        let sub_classes = List.map (function cl -> auto_place cl) [] in

        let methods : T.method0 list = List.map auto_place [m_get_st] in
        let methods = List.map (function m -> auto_place ({
            T.annotations = [];
            decorators = [];
            v = T.MethodDeclaration m
        })) methods in

        let stmts = List.map (function stmt -> auto_place ({
            T.annotations = [];
            decorators = [];
            v = T.Stmt stmt
        })) [] in

        [{
            place; 
            value = {
                T.annotations = [T.Visibility T.Public];
                decorators = [];
                v = T.ClassOrInterfaceDeclaration {
                    isInterface = false;
                    extended_types = [t_lg4dc_protocol place];
                    implemented_types = [];
                    name = name;
                    body = stmts @ sub_classes @ methods (*@ events*)
                }
            }
        }]

        (* TODO generate the dynamic checking of protocol order if needed *)

    | S.Typealias (v, S.AbstractTypealias body) -> raise (Error.PlacedDeadbranchError (place, "partial evaluation should have removed type alias exept those from impl"))
    | S.Typealias (x, S.BBTypealias body) as term -> 
        (* Java does not support type aliasing *)
        Hashtbl.add typealias x body;
        []
    |Typedef {value= EventDef (name, mts, None) as tdef; place = inner_place} ->
        (* Registration *)
        Hashtbl.add to_capitalize_variables name ();

        [{
            place;
            value = {
                T.annotations = [];
                decorators = [];
                v = T.Event (finish_eventdef inner_place (name, mts, None)) 
            }
        }]

    (* Inductive type definition *)
    | S.Typedef  {value= ClassicalDef (name, args, None) as tdef; place} -> (* implicit constructor should translate to akka *)
        (* Registration *)
        Hashtbl.add to_capitalize_variables name ();

        let args = List.map (function (arg:T.ctype) -> (arg, Atom.fresh "arg")) (List.map fmtype args) in
        let constructor_body = 
            let place = (Error.forge_place "Plg=Akka/finish_term/typedef/implicit_constructor" 0 0) in
            let aux (_, arg_name) = 
                { place; value = T.AssignExpr (
                    { place; value = T.AccessExpr (
                        { place; value = T.This, auto_place T.TUnknown }, 
                        { place; value = T.VarExpr arg_name, auto_place T.TUnknown }
                    ), auto_place T.TUnknown},
                    { place; value = T.VarExpr arg_name, auto_place T.TUnknown }
                )}
            in
            T.BlockStmt (List.map aux args)
        in
        let fields = 
            let place = (Error.forge_place "Plg=Akka/finish_term/typedef/implicit_constructor" 0 0) in
            let aux (arg_ctype, arg_name) = { 
                place; 
                value = {
                    T.annotations = [];
                    decorators = [];
                    v = T.Stmt { 
                        place; 
                        value = T.LetStmt (
                            arg_ctype, 
                            arg_name,
                            None 
                        )
                    }
                }
            } in
            List.map aux args
        in

        let constructor = { place = place @ (Error.forge_place "Plg=Akka/finish_term/typedef/implicit_constructor" 0 0); value= {
            T.annotations = [];
            decorators = [];
            v = T.MethodDeclaration { place; value = {
                T.annotations = [T.Visibility T.Public];
                decorators = [];
                v = {
                    T.ret_type = {place; value = T.Atomic "void"}; (* removed by the is_constructor *)
                    name;
                    body = T.AbstractImpl [{place; value  = constructor_body}];
                    args = args;
                    is_constructor = true
                }
            }}
        }} in


        let getters = make_getters args in

        [{
            place; 
            value = {
                T.annotations = [T.Visibility T.Public];
                decorators = [];
                v = T.ClassOrInterfaceDeclaration {
                    isInterface = false;
                    extended_types = [];
                    implemented_types = [];
                    name;
                    body = fields @ (constructor::getters) 
                }
            }
        }]
    | S.Typedef {value = ClassicalDef (v, _, Some body); _} ->
        (* Registration *)
        Hashtbl.add to_capitalize_variables v ();
        [{
            place; 
            value = {
                T.annotations = [];
                T.decorators = [];
                v = T.TemplateClass (fbbterm body)
            }
        }]
    | S.Typedef {value = EventDef (v, _, Some body); _} -> Error.error place "eventdef with body is not yet supported by the akka.finish"
    | S.Typedef {value = VPlaceDef x;} -> 
        (* Registration *)
        Hashtbl.add to_capitalize_variables x ();
        [{
            place; 
            value = {
                T.annotations = [];
                T.decorators = [];
                v = begin
                   T.ClassOrInterfaceDeclaration {
                       isInterface = false;
                       name = x;
                       extended_types = [];
                       implemented_types = []; (* TODO inherit ??*)
                       body = [] (* TODO add x as identity ?? *)
                   } 
                end
            }
        }]

    and fterm : S.term -> T.term list = function t -> finish_term t.place t.value

    let cstate = ref (empty_cstate ())

    let finish_program program = 
        let terms =     
            program
            |> GuardTransform.gtransform_program
            |> function terms -> List.flatten (List.rev(List.map fterm terms))
        in
        
        (* Apply renaming *)
        let terms = List.map (T.apply_rename_term true (make_capitalize_renaming)) terms in 
        rename_collected_state (make_capitalize_renaming);  
       
        cstate := {
            event2receptionists;
            collected_components;
            guardian_components = ref Atom.Set.empty (* hydrated in AkkaJava.ml *)
        };

        { 
            T.entrypoint = [];
            T.system = ();
            T.terms = terms   
        }

    (*****************************************************)
    let displayed_pass_shortdescription = Printf.sprintf "Codegen: Runtime AST" 
    let displayed_ast_name = "Runtime AST"
    let show_ast = true
    let precondition program = program
    let postcondition program = program
    let apply_program = finish_program
end