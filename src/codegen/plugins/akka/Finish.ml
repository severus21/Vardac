open Core
open Utils
open AstUtils
open Easy_logging
open Fieldslib
open Misc

(* Function composition, TODO put it in some Core file*)
let plg_name = "Akka.Finish"
let plg_version = "0.0.1"

let fplace = (Error.forge_place ("plg."^plg_name^".Finish") 0 0) 
let auto_fplace smth = {place = fplace; value=smth}
include Ast.AstUtil2.Make(struct let fplace = fplace end)
module S_A2 = AstUtils2.Mtype.Make(struct let fplace = fplace end)
module T_A2 = Ast.AstUtil2.Make(struct let fplace = fplace end)

(* The source calculus. *)
module S = IRI 
(* The target calculus. *)
module T = Ast 



(*** Global state *)
type collected_state = {
    mutable target : Core.Target.target option;
    event2receptionists : (Atom.t list) Atom.AtomHashtbl.t; 
    external2receptionists : (Atom.t list) Atom.AtomHashtbl.t; 
    collected_components: Atom.Set.t ref;
    guardian_components: Atom.Set.t ref
}
 
let print_cstate cstate = 
    Format.fprintf Format.std_formatter "Cstate.event2receptionists\n";
    Atom.AtomHashtbl.iter (fun k v -> 
        Format.fprintf 
            Format.std_formatter "+ %s -> @[<hv>%a@]\n" 
            (Atom.to_string k) 
            (Error.pp_list ";" (fun out x-> Format.fprintf out "%s" (Atom.to_string x))) 
            v
    ) cstate.event2receptionists

let empty_cstate () : collected_state = {
    target = None;
    event2receptionists = Atom.AtomHashtbl.create 0;
    external2receptionists = Atom.AtomHashtbl.create 0;
    collected_components = ref Atom.Set.empty;
    guardian_components = ref Atom.Set.empty
}

module Make (Arg: sig val target:Target.target end) = struct
    let logger = make_log_of plg_name

    module GuardTransform = GuardTransform.Make()
    module FutureElim0 = FutureElim.Make()
    module FutureElim = Core.IRICompilationPass.Make(FutureElim0)

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
    let event2receptionists : Atom.t list Atom.AtomHashtbl.t = 
        Atom.AtomHashtbl.create 64
    let external2receptionists : Atom.t list Atom.AtomHashtbl.t = 
        Atom.AtomHashtbl.create 64
    let add_event_e2rs event component : unit = 
        let vs = 
            try
                Atom.AtomHashtbl.find event2receptionists event
            with Not_found -> []
        in
        Atom.AtomHashtbl.add event2receptionists event (component::vs)
    let add_external_e2rs name component : unit = 
        let vs = 
            try
                Atom.AtomHashtbl.find external2receptionists name 
            with Not_found -> []
        in
        Atom.AtomHashtbl.add external2receptionists name (component::vs)

    let rename_collected_state renaming = 
        let e2rs = Atom.AtomHashtbl.to_seq event2receptionists in
        let e2rs = List.of_seq (Seq.map (function (k,v) -> renaming k, List.map renaming v) e2rs) in

        (* Since we change the keys *)
        Atom.AtomHashtbl.reset event2receptionists; 
        Atom.AtomHashtbl.add_seq event2receptionists (List.to_seq e2rs);

        let e2rs = Atom.AtomHashtbl.to_seq external2receptionists in
        let e2rs = List.of_seq (Seq.map (function (k,v) -> renaming k, List.map renaming v) e2rs) in

        (* Since we change the keys *)
        Atom.AtomHashtbl.reset external2receptionists; 
        Atom.AtomHashtbl.add_seq external2receptionists (List.to_seq e2rs);

        ()
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
        methods:    (S.method0 plg_annotated) list; 
        eventdefs:  S.typedef list;
        states:     S.state list; 
        nested:     (S.component_dcl plg_annotated) list; 
        classes:    S.class_structure list;
        ports:      S.port list;
        eports:     S.eport list;
        outports:   S.outport list;
        others:     S.term list
    }

    let fresh_items_grp () = { 
        methods     = [];
        eventdefs   = [];
        states      = [];
        nested      = [];
        classes     = [];
        ports       = [];
        outports    = [];
        eports      = [];
        others      = [];
    }


    let group_cdcl_by (citems:  S.component_item list) : items_grps =
        let dispatch grp (citem: S.component_item) = match citem.value.v with
            | S.Contract _  -> raise (Core.Error.PlacedDeadbranchError  (citem.place, "Contract term should have been remove from AST by the cook pass and binded to a method"))
            | S.Include _   -> Core.Error.perror citem.place "Include is not yet supported in Akka plg"
            | S.Method  m   -> {grp with methods={v=m; plg_annotations=citem.value.plg_annotations}::grp.methods}
            | S.State f     -> {grp with states=f::grp.states}
            | S.Term{place; value={plg_annotations; v=S.Class cl}}    -> {grp with classes=cl::grp.classes}
            | S.Inport p    -> {grp with ports=p::grp.ports}
            | S.Eport p     -> {grp with eports=p::grp.eports}
            | S.Outport p   -> {grp with outports=p::grp.outports}
            (* Shallow search of Typealias, FIXME do we need deep search ?*)
            | S.Term {place; value={v=S.Component cdcl; plg_annotations}} -> {grp with nested={v=cdcl;plg_annotations = plg_annotations@citem.value.plg_annotations}::grp.nested}
            | S.Term {place; value={v=S.Typedef ({value=EventDef _;_} as edef)}} -> {grp with eventdefs=edef::grp.eventdefs}
            | S.Term t      -> {grp with others=t::grp.others}
        in

        let grp = (List.fold_left  dispatch (fresh_items_grp ()) citems) in 
            {
                methods     = (List.rev grp.methods);
                eventdefs   = (List.rev grp.eventdefs);
                states      = (List.rev grp.states) ; 
                nested      = (List.rev grp.nested) ; 
                classes     = (List.rev grp.classes) ; 
                ports       = (List.rev grp.ports) ; 
                eports      = (List.rev grp.eports) ; 
                outports    = (List.rev grp.outports) ; 
                others      = (List.rev grp.others)
            }

    (************************************* Base types ****************************)

    (************************************ Types **********************************)
    let rec finish_ctype parent_opt place : S._composed_type ->  T._ctype = function
        | S.TActivationRef mt -> T.TActivationRef (fmtype parent_opt mt) 
        | S.TObject x when Atom.is_builtin x -> Encode.encode_builtin_type place (Atom.value x)
        | S.TObject x -> T.TVar x 
        | S.TArrow (m1, m2) -> T.TArrow (
            fmtype parent_opt m1,
            fmtype parent_opt m2
        )

        | S.TVar x ->  begin
            (* Remove type alias introduced by *.vimpl if any *)
            match Hashtbl.find_opt typealias x with
            | None ->
                if Atom.is_builtin x then 
                    Encode.encode_builtin_type place (Atom.value x)
                else  T.TVar x
            | Some bb -> T.TBB (fbbterm parent_opt bb) 
        end
        | S.TFlatType ft -> begin match ft with  
            (* When using Tuyple, Map, ... we need object so for ease we box atomic type in objects everywhere *)
            | AstUtils.TActivationID -> T.Atomic "String"
            | AstUtils.TBool -> T.Atomic "Boolean"
            | AstUtils.TInt -> T.Atomic "Integer" (* 32bits *)
            | AstUtils.TLong -> T.Atomic "Long" (* 64bits *)
            | AstUtils.TFloat -> T.Atomic "Float"
            | AstUtils.TSessionID -> T.Atomic "UUID"
            | AstUtils.TRange -> T.Atomic "IntegerRange"
            | AstUtils.TStr -> T.Atomic "String"
            | AstUtils.TVoid -> T.Atomic "Void" 
            | AstUtils.TUUID -> T.Atomic "UUID" 
            | AstUtils.TWildcard -> T.Atomic "?"
            | AstUtils.TBottom -> T.Atomic "Object"
            | AstUtils.TPlace -> (t_varda_place place).value
            | AstUtils.TBLabel -> T.Atomic "LabelEvent"
            | AstUtils.TUnit -> T.Atomic "void"
        end
        | S.TArray mt -> T.TArray (fmtype parent_opt mt)
        | S.TFuture mt -> T.TFuture (fmtype parent_opt mt)
        | S.TDict (m1, m2) -> T.TMap (fmtype parent_opt m1, fmtype parent_opt m2)
        | S.TList mt -> T.TList (fmtype parent_opt mt)
        | S.TOption mt -> T.TOption (fmtype parent_opt mt)
        | S.TResult (m1, m2) -> T.TResult (fmtype parent_opt m1, fmtype parent_opt m2)
        | S.TSet mt -> T.TSet (fmtype parent_opt mt)
        | S.TTuple mts ->  T.TTuple (List.map (fun x -> (fmtype parent_opt x)) mts)
        | S.TVPlace mt -> (t_varda_vplace place).value
        | S.TBridge b -> (t_varda_bridge place).value
        | S.TUnion _-> T.TRaw "Object" (* TODO maybe a better solution*)
        | S.TForall _ -> T.TUnknown (* TODO maybe encode it as class <T> ... { <T> } *)
        | S.TPolyVar _ -> T.TUnknown (* TODO maybe encode it as class <T> ... { <T> } *)
        | S.TOutport mt -> 
            logger#warning "TODO TOutport parameter type is not yet encoded in Java";
            T.TRaw "OutPort"
        | S.TInport mt -> 
            logger#warning "TODO TInport parameter type is not yet encoded in Java";
            T.TRaw "InPort"
        | S.TInductive mts -> T.TTuple (List.map (fun x -> (fmtype parent_opt x)) mts)
    and fctype parent_opt ct :  T.ctype = map_place (finish_ctype parent_opt) ct

    (* Represent an ST object in Java type *)
    and finish_stype parent_opt place : S._session_type -> T._ctype = function 
        | S.STEnd -> T.TVar (Atom.builtin "Protocol.STEnd") 
        | (S.STSend (mt, st) as st0) | (S.STRecv (mt, st) as st0) -> begin 
            let ct = fmtype parent_opt mt in 
            T.TParam (
                {
                    place;
                    value =  T.TVar (Atom.builtin (match st0 with | S.STSend _ -> "Protocol.STSend" | STRecv _ -> "Protocol.STRecv"))
                },
                [ct; fstype parent_opt st]
            )
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
                            [fstype parent_opt st]
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
                [ fstype parent_opt st]
            )

        | S.STInline x -> 
            raise (Error.PlacedDeadbranchError (place, "STInline should remains outside the codegen part, it should have been resolve during the partial evaluation pass."))
    and fstype parent_opt st : T.ctype = map_place (finish_stype parent_opt) st

    (* Represent an ST object in Java value *)
    and finishv_stype parent_opt place : S._session_type -> T._expr * T.ctype = 
    (****** Helpers *****)
    let fplace = place@(Error.forge_place "Plg=Akka/finishv_stype" 0 0) in
    let auto_place smth = {place = fplace; value=smth} in

    let rec encode_guard_header_ place = function
    | S.UseMetadata _ -> failwith "UseMetadata/Global not yet supported by Akka"
    | S.SetTimer _ -> raise (Error.PlacedDeadbranchError (place, "SetTimer should have been replace by SetFireTimer before the Akka.Finish since Akka needs a delay value to create a timer"))
    | S.SetFireTimer (x, i) -> e_ASTStype_TimerHeader_of place x i 
    and encode_guard_header header : T.expr = encode_guard_header_ header.place header.value in 



    function 
        | S.STEnd -> T.NewExpr (e2var (a_ASTStype_of "End"), []), auto_place T.TUnknown
        (* With guard *)
        | (S.STSend ({place=p_mt; value=S.ConstrainedType (mt, (guard_headers, guard_opt))}, st) as st0) | (S.STRecv ({place=p_mt; value=S.ConstrainedType (mt, (guard_headers, guard_opt))}, st) as st0) -> begin 

            let encoded_headers = List.map encode_guard_header guard_headers in
            (* TODO remove timer binop from guard_opt -> fully manage by through header in Akka *)

            match fmtype parent_opt mt with
            | ct ->
                let constructor = auto_place ((match st0 with 
                | S.STSend _ -> T.VarExpr (a_ASTStype_of "Send")
                | STRecv _ -> T.VarExpr (a_ASTStype_of "Receive")
                ), auto_place T.TUnknown) in

                T.NewExpr (
                    constructor,
                    [
                        e_ASTStype_MsgT_of place (
                            e2_e (T.CallExpr(
                                e2_e (T.AccessExpr(
                                    e2_e (T.AccessExpr (Ast.encodectype ct, e2var (Atom.builtin "class"))),
                                    e2_e (T.RawExpr "toString")
                                )),
                                []
                            ))
                        );
                        e2_e (Encode.encode_list place encoded_headers);  
                        fvstype parent_opt st
                    ]
                ), auto_place T.TUnknown 
            | _ -> raise (Core.Error.PlacedDeadbranchError (mt.place, "finish_stype : STSend/STRecv type should not be a session type."))
        end

        (* Without guard *)
        | (S.STSend (mt, st) as st0) | (S.STRecv (mt, st) as st0) -> begin 
            match fmtype parent_opt mt with
            | ct ->
                let constructor = auto_place ((match st0 with 
                | S.STSend _ -> T.VarExpr (a_ASTStype_of "Send")
                | STRecv _ -> T.VarExpr (a_ASTStype_of "Receive")
                ), auto_place T.TUnknown) in

                T.NewExpr (
                    constructor,
                    [
                        e_ASTStype_MsgT_of place (
                            e2_e (T.CallExpr(
                                e2_e (T.AccessExpr(
                                    e2_e (T.AccessExpr (Ast.encodectype ct, e2var (Atom.builtin "class"))),
                                    e2_e (T.RawExpr "toString")
                                )),
                                []
                            ))
                        );
                        e2_e (Encode.encode_list place []);  
                        fvstype parent_opt st
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
                    e2_e (T.BlockExpr (
                        AstUtils.List,
                        List.map (function (label, st, _) -> e2_e (T.BlockExpr (
                            AstUtils.Tuple, 
                            [ 
                                e_ASTStype_MsgT_of place (
                                    e2_lit (T.StringLit (Atom.to_string label)) 
                                );
                                e2_e (Encode.encode_list place []);  
                                fvstype parent_opt st 
                            ]
                        ))) xs
                    ))
                ]
            ), auto_place T.TUnknown
        | S.STVar _ -> failwith "Not yet supported" 
        | S.STRec (_,st) -> failwith "Not yet supported"
        | S.STInline x -> 
            raise (Error.PlacedDeadbranchError (place, "STInline should remains outside the codegen part, it should have been resolve during the partial evaluation pass."))
        | S.STWildcard -> 
            (* FIXME TODO WARNING dirty hack since we do not have working mgu to unify receive bridge type with concrete type
            then temporaly we return STEnd encoding *)
            T.NewExpr ( e2var (a_ASTStype_of "End"), []), auto_place T.TUnknown
            (* The correct behaviour is *)
            (* raise (Error.PlacedDeadbranchError (place, "STWildcard should have been concretized during type inference.")) *)
    and fvstype parent_opt st : T.expr = map_place (finishv_stype parent_opt) st

    and finish_struct_type place : S._struct_type -> T._ctype = function
    | S.CompTUid x -> T.TVar x 
    | S.TStruct (x, _) -> 
        (* Structural types can not be encoded easily in Java *)
        T.TVar x
    | S.TPolyCVar x -> Error.perror place "TPolyCVar should have been reduce before reaching Akka ???"
    and fcctype ct : T.ctype = map_place finish_struct_type ct

    and finish_mtype parent_opt place : S._main_type -> T.ctype = 
    let fplace = place@(Error.forge_place "Plg=Akka/finish_mtype" 0 0) in
    function
    | S.CType ct -> fctype parent_opt ct 
    (* TODO FIXME URGENT
            Type de session dans Akka 
            actuellement encapsuler dans le protocol pb pour la creation apr??s
            puisque dans le language on utilise le protocol que pour le initiate_sesion et les autres types de sessions sont ind??pendants du protocol.
    *)
    | S.SType {value=S.STInline _} -> t_varda_session place (* FIXME x not used *)
    | S.SType st -> {
        place; 
        value = T.TParam (
            t_varda_session fplace,
            [ ] (* FIXME at this point we do not parametrize session with session types since 
                    do not compile yet
                    if we can exhange session by message-passing Java loose the generic parameter types dynamically 
                    [fstype parent_opt st] *)
        )
    }
    | S.CompType ct -> fcctype ct
    | S.ClType ct -> fcctype ct
    | S.EmptyMainType -> {place; value=T.TUnknown}
    | S.TRaw x -> {place; value=T.TRaw x}
    and fmtype parent_opt : S.main_type ->  T.ctype = map0_place (finish_mtype parent_opt)

    (************************************ Literals *****************************)

    and finish_literal place : S._literal -> T._literal = function
        | S.VoidLit -> T.VoidLit
        | S.BoolLit b -> T.BoolLit b
        | S.FloatLit f -> T.FloatLit f
        | S.IntLit i -> T.IntLit i
        | S.LabelLit l -> Core.Error.perror place "Label are not yet supported"
        | S.StringLit str -> T.StringLit str

        | S.ActivationRef _ -> failwith "Activation info is not yet supported"

        | S.Place _ -> failwith "Place is not yet supported"
        | S.StaticBridge _ -> raise (Error.DeadbranchError "Bridge should have been process by the finish_expr (returns an expr)")
        | x -> failwith (S.show__literal x)
    and fliteral lit : T.literal = map_place finish_literal lit

    (************************************ Expr & Stmt *****************************)

    and fbinop = function
    | AstUtils.And                 -> T.And    
    | AstUtils.Or                  -> T.Or 
    | AstUtils.Plus                -> T.Plus
    | AstUtils.Minus               -> T.Minus
    | AstUtils.Mult                -> T.Mult
    | AstUtils.Divide              -> T.Divide
    | AstUtils.Equal               -> T.StructuralEqual 
    | AstUtils.NotEqual            -> T.NotStructuralEqual 
    | AstUtils.GreaterThanEqual    -> T.GreaterThanEqual
    | AstUtils.LessThanEqual       -> T.LessThanEqual
    | AstUtils.GreaterThan         -> T.GreaterThan
    | AstUtils.LessThan            -> T.LessThan
    | AstUtils.In                  -> T.In

    and finish_expr parent_opt place (e, mt): T._expr * T.ctype =
    let fplace = place@(Error.forge_place "Plg=Akka/finish_expr" 0 0) in
    let auto_place smth = {place = fplace; value=smth} in
    (match e with
        | S.VarExpr x -> T.VarExpr x
        | S.AccessExpr (e1, {value=S.VarExpr x, _}) when Atom.is_builtin x -> Encode.encode_builtin_access place (fexpr parent_opt e1) (Atom.value x)
        | S.AccessExpr (e1, e2) -> T.AccessExpr (fexpr parent_opt e1, fexpr parent_opt e2)
        | S.BinopExpr (t1, op, t2) -> T.BinopExpr (fexpr parent_opt t1, fbinop op, fexpr parent_opt t2)
        | S.CastExpr (mt, e) -> T.CastExpr (fmtype parent_opt mt, fexpr parent_opt e)
        | S.LambdaExpr (params, e) -> 
            T.LambdaExpr (
                List.map (map0_place (fun _ (mt, x) -> fmtype parent_opt mt, x)) params,
                auto_place (T.ReturnStmt (fexpr parent_opt e))
            ) 
        | S.NewBridge {protocol_opt; protocol_name_opt} -> begin
            match protocol_opt, protocol_name_opt with
            | Some protocol, None ->
                fst (e_bridge_of_protocol place (fexpr parent_opt protocol)).value 
            | None, Some protocol_name ->
                fst (e_bridge_of_protocol 
                    place 
                    (T_A2.e2_e (Ast.NewExpr ( 
                        (T_A2.e2_e (T.VarExpr protocol_name)),
                        []
                    ))
                )).value 
            | None, None | Some _, Some _ -> raise (Error.DeadbranchError "NewBridge wrong protocol")
        end
        | S.LitExpr {value=S.BLabelLit l; place=lit_place} -> 
            T.NewExpr( 
                e2_e (T.RawExpr "LabelEvent"), 
                [ e2_lit (T.StringLit (Atom.to_string l))]
            )
        | S.LitExpr {value=S.StaticBridge b; place=lit_place} -> 
            fst (e_static_bridge_of_protocol place (e2var b.protocol_name) b.id).value
        | S.LitExpr {value=S.VPlace vp} -> begin 
            T.CallExpr (
                e2var (Atom.builtin "VPlaces.get"),
                [
                    e2_lit (T.StringLit (Atom.hint vp.name))
                ]
            )
        end
        | S.LitExpr lit -> T.LitExpr (fliteral lit)
        | S.UnopExpr (op, e) -> T.UnopExpr (op, fexpr parent_opt e)

        | S.CallExpr (e1, es) -> begin 
            match e1.value with 
            | S.VarExpr x,_ when Atom.is_builtin x ->
                Encode.encode_builtin_fct parent_opt e1.place (Atom.value x) (List.map (fexpr parent_opt) es)
            | _ -> T.CallExpr (fexpr parent_opt e1, List.map (fexpr parent_opt) es)
        end
        | S.NewExpr (e1, es) -> begin 
            match e1.value with 
            | S.VarExpr x,_ when Atom.is_builtin x ->
                Encode.encode_builtin_fct parent_opt e1.place (Atom.value x) (List.map (fexpr parent_opt) es)
            | _ -> T.NewExpr (fexpr parent_opt e1, List.map (fexpr parent_opt) es)
        end
        | S.RawExpr x -> T.RawExpr x
        | S.This | S.Self -> T.This
        | S.Create c -> 
            T.NewExpr (auto_fplace(T.VarExpr c.c, auto_fplace (T.TVar c.c)), List.map (fexpr parent_opt) c.args)
        | S.Spawn {c; args; at=None} ->
            T.ActivationRef{
                schema = e2_lit (T.StringLit (Atom.to_string (IRMisc.schema_of c)));
                actor_ref = e2_e (T.Spawn {
                    context = e2_e T.CurrentContext;  
                    actor_expr= e2_e (T.CallExpr(
                        e2var (Atom.builtin "spawn"),
                        [ e2_e (T.CallExpr (
                            e2_e (T.AccessExpr (
                                    fcexpr parent_opt c,
                                    e2var (Atom.builtin "create")
                                )),
                                e_this_guardian (this_actor parent_opt) fplace
                                :: List.map (fexpr parent_opt) args
                            )
                        )] @ [ 
                            e2_e(T.CallExpr(
                                e2_e(T.RawExpr "ActivationRef.unique_actor_name"),
                                [ e2_lit (T.StringLit (Atom.to_string (Atom.fresh ((Atom.to_string (IRMisc.schema_of c))^"_name"))))]
                            ))
                        ]
                    ))
                })
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
            let a_context = Atom.fresh "ccontext" in
            let a_guardian = Atom.fresh "gguardian" in
            let a_timers = Atom.fresh "ttimers" in
            let schema = IRMisc.schema_of c in

            (* TODO ?? DUplicated with AkkaJAva [arg_lambda] ?? *)
            let runnable = 
            e2_e (T.LambdaExpr (
                [auto_place T.TUnknown, a_guardian],
                auto_place (T.BlockStmt [
                    auto_place (T.ReturnStmt (
                        e2_e (T.LambdaExpr (
                            [
                                (* ActorContext<author.project_name.a14.C33.Command> *)
                                auto_place (
                                    T.TParam(
                                        t_context place,
                                        [
                                            match (fmtype parent_opt mt).value with
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
                                    e2_e(T.CallExpr(
                                        e_behaviors_with_timers fplace,
                                        [
                                            e2_e (T.LambdaExpr (
                                                [auto_place T.TUnknown, a_timers],
                                                auto_place (T.BlockStmt [
                                                    auto_place (T.ExpressionStmt (
                                                    e_debug_of 
                                                        place 
                                                        (e2var a_context) 
                                                        [
                                                            e2_lit (T.StringLit ("SpawnAT::create"));
                                                        ]
                                                    ));
                                                    auto_place (T.ReturnStmt (
                                                        e2_e (T.NewExpr (
                                                            fcexpr parent_opt c,
                                                            (
                                                                List.map (function x -> e2var x) (a_context
                                                                ::a_timers
                                                                ::a_guardian
                                                                ::[]
                                                            )
                                                            @(List.map (fexpr parent_opt) args))

                                                        ))
                                                    ));
                                                ])
                                            ))
                                        ]
                                ))))
                            ])
                        ))
                    ))
                ])
            )) in
        
            T.ActivationRef {
                schema = e2_lit (T.StringLit (Atom.to_string (IRMisc.schema_of c)));
                actor_ref = 
                e2_e (T.CallExpr(
                    e_varda_spawnat fplace,
                    [
                        e_get_context place;
                        e_this_guardian (this_actor parent_opt) fplace;
                        runnable;
                        e2_lit (T.StringLit (Atom.to_string (Atom.fresh "actor_name")));
                        e2_lit T.VoidLit;
                        fexpr parent_opt at;
                    ]
                ))
            }
        | S.BoxCExpr _ -> failwith "finish_expr BoxCexpr is not yet supported"
                
        | S.OptionExpr None -> T.CallExpr (
            e2var (Atom.builtin "SerializableOptional.empty"),
            []
        )  
        | S.OptionExpr (Some e) -> T.CallExpr (
            e2var (Atom.builtin "SerializableOptional.of"),
            [fexpr parent_opt e]
        )  
        | S.ResultExpr (None, Some err) ->  T.CallExpr (
            e2var (Atom.builtin "Either.left"),
            [fexpr parent_opt err]
        ) 
        | S.ResultExpr (Some ok, None) -> T.CallExpr (
            e2var (Atom.builtin "Either.right"),
            [fexpr parent_opt ok]
        ) 
        | S.ResultExpr (_,_) -> raise (Core.Error.PlacedDeadbranchError (place, "finish_expr : a result expr can not be Ok and Err at the same time."))
        | S.BlockExpr (b, es) -> T.BlockExpr(b, List.map (fexpr parent_opt) es)
        | S.Block2Expr (b, ees) -> T.Block2Expr(b, List.map (function (e1, e2) -> fexpr parent_opt e1, fexpr parent_opt e2) ees) 
        | S.InterceptedActivationRef (e1, e2_opt, intercepted_schema) -> 
            T.InterceptedActivationRef{
                actor_ref = fexpr parent_opt e1;
                intercepted_actor_ref = Option.map (fexpr parent_opt) e2_opt;
                intercepted_schema
            }

        | S.TernaryExpr (e1, e2, e3) -> T.TernaryExpr (fexpr parent_opt e1, fexpr parent_opt e2, fexpr parent_opt e3) 
    ), fmtype parent_opt mt
    and fexpr parent_opt e : T.expr = map_place (finish_expr parent_opt) e

    and finish_stmt parent_opt place : S._stmt -> T._stmt = 
    let fplace = place@(Error.forge_place "Plg=Akka/finish_stmt" 0 0) in
    let auto_place smth = {place = fplace; value=smth} in
    function
        | S.EmptyStmt -> T.CommentsStmt (AstUtils.LineComment "Empty Statement")

        (*S.* Binders *)
        | S.AssignExpr (x, e) -> T.AssignExpr (auto_place (T.VarExpr x, auto_place T.TUnknown), fexpr parent_opt e)
        | S.AssignThisExpr (x, e) | S.AssignSelfExpr (x, e) -> 
            T.AssignExpr ( 
                e2_e (T.AccessExpr (
                    e2_e T.This,
                    e2var x
                )),
                fexpr parent_opt e)        
        | S.LetStmt (mt, x, e) ->  
            (* 
                Tmp fix, since right handside type of a let is never a TUnknown we use it for e;
                Once e= (_, not unknown) we could remove the following line
                FIXME    
            *)
            let e = {e with value = (fst e.value, mt)} in
            T.LetStmt (fmtype parent_opt mt, x, Some (fexpr parent_opt e))                             

        (*S.* Comments *)
        | S.CommentsStmt comments -> T.CommentsStmt comments.value

        (*S.* Control flow *)
        | S.BreakStmt -> T.BreakStmt
        | S.ContinueStmt -> T.ContinueStmt
        | S.ExitStmt _ -> failwith "Exist is not yet supported"
        | S.ForeachStmt (mt,x,e,stmt) -> T.ForeachStmt(fmtype parent_opt mt, x, fexpr parent_opt e, fstmt parent_opt stmt)
        | S.IfStmt (e, s1, s2_opt) -> T.IfStmt (fexpr parent_opt e, fstmt parent_opt s1, Option.map (fstmt parent_opt) s2_opt)
        | S.MatchStmt (_,_) -> Core.Error.perror place "Match is not yet supported"

        (*S.*type name, type definition*)
        | S.ExpressionStmt {value=S.CallExpr({value=S.VarExpr x, _}, args), _} 
        when Atom.is_builtin x && Encode.is_stmt_builtin (Atom.hint x) -> 
            Encode.encode_builtin_fct_as_stmt false place (Atom.value x) (List.map (fexpr parent_opt) args)
        | S.ReturnStmt {value=S.CallExpr({value=S.VarExpr x, _}, args), _}
        when Atom.is_builtin x && Encode.is_stmt_builtin (Atom.hint x) -> 
            Encode.encode_builtin_fct_as_stmt true place (Atom.value x) (List.map (fexpr parent_opt) args)
        | S.ReturnStmt e -> T.ReturnStmt (fexpr parent_opt e) 
        | S.ExpressionStmt e -> T.ExpressionStmt (fexpr parent_opt e) 
        | S.BlockStmt stmts -> T.BlockStmt (List.map (fstmt parent_opt) stmts)
        
        | S.GhostStmt _ -> raise (Core.Error.PlacedDeadbranchError (place, "finish_stype : GhostStmt should have been remove by a previous compilation pass."))
    and fstmt parent_opt stmt : T.stmt = map_place (finish_stmt parent_opt) stmt

    and finish_eventdef parent_opt (inner_place:Error.place) (name, mts, body) : T.event =
    match body with  
    | None -> { 
                place = inner_place; 
                value = {
                    T.vis=T.Public; 
                    name= name;
                    args=List.mapi ( fun i mt ->
                        fmtype parent_opt mt, Atom.builtin ("value"^(string_of_int i))
                    ) mts;
                    headers = [];
                }
            }

    and finish_function parent_opt place : S._function_dcl -> T.method0 list = function
        | f ->
            let body = match f.body with
                | S.AbstractImpl stmts -> T.AbstractImpl (List.map (fstmt parent_opt) stmts)
                | S.BBImpl bbterm -> 
                    T.BBImpl { 
                        place = bbterm.place; 
                        value = {
                            language=bbterm.value.language; 
                            body = (List.map (function
                                | S.Text t -> T.Text t 
                                | S.Varda e -> T.Varda (fexpr parent_opt e)
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
                        ret_type        = fmtype parent_opt f.ret_type;
                        name            = f.name;
                        args            = (List.map (fparam parent_opt) f.args);
                        throws          = [];
                        body            = body; 
                        is_constructor  = false 
                    }
                }
            } in

            [new_function]
    and ffunction parent_opt : S.function_dcl -> T.method0 list = function m -> finish_function parent_opt m.place m.value
    (************************************ Component *****************************)


    (* return type is T._expr for now, since we built only one state with all the variable inside FIXME *)
    and finish_state parent_opt place : S._state -> T._stmt = 
    let fplace = (Error.forge_place "Plg=Akka/finish_state" 0 0) in
    let auto_place smth = {place = fplace; value=smth} in
    function 
        | {ghost; type0; name; body = S.InitExpr e} -> 
            T.LetStmt (fmtype parent_opt type0, name, Some (fexpr parent_opt e))
        | {ghost; type0; name; body = S.InitBB bb_term} -> 
            T.LetStmt (
                fmtype parent_opt type0, 
                name, 
                Some {
                    place = bb_term.place;
                    value = T.BBExpr (fbbterm parent_opt bb_term), auto_place T.TUnknown
                })
        | { ghost; type0; name; body = S.NoInit} ->
            T.LetStmt (fmtype parent_opt type0, name, None)
    and fstate parent_opt s : T.stmt = map_place (finish_state parent_opt) s


    and finish_param parent_opt place : S._param -> (T.ctype * T.variable) = function
    | mt, x -> fmtype parent_opt mt, x
    and fparam parent_opt : S.param -> (T.ctype * T.variable) = function p -> finish_param parent_opt p.place p.value


    and finish_contract parent_opt place (method0 : T.method0) (contract : S._contract) : T.method0 list =
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
        let with_params = List.map (function (x,y,_) -> finish_param parent_opt place (x,y)) contract.pre_binders in
        let with_stmts : S.stmt list = List.map (function (x,y,z) -> {place; value=S.LetStmt (x,y,z)}) contract.pre_binders in 
        let with_stmts : T.stmt list = List.map (fstmt parent_opt) with_stmts in
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
                            value = T.ReturnStmt (fexpr parent_opt ensures_expr)
                        }]);
                        args            = ensures_params;
                        throws          = [];
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
        | None -> 
            [], [
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
                            {place = fplace; value= T.ReturnStmt (
                                {place = fplace; value=T.CallExpr(
                                    fexpr parent_opt returns_expr,
                                    [
                                        begin    
                                            let x = Atom.fresh "x" in
                                            match method0.value.v.ret_type.value with   
                                            | T.TRaw "ResolvedResult" -> 
                                                e2_e (T.CallExpr(
                                                    e2_e (T.AccessExpr (
                                                        T_A2.e2_e (T.AccessExpr(
                                                            {place = returns_expr.place; value=T.VarExpr (snd ret_type_param), auto_place T.TUnknown},
                                                            T_A2.e2_e (T.RawExpr "getValue()")
                                                        )),
                                                        e2_e (T.RawExpr "map")
                                                    )),
                                                    [
                                                        e2_e (T.LambdaExpr (
                                                            [ (auto_place (T.Atomic "Object"), x) ],
                                                            auto_place(T.ReturnStmt(
                                                                e2_e (T.CastExpr (
                                                                    (match fst returns_expr.value with 
                                                                    | S.LambdaExpr( {value=({value=S.CType {value=S.TResult (mt1, _)}},_)}::_, _) -> 
                                                                        fmtype parent_opt mt1
                                                                    | _ -> 
                                                                        raise (Error.DeadbranchError "wrong post-condition type") 
                                                                    ),
                                                                    e2var x 
                                                                ))
                                                            ))
                                                        ))
                                                    ]
                                                ))


                                            | _ -> 
                                                {place = returns_expr.place; value=T.VarExpr (snd ret_type_param), auto_place T.TUnknown}
                                        end
                                    ]), auto_place T.TUnknown
                                })
                            }
                        ]);
                        args            = returns_params;
                        throws          = [];
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
                    throws          = [];
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
    and fcontract parent_opt actor_name (method0 : T.method0) : S.contract -> T.method0 list = function m -> finish_contract parent_opt m.place method0 m.value
        
    and finish_method parent_opt actor_name plg_annotations place (m0 : S._method0) : T.method0 list = 
        (* TODO actor_name is include in parent_opt, actor_name can be removed *)
        assert( false = m0.on_destroy); (* TODO not yet supported*)

        let decorators, annotations = List.split (List.map (map0_place (function place -> function
            | T.AOverride ->  [T.Override], []
            | T.AExtends _ | T.AImplements _ -> Error.perror place "extends or implements plg_annotations are not supported for [target=akka] methods"
        )) plg_annotations) in
        let decorators = List.flatten decorators in
        let annotations = List.flatten annotations in

        let body = match m0.body with
            | S.AbstractImpl stmts -> T.AbstractImpl (List.map (fstmt parent_opt) stmts)
            | S.BBImpl body -> T.BBImpl (fbbterm parent_opt body)
        in

        let new_method : T.method0 = {
            place;
            value = { 
                decorators      = decorators; 
                annotations     = [ T.Visibility T.Public ] @ annotations; 
                v = {
                    ret_type        = fmtype parent_opt m0.ret_type;
                    name            = if m0.on_startup then actor_name else m0.name;
                    args            = (List.map (fparam parent_opt) m0.args);
                    throws          = [];
                    body            = body; 
                    is_constructor  = m0.on_startup 
                }
            }
        } in

        begin
            match m0.contract_opt with
            | None -> [new_method]
            | Some contract -> fcontract parent_opt actor_name new_method contract 
        end
    and fmethod parent_opt actor_name {v; plg_annotations} = map0_place (finish_method parent_opt actor_name (fplgannot plg_annotations)) v
    and finish_bbterm parent_opt place {S.language; body} = 
    {
        T.language;
        body = List.map (
            function 
            | S.Text t -> T.Text t
            | S.Varda e -> T.Varda (fexpr parent_opt e)
        ) body
    }
    and fbbterm parent_opt bbterm: T.blackbox_term = map_place (finish_bbterm parent_opt) bbterm

    and finish_plgannot_component  a plg_annotations =
        let a, res = List.fold_left_map (fun (a:T.actor) -> map0_place (function place -> function
            | T.AOverride -> Error.perror place "Component can not be overrided !"
            | AExtends x -> begin
                match a.value.extends with 
                | None -> 
                {a with 
                value = {a.value with T.extends = Some (auto_fplace (T.Atomic x))}},  ([], [])
                | Some mt -> Error.perror (mt.place@place)"Multiple heritage is forbidden in Java!"
            end
            | AImplements x ->
                {a with 
                value = {a.value with implemented_types = (auto_fplace (T.Atomic x))::a.value.implemented_types}}, ([],[])
        )) a plg_annotations in
        let (annotations, decorators) = List.split res in
        let annotations = List.flatten annotations in
        let decorators = List.flatten decorators in
        a, annotations, decorators

       
    and finish_class_item parent_opt cl_name place plg_annotations = function
    | S.CLMethod m -> List.map (function m -> {T.annotations = []; decorators = []; v = T.MethodDeclaration m}) (fmethod parent_opt cl_name {plg_annotations=plg_annotations; v=m})
    | S.CLState s -> [ {T.annotations = []; decorators = []; v=T.Stmt (fstate parent_opt s)}]
    and fclass_item parent_opt cl_name = map_places (map0_plgannot(finish_class_item parent_opt cl_name))

    and fclass parent_opt (cl:S.class_structure) = 
    let parent_opt = match parent_opt with | _, pself_opt -> Some cl.name, pself_opt in
        auto_fplace ({T.annotations = []; decorators = []; v =T.ClassOrInterfaceDeclaration {
            isInterface = false;
            name = cl.name;
            extends = None;
            implemented_types = [];
            body = List.flatten(List.map (fclass_item parent_opt cl.name) cl.body);
            headers = [];
        }})


    and finish_component_dcl parent_opt place : S._component_dcl -> T.actor list = function
    | S.ComponentStructure {name; body; headers} -> begin 
        let parent_opt = (None, Some name) in

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
            match x.value.v with
            | S.Typedef _ -> ()
            | S.Comments _ -> () (* TODO needs to get ride of grp_items to support comments at the right place *)
            | S.BBTerm _ -> ()
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
                finish_eventdef parent_opt place (name, mts, body)
        ) grp_items.eventdefs in
        (* 
            List.flatten (List.map (function x -> snd (fmtype parent_opt x)) (List.filter_map (function x -> match snd x with |S.AbstractTypealias mt -> Some mt | _ -> None) (List.map (function |{value=S.EventDef (x, mts,body); _} -> (x, mts, body)) grp_items.eventdefs))) in*)
        

        (*** Building states ***)
        let states : T.state list = List.map (
            function state -> {
                place = state.place; 
                value = {T.persistent=false; stmts= [fstate parent_opt state] }
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
                    Some (e2_lit (T.StringLit (Atom.to_string name)))
                ))]
            };
            (* Set<UUID> frozen_sessions = new HashSet();*)
            auto_place {   T.persistent = false; (*TODO persistence True ??*)
                stmts = [ auto_place(T.LetStmt (
                    auto_place (T.TSet(auto_place( T.Atomic "UUID"))),
                    a_frozen_sessions,
                    Some (e2_e (T.BlockExpr(Core.AstUtils.Set, [])))
                ))]
            };
            (* Set<UUID> dead_sessions = new HashSet() *)
            auto_place {   T.persistent = false;(*TODO persistence True ??*)
                stmts = [ auto_place(T.LetStmt (
                    auto_place (T.TSet(auto_place( T.Atomic "UUID"))),
                    a_dead_sesison,
                    Some ( e2_e(T.BlockExpr(Core.AstUtils.Set, [])))
                ))]
            };
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
                        Some (
                            e2_e (T.NewExpr(
                                e2_e (T.RawExpr "OutPort"),
                                [
                                    fexpr parent_opt (S_A2.e2_lit (S.StringLit (Atom.to_string _p.name)));
                                    fexpr parent_opt (S_A2.e2_e (S.BlockExpr(AstUtils.List, 
                                        List.map (function name -> 
                                            S_A2.e2_e (S.AccessExpr( S_A2.e2_e S.This, S_A2.e2var name))
                                        ) _p._children
                                    )));
                                    fvstype parent_opt (match _p.protocol.value with
                                    | S.SType st -> st);
                                ]
                            ))    
                        )
                    ))]
                }
            ) grp_items.outports
            @ states 
        in

        (*** Inports ***)
        let states = 
            List.map (function p -> 
                let _p : S._port = fst p.value in
                let t_msg, t_cont, t_ret = match snd _p.callback.value with
                    | {value=S.CType {value=S.TArrow (t_msg, {value=S.CType{value=S.TArrow (t_cont, t_ret)}})}} -> t_msg, t_cont, t_ret 
                    | mt -> raise (Error.PlacedDeadbranchError ( _p.callback.place, (Printf.sprintf "Callback of port %s is ill-typed %s" (Atom.to_string _p.name) (S.show_main_type mt))))
                in

                auto_place {   
                    T.persistent = false; (*TODO persistence True ??*)
                    stmts = [ auto_place(T.LetStmt (
                        auto_place (T.TParam(
                            auto_place( T.Atomic "InPort"),
                            [
                                fmtype parent_opt t_msg;
                                fmtype parent_opt t_cont;
                                fmtype parent_opt t_ret;
                            ]
                        )),
                        _p.name,
                        Some (
                            e2_e (T.NewExpr(
                                e2_e (T.RawExpr "InPort"),
                                [
                                    fexpr parent_opt (S_A2.e2_lit (S.StringLit (Atom.to_string _p.name)));
                                    fexpr parent_opt (S_A2.e2_e (S.BlockExpr(AstUtils.List, 
                                        List.map (function name -> 
                                            S_A2.e2_e (S.AccessExpr( S_A2.e2_e S.This, S_A2.e2var name))
                                        ) _p._children
                                    )));
                                    fexpr parent_opt (S_A2.e2_lit (S.BoolLit _p._is_intermediate));
                                    fvstype parent_opt (match _p.expecting_st.value with
                                    | S.SType st -> st);
                                    fexpr parent_opt (match _p._receive_id with
                                        | None -> S_A2.e2_e (S.OptionExpr None)
                                        | Some x -> S_A2.e2_e (S.OptionExpr (Some (S_A2.e2_lit (S.StringLit (Atom.to_string x)))))
                                    );
                                ]
                            ))    
                        )
                    ))]
                }
            )  (List.filter (function (p:S.port) -> Bool.not ((fst p.value)._disable_session)) grp_items.ports)
            @ states 
        in

        let hydrate_state = 
            List.map (function p -> 
                let _p : S._port = fst p.value in
            
                auto_place (T.ExpressionStmt (
                    T_A2.e2_e (T.CallExpr(
                        T_A2.e2_e (T.AccessExpr(
                            T_A2.e2_e (T.AccessExpr(
                                T_A2.e2_e T.This,
                                T_A2.e2var _p.name
                            )),
                            T_A2.e2_e (T.RawExpr "setCallback")
                        )),
                        [ 
                            match (fexpr parent_opt (fst p.value).callback) with 
                            | {place; value=T.AccessExpr (a, {value=T.VarExpr b,_}), mt} -> {place=place@fplace; value=T.AccessMethod (a, b), mt}
                            | e -> e 
                        
                        ]
                    ))
                ))
            ) (List.filter (function (p:S.port) -> Bool.not (fst p.value)._disable_session) grp_items.ports)
        in

        (*** Eports 
            In Akka, eports messages are events received by the actor that boxes the value    
        ***)
        let states = 
            List.map (function p -> 
                let _p : S._eport = fst p.value in
                auto_place {   
                    T.persistent = false; (*TODO persistence True ??*)
                    stmts = [ auto_place(T.LetStmt (
                        auto_place( T.Atomic "EPort"),
                        _p.name,
                        Some (
                            e2_e (T.NewExpr(
                                e2_e (T.RawExpr "EPort"),
                                [
                                    fexpr parent_opt (S_A2.e2_lit (S.StringLit (Atom.to_string _p.name)));
                                    match _p.expecting_mt.value with
                                    | CType {value=TVar x} -> T_A2.e2_e (T.ClassOf (fmtype parent_opt _p.expecting_mt))
                                    | _ -> Error.perror place "unsupported expecte type for eport"
                                ]
                            ))    
                        )
                    ))]
                }
            ) grp_items.eports
            @ states 
        in


        (*** Building receiver ***)
        (* Step0 - name of receiver param (event) *)
        let l_event_name : Atom.atom = (Atom.fresh "e") in
        let l_event : T.expr = auto_place (T.VarExpr l_event_name, auto_place T.TUnknown) in

        (* Step 1bis - create {external_event: eport} *)
        let external_env : S.eport Atom.AtomHashtbl.t = Atom.AtomHashtbl.create 16 in
        let hydrate_external_env (p:S.eport) = 
            let external_event_name = match (fst p.value).expecting_mt.value with
                | S.CType{value=S.TVar x} -> x
                | _ -> raise (Error.PlacedDeadbranchError (p.place, "expecting_mt should be a TVar, this should have been checked before Akka/Finish.ml"))
            in

            match Atom.AtomHashtbl.find_opt external_env external_event_name with
            | None -> Atom.AtomHashtbl.add external_env external_event_name p
            | Some p2 -> raise (Error.PlacedDeadbranchError (p.place@p2.place, "eports should be deterministic, this should have been checked before Akka/Finish.ml"))
        in
        List.iter hydrate_external_env grp_items.eports;


        (* Step1 - create {event_name: {(port, st, remaining_step i.e st) ->  callback}} *)
        let env : ((S.port * S.session_type * S.session_type, T.expr) Hashtbl.t) Atom.AtomHashtbl.t = Atom.AtomHashtbl.create 16 in
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
                    | st' -> 
                        logger#error "%s" (S.show_session_type st);
                        Core.Error.perror (fst p.value).expecting_st.place "%s plugin: expecting type of [%s] can only start by the reception of a message or of a label" plg_name (Atom.to_string (fst p.value).name)
                );

                st, (event_name, st_continuation)
            end 
            | _ -> Core.Error.perror (fst p.value).expecting_st.place "%s plugin do not support main type for port expecting" plg_name  
            in
            
            let inner_env : (S.port * S.session_type * S.session_type, T.expr) Hashtbl.t= begin 
                try 
                    Atom.AtomHashtbl.find env msg_type 
                with Not_found -> 
                    let _inner_env = Hashtbl.create 8 in 
                    Atom.AtomHashtbl.add env msg_type _inner_env; 
                    _inner_env
            end in

            let key = (p, expecting_st, remaining_st) in

            (* check that key are not duplicated for the current event *)
            try
                ignore (Hashtbl.find inner_env key);
                Error.perror (place@p.place) "Tuple (bridge, st) is not unique for the component %s" (Atom.hint name)
            with Not_found -> 
                Hashtbl.add inner_env key (fexpr parent_opt (fst p.value).callback)
        in

        List.iter hydrate_env grp_items.ports;

        (* Step 2 - Generate a receiver per event *)
        let generate_event_receiver (event_name:Atom.atom) (inner_env:(S.port * S.session_type * S.session_type, T.expr) Hashtbl.t) : T.stmt list =            
            (* Helpers *)
            let bridgeid (port: S.port) =
                e2_e (T.CallExpr (
                    e2_e (T.AccessExpr (
                        e2_e (T.AccessExpr (
                            e2_e T.This,
                            e2var (fst port.value).name
                        )),
                        e2_e (T.RawExpr "get_binded_bridge_id")
                    )),
                    []
                ))
            in
            let bridgestdual (port: S.port) =
                e2_e (T.CallExpr (
                    e2_e (T.AccessExpr (
                        e2_e (T.AccessExpr (
                            e2_e T.This,
                            e2var (fst port.value).name
                        )),
                        e2_e (T.RawExpr "expecting_st.dual")
                    )),
                    []
                ))
            in

            let e_bridgeid e = e2_e (T.AccessExpr (e, e2var (Atom.builtin "bridge_id"))) in
            let e_sessionid e = e2_e (T.AccessExpr (e, e2var (Atom.builtin "session_id"))) in
            let e_replyto e = e2_e (T.AccessExpr (e, e2var (Atom.builtin "replyTo"))) in
            let e_remaining_step e = e2_e (T.AccessExpr (e, e2var (Atom.builtin "st"))) in
            let e_init_stage e = e2_e (T.AccessExpr (e, e2var (Atom.builtin "init_stage"))) in
            let e_hidden_right e = e2_e (T.AccessExpr (e, e2var (Atom.builtin "hidden_right"))) in

            let a_session = Atom.builtin "s" in
            let l_session = e2var a_session in

            let generate_case_body ((e_port, e_remaining_st): T.expr * T.expr) (callback:T.expr) : T.stmt list = 
                [
                    auto_place (T.LetStmt (
                        t_varda_session place,
                        a_session,
                        Some ( e2_e (T.NewExpr(
                            e_varda_session place,
                            [
                                e_bridgeid l_event;
                                e2_e (T.CastExpr(
                                    auto_place (T.TVar (Atom.builtin "ActivationRef")),
                                    e_get_self_activation place (e_get_context place)
                                ));
                                e_replyto l_event;
                                e_remaining_st;
                                e_init_stage l_event;
                                e_hidden_right l_event;
                                e_port;

                            ]
                        )))
                    ));
                    auto_place (T.ExpressionStmt (
                        e2_e (T.CallExpr(
                            e_setid_of_session fplace a_session,
                            [ e_sessionid l_event]
                        ))
                    ));
                    auto_place (T.ExpressionStmt (
                        e_apply_headers (this_actor parent_opt) fplace l_session
                    ));
                ] @ (
                    if Config.trace_enabled () then [   
                        auto_fplace(T.RawStmt {|
                        long durationReception2Callback = System.nanoTime() - t_actor_event_reception;
                        durationsReception2Callback.add(durationReception2Callback);
                        |})
                    ]
                    else []
                ) @
                [
                    auto_place (T.ExpressionStmt (
                        e2_e (T.CallExpr(
                            callback,
                            [ l_event; l_session ]
                        ))
                    ))
                ] @ (
                    if Config.trace_enabled () then [   
                        auto_fplace(T.RawStmt {|
                        long durationReception2EndCallback = System.nanoTime() - t_actor_event_reception;
                        durationsReception2EndCallback.add(durationReception2EndCallback);
                        |})
                    ]
                    else []
                )
            in


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
                    e2_e(T.UnopExpr(
                        Core.AstUtils.Not,    
                        e2_e (T.CallExpr(
                            e2_e (T.AccessExpr(
                                e2var (Atom.builtin "Handlers"),
                                e2var (Atom.builtin "is_session_alive")
                            )),
                            [ 
                                e_cast fplace "ActorContext" (e_get_context fplace);
                                e_cast fplace "ActivationRef" (e_get_self_activation fplace (e_get_context fplace));
                                e_this_frozen_sessions (this_actor parent_opt) fplace; 
                                e_this_dead_sessions (this_actor parent_opt) fplace; 
                                e_sessionid l_event; 
                                e_replyto l_event;
                            ]
                        ))
                    )),
                    auto_place(T.BlockStmt [
                        auto_place(T.ReturnStmt (e_behaviors_same fplace)) 
                    ]),
                    None 
                ))
            in

            let add_check_registered_session_for_callback ()=
                (*
                    if(this.registered_session.containsKey(e.session_id)){
                        InPort p = this.registered_session.get(e.session_id);
                        com.varda.Session s = ...
                        s.set_id(e.session_id);
                        ASTStype.TimerHeader.apply_headers(...);
 
                        p.callback.apply(...);
                        return Behaviors.same();
                    }
                    *)
                let l_port = Atom.fresh "port" in

                auto_place (T.IfStmt(
                    T_A2.e2_e(T.CallExpr(
                        T_A2.e2_e (T.RawExpr "this.registered_session.containsKey"),
                        [ e_sessionid l_event ]
                    )),
                    auto_place (T.BlockStmt ( 
                        auto_place (T.LetStmt(
                            auto_place (T.Atomic "InPort"),
                            l_port,
                            Some (T_A2.e2_e(T.CallExpr(
                                T_A2.e2_e (T.RawExpr "this.registered_session.get"),
                                [ e_sessionid l_event ]
                            )))
                        ))
                        :: (
                            generate_case_body 
                                (
                                    T_A2.e2var l_port, 
                                    T_A2.e2_e (T.AccessExpr (T_A2.e2var l_port, T_A2.e2_e (T.RawExpr "remaining_st()")))
                                ) 
                                (T_A2.e2_e (T.AccessExpr (T_A2.e2var l_port, T_A2.e2_e (T.RawExpr "getCallback().apply"))))
                        )
                        @ [ auto_place(T.ReturnStmt (e_behaviors_same fplace)) ]
                    )),
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


            let add_case__disable_session (callback:T.expr) : T.stmt =
                auto_place (T.ExpressionStmt (
                    e2_e (T.CallExpr(
                        callback,
                        [ l_event; ]
                    ))
                ))
            in

            let add_case__with_session (port, st, remaining_st) (callback:T.expr) acc : T.stmt =
                auto_place (T.IfStmt (
                    e2_e (T.BinopExpr(
                        e2_e (T.BinopExpr (e_bridgeid l_event, T.StructuralEqual, bridgeid port)),
                        T.And,
                        e2_e (T.BinopExpr (e_remaining_step l_event, T.StructuralEqual, bridgestdual port))
                    )),
                    auto_place (T.BlockStmt (generate_case_body (
                        e2_e (T.AccessExpr(
                            e2_e T.This,
                            e2var (fst port.value).name
                        )), fvstype parent_opt remaining_st) callback)),
                    Some acc
                ))
            in

            let add_case (port, st, remaining_st) (callback:T.expr) acc : T.stmt =
                if (fst (port : S.port).value)._is_intermediate then
                    acc
                else
                    if (fst (port : S.port).value)._disable_session then
                        add_case__disable_session callback
                    else
                        add_case__with_session (port, st, remaining_st) callback acc
                
            in

            (* return Behaviors.same(); *)
            let ret_stmt = T.ReturnStmt (e_behaviors_same fplace) in

            (*(if Bool.not _disable_session then*)
                 [ add_check_session_validity (); add_check_registered_session_for_callback () ] 
                (*else [])*)
            @ 
            [
                Hashtbl.fold add_case inner_env (auto_place (T.ExpressionStmt ((e_error_of fplace (e_get_context fplace) [
                    e2_lit (T.StringLit"Dispatcher does not caught message ");
                    e2_e (T.CallExpr (
                        e2_e (T.AccessExpr(
                            l_event,
                            e2_e (T.RawExpr "toString")
                        )),
                        []
                    ));
                ]))));
                auto_place ret_stmt
            ]
        in

        (* Step 2bis - Generate a receiver per external event (i.e. eport) *)

        (* TODO check at most one port per external event type (deterministic) *)
        let generate_external_event_receiver (port:S.eport) : T.stmt list =
            [
                auto_place (T.ExpressionStmt (
                    e2_e (T.CallExpr(
                        fexpr parent_opt (fst port.value).callback,
                        [ l_event ]
                    ))
                ));
                (* return Behaviors.same(); *)
                auto_place (T.ReturnStmt (e_behaviors_same fplace))
            ]
        in

        let generate_tracing_for_on_message () =
            if Config.trace_enabled () then [
                auto_fplace(T.TemplateStmt (
                    {|
                    t_actor_event_reception = System.nanoTime();
                    |}, [ ]
                ))
            ]
            else []
        in

        (* Step3 - Generate the component receiver *)
        let generate_component_receiver () = 
            logger#debug "> generate_receiver for [%s]" (Atom.to_string name);
            let init_receiver_expr : T.expr = {place; value=T.CallExpr(
                {place; value=T.VarExpr (
                    Atom.builtin "newReceiveBuilder"
                ), auto_place T.TUnknown}, 
                []
            ), auto_place T.TUnknown} in


            let add_on_result_received acc intermediate_futures = 
                T_A2.e2_e (T.AccessExpr(
                    acc, 
                    T_A2.e2_e (T.CallExpr (
                        T_A2.e2var (Atom.builtin "onMessage"),
                        [

                            {place; value=T.ClassOf (auto_place (T.TVar (Atom.builtin "ResolvedResult"))), auto_place T.TUnknown};
                            e2_e (T.LambdaExpr (
                                [
                                    auto_place T.TUnknown, l_event_name 
                                ],
                                auto_place(T.BlockStmt 
                                ((generate_tracing_for_on_message ())@
                                [
                                    auto_place(T.ExpressionStmt(e2_e(T.CallExpr(
                                        e2var (Atom.builtin "ResolvedResult.onResolvedResult"),
                                        [
                                            e_get_context fplace;
                                            e2_e(T.AccessExpr (
                                                e2_e T.This, 
                                                e2var intermediate_futures
                                            ));
                                            l_event
                                        ]
                                    ))));
                                    auto_place(T.ReturnStmt(
                                        e_behaviors_same fplace
                                    ))
                                ]))
                            ))

                        ]
                    ))
                ))
            in


            let add_timer_case acc (event_name, handler) =
                {place; value=T.AccessExpr(
                    acc, 
                    {place; value=T.CallExpr(
                        {place; value=T.VarExpr (Atom.builtin "onMessage"), auto_place T.TUnknown}, 
                        [
                            {place; value=T.ClassOf (auto_place (T.TVar (Atom.builtin event_name))), auto_place T.TUnknown};
                            e2_e (T.LambdaExpr (
                                [
                                    auto_place T.TUnknown, l_event_name 
                                ],
                                auto_place(T.BlockStmt 
                                ((generate_tracing_for_on_message ())@
                                [
                                    auto_place(T.ExpressionStmt(e2_e(T.CallExpr(
                                        e2var (Atom.builtin handler),
                                        [
                                            e_get_context fplace;
                                            e_get_self_activation place (e_get_context fplace);
                                            (*Rt.Misc.e_this_timers;*)
                                            e_this_frozen_sessions (this_actor parent_opt) fplace;
                                            e_this_dead_sessions (this_actor parent_opt) fplace;
                                            e_this_intermediate_states (this_actor parent_opt) fplace;
                                            l_event
                                        ]
                                    ))));
                                    auto_place(T.ReturnStmt(
                                        e_behaviors_same fplace
                                    ))
                                ]))
                            ))
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
            let init_receiver_expr = 
                match Hashtbl.find_opt FutureElim0.intermediate_futures_tbl name with
                | Some intermediate_futures -> add_on_result_received init_receiver_expr intermediate_futures
                | _ -> init_receiver_expr
            in

            let add_external_case event_name (port:S.eport) (acc, acc_methods) =
                let _m_name = Atom.fresh "external_event_dispatcher" in
                let _m : T.method0 = auto_place {
                    T.decorators = [];
                    annotations = [T.Visibility T.Public];
                    v = {
                        T.ret_type = t_behavior_of_actor fplace name;
                        name = _m_name;
                        body = AbstractImpl (generate_external_event_receiver port);
                        args = [
                            (
                                auto_place (T.TParam (
                                    auto_place(T.TVar event_name),
                                    [ ]
                                )), 
                                l_event_name
                            )

                        ];
                        throws          = [];
                        is_constructor  = false;
                    }
                } in

                ({place; value=T.AccessExpr(
                    acc, 
                    {place; value=T.CallExpr(
                        {place; value=T.VarExpr (Atom.builtin "onMessage"), auto_place T.TUnknown}, 
                        [
                            {place; value=T.ClassOf (auto_place (T.TVar event_name)), auto_place T.TUnknown};
                            if Config.trace_enabled () then
                                e2_e (T.LambdaExpr (
                                    [
                                        auto_place T.TUnknown, l_event_name 
                                    ],
                                    auto_place(T.BlockStmt 
                                    ((generate_tracing_for_on_message ())@
                                    [
                                        auto_fplace (T.ReturnStmt (e2_e(T.CallExpr(
                                            e2_e (T.AccessExpr (
                                                e2_e T.This,
                                                e2var _m_name
                                            )),
                                            [ l_event ]
                                        ))))
                                    ]))
                                ))
                            else
                                e2_e (T.AccessMethod (
                                    e2_e T.This,
                                    _m_name
                                ))
                        ]
                    ), auto_place T.TUnknown}
                ), auto_place T.TUnknown}, _m::acc_methods)
            in

            let add_case event_name inner_env (acc, acc_methods) =
                logger#error "> add_case for [%s] %s %d %d %d" (Atom.to_string name) (Atom.to_string event_name) (Atom.identity event_name) (Hashtbl.hash event_name) (Atom.hash event_name);
                (
                    match Atom.AtomHashtbl.find_opt external_env event_name with
                    | None -> ();
                    | Some _ -> Error.error "(currently) event [%s] can not be used for both eport and input port" (Atom.to_string event_name)
                );

                let _m_name = Atom.fresh "event_dispatcher" in
                let _m : T.method0 = auto_place {
                    T.decorators = [];
                    annotations = [T.Visibility T.Public];
                    v = {
                        T.ret_type = t_behavior_of_actor fplace name;
                        name = _m_name;
                        body = AbstractImpl (
                        (if Config.trace_enabled () then 
                            [
                                auto_fplace(T.RawStmt {|
                                long durationReception2Dispatcher = System.nanoTime() - t_actor_event_reception;
                                durationsReception2Dispatcher.add(durationReception2Dispatcher);
                                |})
                            ]    
                        else [])@
                        generate_event_receiver event_name inner_env);
                        args = [
                            (
                                auto_place (T.TParam (
                                    auto_place(T.TVar event_name),
                                    [ ] (* FIXME for now metadata is fixed per type of event and cannot be changed in a per channel basis TODO t_varda_nometadata fplace ]*)
                                )), 
                                l_event_name
                            )

                        ];
                        throws          = [];
                        is_constructor = false;
                    }
                } in

                ({place; value=T.AccessExpr(
                    acc, 
                    {place; value=T.CallExpr(
                        {place; value=T.VarExpr (Atom.builtin "onMessage"), auto_place T.TUnknown}, 
                        [
                            {place; value=T.ClassOf (auto_place (T.TVar event_name)), auto_place T.TUnknown};
                            if Config.trace_enabled () then
                                e2_e (T.LambdaExpr (
                                    [
                                        auto_place T.TUnknown, l_event_name 
                                    ],
                                    auto_place(T.BlockStmt 
                                    ((generate_tracing_for_on_message ())@
                                    [
                                        auto_fplace (T.ReturnStmt (e2_e(T.CallExpr(
                                            e2_e (T.AccessExpr (
                                                e2_e T.This,
                                                e2var _m_name
                                            )),
                                            [ l_event ]
                                        ))))
                                    ]))
                                ))
                            else
                                e2_e (T.AccessMethod (
                                    e2_e T.This,
                                    _m_name
                                ))
                        ]
                    ), auto_place T.TUnknown}
                ), auto_place T.TUnknown}, _m::acc_methods)
            in

            (* Akka support only one onMessage per type *)
            (*let group_by_expecting_msg_type = Hashtbl.create 16 in
            Hashtbl.iter (fun (p, expecting_st, remaining_st) value ->
                let msg_mt = in
                match Hashtbl.find_opt group_by_expecting_msg_type with
                | None ->
                    Hashtbl.add group_by_expecting_msg_type msg_mt [(p, expecting_st, remain_st, value)]
                | Some entries ->
                    Hashtbl.add group_by_expecting_msg_type msg_mt ((p, expecting_st, remain_st, value)::entries)
            ) env;*)


           
            Atom.AtomHashtbl.fold add_external_case external_env (Atom.AtomHashtbl.fold add_case env (init_receiver_expr, []))
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
                    throws          = [];
                    body            = T.AbstractImpl (
                    [
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

        Atom.AtomHashtbl.iter (fun event _ -> add_event_e2rs event name) env;
        Atom.AtomHashtbl.iter (fun event _ -> add_external_e2rs event name) external_env;

        (***** Building methods *****)
        let methods = receiver_methods @ (List.flatten (List.map (fmethod parent_opt name) grp_items.methods)) in 

        (* Update constructor to
           - set InPort callback *)
        let methods = Ast.map_constructor 
            (function m -> 
                { m with
                    value = {m.value with v = { m.value.v with body = match m.value.v.body with
                    | AbstractImpl stmts -> AbstractImpl (hydrate_state @ stmts)
                    | body -> body }}
                }
            )
            methods in
        

        (***** Sumup *****)
        [{
            place;
            value = {   
                T.extends = None;
                implemented_types = [];
                is_guardian = false;
                headers = headers;
                name;
                methods; 
                states;
                events;
                nested_items= List.map 
                    (function {plg_annotations; v=x}-> 
                        let x, annotations, decorators = finish_plgannot_component x (fplgannot plg_annotations) in

                        { 
                        place = x.place; 
                        value={ 
                            T.annotations=annotations@[ T.Visibility T.Public ];
                            decorators= decorators;
                            v = T.Actor x 
                        }
                    })
                    (List.flatten (List.map (function {v; plg_annotations} -> List.map (function y -> {v=y; plg_annotations}) (fcdcl parent_opt v)) grp_items.nested)
                ); 
                static_items = 
                (List.flatten (List.map (fterm parent_opt) grp_items.others))
                @ (List.map (fclass parent_opt) grp_items.classes); 
                receiver = receiver
            }
        }]
    end 
    | S.ComponentAssign _ -> failwith "Component expr are not yet supported" 

    and fcdcl parent_opt : S.component_dcl -> T.actor list = function cdcl -> finish_component_dcl parent_opt cdcl.place cdcl.value 

    (********************** Manipulating component structure *********************)
    and finish_component_expr parent_opt place = function
        | S.VarCExpr x, mt -> T.VarExpr x, fmtype parent_opt mt
        | _ -> failwith "Akka plg do not support yet advance component expr" 
    and fcexpr parent_opt ce : T.expr = map_place (finish_component_expr parent_opt) ce

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
                                e2_e T.This,
                                e2var name
                            ), ct)
                        ))
                    ];
                    args = [];
                    throws          = [];
                    is_constructor = false; 
                }
            })
        } in
        List.mapi make_getter args

    and finish_term parent_opt place plg_annotations : S._term -> T.term list = 
    let fplace = place@(Error.forge_place "Plg=Akka/finish_term" 0 0) in
    let auto_place smth = {place = fplace; value=smth} in
    function
    | S.Comments c -> 
        assert(plg_annotations = []);
        [{
        place;
        value= {
            T.annotations = [];
            decorators = [];
            v = T.Comments c.value
        }
    }]
    | S.BBTerm bbterm -> 
        assert(plg_annotations = []);
        [{
        place;
        value= {
            T.annotations = [];
            decorators = [];
            v = T.BBTerm (fbbterm parent_opt bbterm) 
        }
    }]
    | S.Component cdcl ->
        List.map (function a -> 
            let a, annotations, decorators = finish_plgannot_component a plg_annotations in

            {
                place=a.place; 
                value= {
                    T.annotations = annotations;
                    decorators = decorators;
                    v=T.Actor a
                }
            }
        ) (fcdcl parent_opt cdcl)
    | S.Stmt stmt -> 
        assert(plg_annotations = []);
        [{
        place; 
        value= {
            T.annotations = [];
            decorators = [];
            v = T.Stmt (fstmt parent_opt stmt)
        }
    }]
    | S.Function f -> 
        List.map (function m -> 
           let m, res = List.fold_left_map (fun (m:T.method0) -> map0_place (function place -> function
                | T.AOverride -> 
                    m, ([], [T.Override])
                    
                | AExtends _ | AImplements _ -> Error.perror place "Function does not support neither extends nor implements !"
            )) m plg_annotations in

            let (annotations, decorators) = List.split res in
            let annotations = List.flatten annotations in
            let decorators = List.flatten decorators in 
            
            {
                place; 
                value= {
                    T.annotations = annotations;
                    decorators = decorators;
                    v = T.MethodDeclaration m
                }
    }) (ffunction parent_opt f)
    | S.Typedef {value=S.ProtocolDef (name, {value=S.SType st; _});_} -> 
        assert(plg_annotations = []);
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
                    name= name;
                    args= [];
                    headers = [];
                }
            }:: (extract_events st_next.place (k+1) st_next.value)
        | (S.STSend _ as t)| (STRecv _ as t)-> failwith "toto"
        | S.STBranch entries | STSelect entries -> begin
            let aux_entry (label, st, _) = 
                { 
                    place; 
                    value = {
                        T.vis = T.Public; 
                        name = label;
                        args= [];
                        headers = [];
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
            e2_e T.This, 
            e2var l_st
        ) in

        (* com.varda.Protocol *)
        let m_get_st = {
            T.decorators = [];
            annotations = [T.Visibility T.Public];
            v = {
                T.ret_type = auto_place (T.TVar (a_ASTStype_of ""));
                name = Atom.builtin "get_st";
                body = AbstractImpl [
                    auto_place (T.ReturnStmt (
                        fvstype parent_opt st
                    ))
                ];
                args = [];
                throws          = [];
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
                    headers = [];
                    isInterface = false;
                    extends = Some(t_varda_protocol place);
                    implemented_types = [];
                    name = name;
                    body = stmts @ sub_classes @ methods (*@ events*)
                }
            }
        }]

        (* TODO generate the dynamic checking of protocol order if needed *)

    | S.Typealias (v, S.AbstractTypealias body) -> 
        assert(plg_annotations = []);
        raise (Error.PlacedDeadbranchError (place, "partial evaluation should have removed type alias exept those from impl"))
    | S.Typealias (x, S.BBTypealias body) as term -> 
        assert(plg_annotations = []);

        (* Java does not support type aliasing *)
        Hashtbl.add typealias x body;
        []
    |Typedef {value= EventDef (name, mts, None) as tdef; place = inner_place} ->
        assert(plg_annotations = []);

        (* Registration *)
        Hashtbl.add to_capitalize_variables name ();

        [{
            place;
            value = {
                T.annotations = [];
                decorators = [];
                v = T.Event (finish_eventdef parent_opt inner_place (name, mts, None)) 
            }
        }]

    (* Inductive type definition *)
    | S.Typedef  {value= ClassicalDef (name, args, None) as tdef; place} -> (* implicit constructor should translate to akka *)
        assert(plg_annotations = []);
        (* Registration *)
        Hashtbl.add to_capitalize_variables name ();

        let args = List.map (function (arg:T.ctype) -> (arg, Atom.fresh "arg")) (List.map (fmtype parent_opt) args) in
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
                    T.ret_type = {place; value = T.Atomic "Void"}; (* removed by the is_constructor *)
                    name;
                    body = T.AbstractImpl [{place; value  = constructor_body}];
                    args = args;
                    throws          = [];
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
                    headers = [];
                    isInterface = false;
                    extends = None;
                    implemented_types = [];
                    name;
                    body = fields @ (constructor::getters) 
                }
            }
        }]
    | S.Typedef {value = ClassicalDef (v, _, Some body); _} ->
        assert(plg_annotations = []);
        (* Registration *)
        Hashtbl.add to_capitalize_variables v ();
        [{
            place; 
            value = {
                T.annotations = [T.Visibility T.Public];
                decorators = [];
                v = T.ClassOrInterfaceDeclaration {
                    headers = [];
                    isInterface = false;
                    extends = Some (auto_place (T.TBB (fbbterm parent_opt body)));
                    implemented_types = [];
                    name = v;
                    body = [] 
                }
            }
        }]

    | S.Typedef {value = EventDef (v, _, Some body); _} -> 
        assert(plg_annotations = []);
        Error.perror place "eventdef with body is not yet supported by the akka.finish"
    | S.Typedef {value = VPlaceDef x;} -> 
        assert(plg_annotations = []);
        (* Registration *)
        Hashtbl.add to_capitalize_variables x ();
        [{
            place; 
            value = {
                T.annotations = [];
                T.decorators = [];
                v = begin
                   T.ClassOrInterfaceDeclaration {
                       headers = [];
                       isInterface = false;
                       name = x;
                       extends = None;
                       implemented_types = []; (* TODO inherit ??*)
                       body = [] (* TODO add x as identity ?? *)
                   } 
                end
            }
        }]

    and fterm parent_opt : S.term -> T.term list = function t -> finish_term parent_opt t.place (fplgannot t.value.plg_annotations) t.value.v

    and fplgannot plg_annotations = 
        plg_annotations
        |> List.map Plgfrontend.Parse.parse
        |> List.flatten

    let cstate = ref (empty_cstate ())

    let finish_program program = 
        let terms =     
            program
            |> GuardTransform.gtransform_program
            |> FutureElim.apply
            |> function terms -> List.flatten (List.rev(List.map (fterm (None,None)) terms))
        in
        
        (* Apply renaming *)
        let terms = List.map (T.apply_rename_term true (make_capitalize_renaming)) terms in 
        rename_collected_state (make_capitalize_renaming);  
       
        cstate := {
            target = Some (match !cstate.target with |None -> Arg.target |Some target -> target);
            event2receptionists;
            external2receptionists;
            collected_components;
            guardian_components = ref Atom.Set.empty (* hydrated in AkkaJava.ml *)
        };

        terms   

    (*****************************************************)
    let name = "Akka.Finish"
    let displayed_pass_shortdescription = Printf.sprintf "Codegen: Runtime AST" 
    let displayed_ast_name = "Runtime AST"
    let show_ast = true
    let global_at_most_once_apply = false

    let precondition program = program
    let postcondition program = program
    let apply_program = finish_program
end