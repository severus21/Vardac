(* TODO create interface/GRPC where interface/ list interface generation plugin with external world *)
(* https://doc.akka.io/docs/akka-grpc/current/server/walkthrough.html#stateful-services *)

open Core
open AstUtils

open Jingoo

let name = "gRPC"

let fplace = (Error.forge_place ("Plg=Akka/interfaces/"^name) 0 0)
let auto_fplace smth = {place = fplace; value=smth}
module S_A2 = AstUtils2.Mtype.Make(struct let fplace = fplace end)
module T_A2 = Ast.AstUtil2.Make(struct let fplace = fplace end)

module Make (Arg : sig
    val build_dir : Fpath.t 
    val target : Target.target
end) = struct
    include Arg

    (************ Externals & Templates ***************)

    let [templates_location] =  Mysites.Sites.akka_interfaces_grpc_templates
    let [externals_location] =  Mysites.Sites.akka_interfaces_grpc_externals

    (* TODO add this to general utils *)
    let l2f = function
    | [] -> assert(false) 
    | h::t -> List.fold_left Fpath.add_seg (Fpath.v h) t
    
    let custom_external_rules () = []
    let proto_models = ref None
    let custom_template_rules () = 
        (* Check that state have been correctly hydrated first *)
        assert(!proto_models <> None);      
        [ 
            (
                l2f [templates_location; "auto"; "grpc"; "proto.j2"], 
                Option.get !proto_models, 
                l2f [Fpath.to_string build_dir; "src"; "main"; "protobuf"; "proto.proto"]
            );
        ]

    let auto_jingoo_env () = [] 

    (************** Shared state *****************)
    let istate = ref (Plg.Interface_plugin.empty_istate ())

    (**************************************************)

    module S = IRI
    module T = Ast

    type msg_field = {
        name: Atom.t;
        ctype: T.ctype;
        protobuf_type: string;
        protobuf_id: int;
    }
    type msg = {
        name: Atom.t;
        fields: msg_field list; 
    }
    let grpc_messages : (Atom.t, msg) Hashtbl.t= Hashtbl.create 128 

    type rpc = {
        name: Atom.t;
        m: S.method0;
        in_type: Atom.t;
        out_type: Atom.t;
    }

    type service = {
        service_name: Atom.t;
        component_name: Atom.t;
        impl_name: Atom.t;
        client_name: Atom.t;
        let_client_name: Atom.t;
        rpcs: rpc list;
        service_handler_instance: Atom.t;
    }
    let grpc_services : (Atom.t, service) Hashtbl.t = Hashtbl.create 32 


    (** Hydrate both grpc_messages and grpc_services
        N.B. this fct is in charge of creating the names for services, impls, fields, types, ...
    *)
    let hydrate_grpc program =
        let selector = function _ -> true in
        
        (* for each component returns (component_name, expose methods) *)
        let collector _ _ = function
            | S.Component {value= S.ComponentStructure cstruct} -> 
                [(cstruct.name, List.filter_map (function 
                    | {value= S.Method m} when List.mem S.Expose m.value.annotations -> Some m
                    | _ -> None
                ) cstruct.body)]
            | _ -> []
        in

        let collected_elts = IRI.IRUtils.collect_term_program true selector collector program in

        (** Create services *)
        List.iter (function (component_name, exported_methods) ->

            let rpcs = List.map (function (m: S.method0) ->

                (** Generates messages and store them in grpc_messages *)

                let rec _gendef place = function
                    | S.CType {value=S.TFlatType ft} -> begin 
                        match ft with
                        | TStr -> auto_fplace(T.Atomic "String"), "string"
                        | TInt -> auto_fplace(T.Atomic "Integer"), "int32" (* since no long during conversion*)
                        | TFloat -> auto_fplace(T.Atomic "Float"), "float"
                        | TBool -> auto_fplace(T.Atomic "Boolean"), "bool"
                        | TVoid -> auto_fplace(T.Atomic "Void"), "NullValue"
                        | _ -> Error.perror place "Type unsupported for interface, should be a simple atomic type."
                    end
                    | S.CType {value=S.TDict (mt1, mt2)} ->
                        let ct1, proto1 = gendef mt1 in
                        let ct2, proto2 = gendef mt2 in
                        auto_fplace(T.TMap (ct1, ct2)), Printf.sprintf "map<%s,%s>" proto1 proto2
                    | S.CType {value=S.TList mt} ->
                        let ct, proto = gendef mt in
                        auto_fplace (T.TList ct), "ListValue"
                    | _ -> Error.perror place "Type unsupported for interface, should be a simple atomic type."
                and gendef x = map0_place _gendef x in

                let to_field i ({value=mt,x}) : msg_field = 
                    let ctype, protobuf_type = gendef mt in
                    { 
                        name = Atom.fresh (Atom.value x);
                        protobuf_type;
                        ctype;
                        protobuf_id = i+1; 
                    }
                in 

                let in_msg = {
                    name = Atom.fresh "ProtoMsgIn";
                    fields = List.mapi to_field m.value.args;
                } in
                let out_msg = {
                    name = Atom.fresh "ProtoMsgOut";
                    fields = List.mapi to_field [auto_fplace (m.value.ret_type, Atom.fresh "RetValue")];
                } in

                Hashtbl.add grpc_messages in_msg.name in_msg; 
                Hashtbl.add grpc_messages out_msg.name out_msg; 

                (** Build rpc *)
                {
                    name = Atom.fresh ((Atom.to_string m.value.name)^"RPC");
                    m = m;
                    in_type = in_msg.name;
                    out_type = out_msg.name;
                }
            ) exported_methods in
                
            (* Only retains component that expose external interfaces *)
            match rpcs with
            | [] -> () 
            | _ -> 
                let service_name = Atom.fresh ((Atom.value component_name)^"Service") in
                let client_name = Atom.builtin ((Atom.to_string service_name)^"Client") in
                let service = {
                    component_name;
                    service_name = service_name;
                    impl_name = Atom.fresh ((Atom.value component_name)^"ServiceImpl"); 
                    client_name = client_name ; 
                    let_client_name = Atom.builtin ("client"^(Atom.to_string client_name));
                    rpcs = rpcs;
                    service_handler_instance = Atom.fresh (String.uncapitalize_ascii (Atom.value service_name));
                } in
                Hashtbl.add grpc_services service.service_name service
        ) collected_elts

    (* Stage 1 - Generating protobuf interfaces and stubs 
        in src/main/protobuf/*.proto
    *)
    let generate_protobuf_interfaces_env build_dir program =
        (*let protodir = List.fold_left Fpath.add_seg build_dir ["src"; "main"; "protobuf"] in
        Core.Utils.refresh_or_create_dir protodir;
        (* TODO create multiple file *)
        let protofile = Fpath.add_seg protodir "proto.proto" in
        *)

        let encode_message (msg:msg) = 
            let encode_field field = 
                Jg_types.Tobj [
                    ("type", Jg_types.Tstr field.protobuf_type);
                    ("name", Jg_types.Tstr (Atom.to_string field.name));
                    ("id", Jg_types.Tint field.protobuf_id);
                ]
            in
            Jg_types.Tobj [
                ("name", Jg_types.Tstr (Atom.to_string msg.name));
                ("fields", Jg_types.Tlist (List.map encode_field msg.fields))
            ]
        in

        let encode_service service = 
            let encode_rpc rpc = 
                Jg_types.Tobj [
                    ("name", Jg_types.Tstr (Atom.to_string rpc.name));
                    ("msg_in", Jg_types.Tstr (Atom.to_string rpc.in_type));
                    ("msg_out", Jg_types.Tstr (Atom.to_string rpc.out_type));
                ]
            in

            Jg_types.Tobj [
                ("name", Jg_types.Tstr (Atom.to_string service.service_name));
                ("rpcs", Jg_types.Tlist (List.map encode_rpc service.rpcs));
            ]
        in
        let services = List.map encode_service (List.of_seq (Hashtbl.to_seq_values grpc_services)) in
        let messages = List.map encode_message (List.of_seq (Hashtbl.to_seq_values grpc_messages)) in
        
        [
            ("project_name", Jg_types.Tstr (Config.project_name ()));
            ("author", Jg_types.Tstr (Config.author ()));

            ("services", Jg_types.Tlist services);
            ("msgs", Jg_types.Tlist messages)
        ]

        (*let res = Jg_template.from_file (Fpath.to_string proto_template) ~models:models in
        let oc = open_out (Fpath.to_string protofile) in
        Printf.fprintf oc "%s" res; 
        close_out oc*)


    (* Stage 2 - Implementing the services *)
    let generate_service_implementation program service =
        let att_system = Atom.fresh "system" in
        let att_actor = Atom.fresh "actor" in
        let constructor_arg_system = Atom.fresh "system" in


        let service2actor_events = ref [] in

        let body_rpcs = List.map (function rpc ->
            let ct_msg_in = auto_fplace (T.TVar rpc.in_type) in
            let a_msg_in = Atom.fresh "in" in
            let ct_msg_out = auto_fplace (T.TVar rpc.out_type) in 

            let a_intermediate_msg = Atom.fresh "message" in

            let in_msg = 
                try Hashtbl.find grpc_messages rpc.in_type
                with Not_found -> raise (Error.DeadbranchError "rpc in_type must be registered into grpc_messages")
            in
            let out_msg = 
                try Hashtbl.find grpc_messages rpc.out_type
                with Not_found -> raise (Error.DeadbranchError "rpc out_type must be registered into grpc_messages")
            in

            let service2actor_event = auto_fplace {
                T.vis = T.Public;
                name = Atom.fresh "Service2Actor";
                args = 
                (List.map (function f -> f.ctype, f.name) in_msg.fields)
                @ [ 
                    (auto_fplace (T.Atomic "ActorRef"), Atom.fresh "replyTo")
                ];
                headers = [ (Printf.sprintf "import %s.%s.grpc.*;" (Config.author ()) (Config.project_name ()))];
            } in
            let actor2service_event = auto_fplace {
                T.vis = T.Public;
                name = Atom.fresh "Actor2Service";
                args = [ 
                    (match out_msg.fields with
                    | [f] -> (f.ctype, f.name)
                    | _ -> raise (Error.DeadbranchError "out_msg should have exactly one field, the rpc return value type")
                    );
                    (auto_fplace (T.ActorRef (auto_fplace (T.TVar service.component_name))), Atom.fresh "replyTo")
                ];
                headers = [ (Printf.sprintf "import %s.%s.grpc.*;" (Config.author ()) (Config.project_name ()))];
            } in

            service2actor_events := ( 
                (
                    rpc.m, 
                    (service2actor_event, in_msg), (actor2service_event, out_msg)
                ) :: !service2actor_events 
            );

            let event2protomsg = Atom.fresh "unpack" in

            auto_fplace {
                T.annotations = [];
                decorators = [];
                v = T.MethodDeclaration (auto_fplace {
                    T.annotations = [T.Visibility T.Public];
                    decorators = [T.Override];
                    v = {
                        T.ret_type = auto_fplace (T.TParam (auto_fplace (T.TRaw "CompletionStage"), [ct_msg_out]));
                        name = rpc.name; (* should be exaclty the same name than in proto.proto *)
                        args = [ (ct_msg_in, a_msg_in) ];
                        throws = [];
                        is_constructor = false;
                        body = AbstractImpl [
                            auto_fplace (T.TemplateStmt (
                                {|
    assert(this.{{att_actor}} != null);
                                |}, [ 
                                ("att_actor", Jingoo.Jg_types.Tstr (Atom.to_string att_actor))
                            ]));
                            auto_fplace (T.LetStmt(
                                auto_fplace (T.TParam(
                                    auto_fplace (T.TRaw "Function"),
                                    [
                                        auto_fplace (T.TVar actor2service_event.value.name);
                                        ct_msg_out
                                    ]
                                )),
                                event2protomsg,
                                Some(
                                    T_A2.e2_e (T.LambdaExpr(
                                        [
                                            (auto_fplace (T.TVar actor2service_event.value.name), a_intermediate_msg)
                                        ],
                                        auto_fplace (T.ReturnStmt(
                                            T_A2.e2_e (T.AccessExpr(
                                                T_A2.e2_e (T.AccessExpr(
                                                    T_A2.e2_e (T.AccessExpr(
                                                        T_A2.e2var out_msg.name,
                                                        T_A2.e2_e (T.RawExpr ("newBuilder()"))
                                                    )),
                                                    match out_msg.fields with
                                                    | [f] -> 
                                                        T_A2.e2_e(T.CallExpr(
                                                            T_A2.e2_e (T.RawExpr (Printf.sprintf "set%s" (String.capitalize_ascii (Atom.to_string f.name)))),
                                                            [
                                                                T_A2.e2_e (T.CallExpr (
                                                                    T_A2.e2_e (T.AccessExpr(
                                                                        T_A2.e2var a_intermediate_msg,
                                                                        T_A2.e2_e (T.RawExpr "_0_")
                                                                    )),
                                                                    []
                                                                ))
                                                            ]
                                                        ))
                                                    | _ -> failwith "wrong number of gRPC message fields for response"
                                                )),
                                                T_A2.e2_e (T.RawExpr "build()")
                                            ))
                                        ))
                                    ))
                                )
                            ));
                            auto_fplace (T.ReturnStmt(
                                T_A2.e2_e (T.CallExpr(
                                    T_A2.e2_e (T.AccessExpr(
                                        (* ask(greeterActor, GreeterActor.GET_GREETING, Duration.ofSeconds(5)) *)
                                        T_A2.e2_e(T.CallExpr( 
                                            T_A2.e2_e( T.RawExpr "AskPattern.ask"),
                                            [
                                                T_A2.e2_e (T.AccessExpr(
                                                    T_A2.e2_e T.This, 
                                                    T_A2.e2var att_actor
                                                ));
                                                (* replytTo -> Event Service2Actor *)
                                                begin 
                                                    let reply_to = Atom.fresh "replyTo" in
                                                    T_A2.e2_e (T.LambdaExpr (
                                                        [ (auto_fplace (T.TRaw ""), reply_to)],
                                                        auto_fplace (T.ReturnStmt (
                                                            T_A2.e2_e (T.NewExpr (
                                                                T_A2.e2var service2actor_event.value.name,
                                                                (
                                                                    List.map (function (f:msg_field) -> 
                                                                        T_A2.e2_e (T.CallExpr(
                                                                            T_A2.e2_e (T.AccessExpr( 
                                                                                T_A2.e2var a_msg_in,
                                                                                T_A2.e2_e (T.RawExpr (
                                                                                    Printf.sprintf "get%s" (String.capitalize_ascii (Atom.to_string f.name))
                                                                                ))
                                                                            )),
                                                                            []
                                                                        ))
                                                                    ) in_msg.fields
                                                                ) @
                                                                [ 
                                                                    T_A2.e2var reply_to;
                                                                ]
                                                            ))
                                                        ))
                                                    ))
                                                end;
                                                (* Timeout *)
                                                T_A2.e2_e (T.RawExpr "Duration.ofSeconds(5)");
                                                T_A2.e2_e (T.AccessExpr(
                                                    T_A2.e2_e (T.AccessExpr (T_A2.e2_e T.This, T_A2.e2var att_system)),
                                                    T_A2.e2_e (T.RawExpr "scheduler()")
                                                ));
                                            ]
                                        )),
                                        T_A2.e2_e (T.RawExpr "thenApply")
                                    )),
                                    [
                                        T_A2.e2var event2protomsg
                                    ]
                                ))
                            ))
                        ]
                    }
                });
            }
        ) service.rpcs in



        (* Add port for event in between service and actor *)
        let program = IRI.IRUtils.rewrite_term_program 
            (function 
                | S.Component {value = ComponentStructure {name} } when name = service.component_name -> true 
                |_ -> false) 
            (fun _ -> function
                | S.Component {place; value = ComponentStructure cstruct } when cstruct.name = service.component_name -> 
                    let inports = List.map (function ((m, (e1, proto_msg1), (e2, proto_msg2)):S.method0 * (T._event Core.IR.placed * msg) * (T._event Core.IR.placed * msg)) ->
                        (**
                        callback(e):
                            res = this.custom_callback(e.getX1(), ..., getXn()) X_i fields of Protobuf msg    
                            e.replyTo.tell(.tell(TrucMsg(res), getSelf())
                        *)
                        let lambda_e = Atom.fresh "e" in
                        let callback = auto_fplace {
                            S.annotations = [];
                            ghost = false;
                            ret_type = S_A2.mtype_of_ft Core.AstUtils.TVoid;
                            name = Atom.fresh "callback";
                            args = [ auto_fplace(S_A2.mtype_of_var e1.value.name, lambda_e) ];
                            contract_opt = None;
                            on_destroy = false;
                            on_startup = false;
                            body = S.AbstractImpl [ auto_fplace (S.ExpressionStmt (
                                S_A2.e2_e (S.CallExpr(
                                    S_A2.e2_e (S.AccessExpr(
                                        S_A2.e2_e (S.AccessExpr(
                                            S_A2.e2var lambda_e,
                                            S_A2.e2var (snd (try (List.nth e1.value.args (List.length e1.value.args -1)) with Not_found -> raise (Error.DeadbranchError "Service2Actor has at least one reply to field")))
                                        )),
                                        S_A2.e2_e (S.RawExpr ("tell"))
                                    )),
                                    [
                                        S_A2.e2_e (S.NewExpr(
                                            S_A2.e2var e2.value.name,
                                            [
                                                S_A2.e2_e (S.CallExpr(
                                                    S_A2.e2_e (S.AccessExpr( 
                                                        S_A2.e2_e S.This, 
                                                        S_A2.e2var m.value.name
                                                    )),
                                                    List.map (function (f:msg_field) -> 
                                                        S_A2.e2_e (S.AccessExpr( 
                                                            S_A2.e2var lambda_e,
                                                            S_A2.e2var f.name
                                                        ))
                                                    ) proto_msg1.fields
                                                ))
                                            ]
                                            @ [ S_A2.e2_e (S.RawExpr ("getContext().getSelf()")) ]
                                        ));
                                    ]
                                ))
                            ))];
                        } in

                        auto_fplace(S.Method callback), auto_fplace (S.Inport (auto_fplace ({
                            S.name = Atom.fresh ("port_service2actor_"^(Atom.value e1.value.name));
                            _disable_session = true;
                            expecting_st = S_A2.mtype_of_st (S.STRecv (S_A2.mtype_of_ct (S.TVar e1.value.name), auto_fplace (S.STSend (S_A2.mtype_of_ct (S.TVar e2.value.name), auto_fplace S.STEnd))));
                            callback = S_A2.e2_e (S.AccessExpr(
                                S_A2.e2_e S.This,
                                S_A2.e2var callback.value.name
                            )); 
                            _children = [];
                            _is_intermediate = false;
                        }, auto_fplace S.EmptyMainType)))
                    ) !service2actor_events in

                    let callbacks, inports = List.split inports in
                    
                    [
                        (S.Component{
                            place;
                            value = S.ComponentStructure { cstruct with 
                                headers = cstruct.headers @ [ 
                                    (Printf.sprintf "import %s.%s.grpc.*;" (Config.author ()) (Config.project_name ()));
                                ];
                                body = cstruct.body @ callbacks @ inports    
                            }
                        })
                    ]
        ) program in

        let events = List.map (function (m, (e1, _), (e2, _)) -> 
            let aux e = auto_fplace {
                T.annotations = [];
                decorators = [];
                v= T.Event e
            } in
            (m, aux e1, aux e2)
        ) !service2actor_events in
        let events = List.flatten (List.map (function (_,e1, e2)-> [e1;e2]) events) in

        let get_actor = Atom.fresh "getActor" in

        program, (events, auto_fplace {
                T.annotations = [];     
                decorators = [];
                v = T.ClassOrInterfaceDeclaration {
                    headers = [ 
                        "import akka.actor.typed.Props;";
                        "import akka.cluster.typed.Cluster;";
                        "import akka.actor.typed.receptionist.ServiceKey;";
                        (Printf.sprintf "import %s.%s.grpc.*;" (Config.author ()) (Config.project_name ()))
                    ];
                    isInterface = false;
                    name = service.impl_name; 
                    extended_types = [];
                    implemented_types = [ auto_fplace (T.TVar service.service_name) ];
                    body = 
                        [
                        auto_fplace {
                            T.annotations = [T.Final];
                            decorators = [];
                            v = T.Stmt (auto_fplace (T.LetStmt (
                                auto_fplace (T.TRaw "ActorSystem"), 
                                att_system,
                                None
                            )));
                        };
                        auto_fplace {
                            T.annotations = [T.Final];
                            decorators = [];
                            v = T.Stmt (auto_fplace (T.LetStmt (
                                auto_fplace (T.TRaw "ActorRef"), 
                                att_actor,
                                None
                            )));
                        };
                        (
                            let local_arg_system = Atom.fresh "system" in
                            let local_arg_retry = Atom.fresh "retry" in
                            auto_fplace {
                                T.annotations = [];
                                decorators = [];
                                v = T.MethodDeclaration (auto_fplace {
                                    T.annotations = [T.Visibility T.Public];
                                    decorators = [];
                                    v = {
                                        T.ret_type = auto_fplace (T.Atomic "ActorRef");
                                        name = get_actor;
                                        args = [
                                            auto_fplace (T.TRaw "ActorSystem"), local_arg_system;
                                            auto_fplace (T.TRaw "int"), local_arg_retry
                                        ];
                                        throws = [];
                                        is_constructor = false;
                                        body = T.BBImpl (auto_fplace {
                                            T.language = None;
                                            body = [
                                                T.Template(
                                                    {|
        assert (null != {{actorSystem}});

        int DEFAULT_MAX_RETRY = 5;
        long DEFAULT_RETRY_TIMEOUT = 500;   // in ms
        Duration DEFAULT_TIMEOUT = Duration.ofSeconds(3);

        Cluster cluster = Cluster.get({{actorSystem}});
        assert (null != cluster);
        ServiceKey key = PlaceDiscovery.activationsServiceKeyOf(cluster.selfMember().address());
        CompletionStage<Receptionist.Listing> result =
                AskPattern.ask({{actorSystem}}.receptionist(),
                        (ActorRef<Receptionist.Listing> replyTo) -> Receptionist.find(key, replyTo),
                        DEFAULT_TIMEOUT,
                        {{actorSystem}}.scheduler());

        ActorRef actor = null;

        try {
            // blocking call
            Set<ActorRef<{{componentName}}.Command>> listing = result.toCompletableFuture().get().getServiceInstances(key);
            if (listing.isEmpty()) {
                if (++{{retry}} < DEFAULT_MAX_RETRY + 1) {
                    final long timeout = DEFAULT_RETRY_TIMEOUT * {{retry}};
                    {{actorSystem}}.log().info("{{implName}}::getActor() retry " + {{retry}} + "/" + DEFAULT_MAX_RETRY + ", timeout=" + timeout);
                    // sleep and retry
                    Thread.sleep(timeout);
                    return {{getActor}}({{actorSystem}}, {{retry}});
                } else {
                    throw new RuntimeException("Could not find {{system_name}} after " + DEFAULT_MAX_RETRY + " retries.");
                }
            } else {
                actor = listing.iterator().next();    // TODO: if more than 1 result, use closest
                {{actorSystem}}.log().info("{{implName}}::getActor(): found {{componentName}} " + actor +
                        " out of " + listing.size());
            }
        } catch (java.util.concurrent.ExecutionException e) {
            assert(false);
            //TODO
        } catch (java.lang.InterruptedException e) {
            assert(false);
            //TODO
        }

        return actor;
                                                    |}, 
                                                    [
                                                        "actorSystem", Jg_types.Tstr (Atom.to_string local_arg_system);
                                                        "componentName", Jg_types.Tstr (Atom.to_string service.component_name);
                                                        "implName", Jg_types.Tstr (Atom.to_string service.impl_name);
                                                        "getActor", Jg_types.Tstr (Atom.to_string get_actor);
                                                        "retry", Jg_types.Tstr (Atom.to_string local_arg_retry);
                                                        "att_actor", Jg_types.Tstr (Atom.to_string att_actor);
                                                    ]
                                                )
                                            ]
                                        })
                                        
                                    }
                                })
                            }
                        );
                        auto_fplace {
                            T.annotations = [];
                            decorators = [];
                            v = T.MethodDeclaration (auto_fplace {
                                T.annotations = [T.Visibility T.Public];
                                decorators = [];
                                v = {
                                    T.ret_type = auto_fplace T.TUnknown;
                                    name = service.impl_name;
                                    args = [auto_fplace (T.TRaw "ActorSystem"),constructor_arg_system];
                                    throws = [];
                                    is_constructor = true;
                                    body = T.BBImpl (auto_fplace {
                                        T.language = None;
                                        body = [
                                            T.Template(
                                                {|
    this.{{att_system}} = {{arg_system}};
    this.{{att_actor}} = {{getActor}}({{arg_system}},  0);
                                                |},
                                                [
                                                    "att_system", Jg_types.Tstr (Atom.to_string att_system);
                                                    "att_actor", Jg_types.Tstr (Atom.to_string att_actor);
                                                    "arg_system", Jg_types.Tstr (Atom.to_string constructor_arg_system);
                                                    "getActor", Jg_types.Tstr (Atom.to_string get_actor);
                                                    "componentName", Jg_types.Tstr (Atom.to_string service.component_name);
                                                ]
                                            )
                                        ]
                                    });
                                }
                            })
                        };
                    ] @ body_rpcs;
                }
            })

    (**
        @return [ Akka program of the ServiceImpl ]
    *)
    let generate_services_implementation program =
        List.fold_left_map generate_service_implementation program (List.of_seq (Hashtbl.to_seq_values grpc_services))

    (* Stage 3 - generate and bind to the HTTP part 
        generate one HTTP server for all the services
    *)
    let generate_gRPC_server services =
        let main_server_name = Atom.fresh "MaingRPCServer" in
        (* Register main_server_name in istate,
           currently impl support at most one grpc_server *)
        assert(Bool.not(List.mem "grpc_server" (List.map fst !istate.jingoo_models)));
        istate := { !istate with
            jingoo_models = ("grpc_server", Jg_types.Tstr (Atom.to_string main_server_name))::(!istate.jingoo_models);
        };

        let att_system = Atom.fresh "sys" in
        let local_service_handlers = Atom.fresh "serviceHandlers" in

        (*
            akka.japi.function.Function<HttpRequest, CompletionStage<HttpResponse>> greeterService =
            GreeterServiceHandlerFactory.create(new GreeterServiceImpl(mat), sys);   
        *)
        let spawn_service service : T.stmt = 
            auto_fplace(
                T.LetStmt (
                    auto_fplace (T.TRaw "akka.japi.function.Function<HttpRequest, CompletionStage<HttpResponse>>"),
                    service.service_handler_instance,
                    Some (
                        T_A2.e2_e (T.CallExpr(
                            T_A2.e2_e (T.AccessExpr(    
                                T_A2.e2_e (T.RawExpr ((Atom.to_string service.service_name)^"HandlerFactory")),
                                T_A2.e2_e (T.RawExpr "create")
                            )),
                            [
                                T_A2.e2_e (T.NewExpr(
                                    T_A2.e2var service.impl_name,
                                    [ T_A2.e2var att_system; ]
                                ));
                                T_A2.e2var att_system;
                            ]
                        ))
                    )
                )
            )
        in

        let rec _spawn_service_handlers = function 
            | [] -> assert false; (* step 3 should have been skipped *) 
            | [s] -> auto_fplace (T.VarExpr s.service_handler_instance, auto_fplace T.TUnknown)
            | s1::s2::t ->
                List.fold_left ( fun e1 s2 ->
                    T_A2.e2_e (T.CallExpr (
                        T_A2.e2_e (T.RawExpr "ServiceHandler.concatOrNotFound"),
                        [ e1; T_A2.e2var s2.service_handler_instance; ]
                    ))
                )  (_spawn_service_handlers [s1]) (s2::t) 
        and spawn_service_handlers services =
            auto_fplace (T.LetStmt(
                auto_fplace (T.TRaw "akka.japi.function.Function<HttpRequest, CompletionStage<HttpResponse>>"),
                local_service_handlers,
                Some ( _spawn_service_handlers services )
            ))
        in

        (* Add HTTP endpoint to target *)
        let target = {
            place = fplace@target.place;
            value = { target.value with
            Target.codegen = {target.value.codegen with 
                mains = 
                    {
                        Target.name = Atom.to_string main_server_name;
                        bootstrap = main_server_name;
                        entrypoint = Atom.builtin "main";
                    } :: target.value.codegen.mains
            }
        }} in


        target, auto_fplace {
            T.annotations = [];     
            decorators = [];
            v = T.ClassOrInterfaceDeclaration {
                headers = List.map (function service -> 
                    Printf.sprintf 
                        "import %s.%s.grpc.%sHandlerFactory;"
                        (Config.author ())
                        (Config.project_name ())
                        (Atom.to_string service.service_name)
                ) services;
                isInterface = false;
                name = main_server_name; 
                extended_types = [Misc.t_lg4dc_abstract_system fplace];
                implemented_types = [];
                body = [
                    auto_fplace {
                        T.annotations = [];
                        decorators = [];
                        v = T.RawTerm (auto_fplace {
                            T.language = None;
                            body = [
                                T.Template ({|
    public static void main(String[] args) throws Exception {
        // important to enable HTTP/2 in ActorSystem's config
        Config conf = ConfigFactory.parseString("akka.http.server.preview.enable-http2 = on")
                .withFallback(AbstractMain.get_config(args));

        // Akka ActorSystem Boot
        ActorSystem sys = ActorSystem.create(
            {{author}}.{{project_name}}.{{main_server_name}}.create(), 
            "{{system_name}}",
            conf);

        run(sys).thenAccept(binding -> {
            System.out.println("gRPC server bound to: " + binding.localAddress());
        });

        // ActorSystem threads will keep the app alive until `system.terminate()` is called
    } 

    public static void main_run(ActorSystem sys){
        run(sys).thenAccept(binding -> {
            System.out.println("gRPC server bound to: " + binding.localAddress());
        });

        // ActorSystem threads will keep the app alive until `system.terminate()` is called
    }
    
    static public Behavior<SpawnProtocol.Command> create() {
        return create(null,  null);
    }
   
    static public Behavior<SpawnProtocol.Command> create( String name, Wait wait) {
        return Behaviors.setup(
            (context) -> {
                return Behaviors.withTimers(
                    (timers) -> {
                        context.getLog().debug( "{{main_server_name}}::create");
                        return new {{main_server_name}}(context, timers, name, wait);
                    } );
            } );
    }

    public {{main_server_name}}( 
        ActorContext<SpawnProtocol.Command> context, 
        TimerScheduler<SpawnProtocol.Command> timers, 
        String name, 
        Wait wait) {
        super(context,  timers,  name,  wait);
    }
                               |}, [
                                    "system_name", Jg_types.Tstr Misc.system_name;
                                    "main_server_name", Jg_types.Tstr (Atom.to_string main_server_name);
                               ])
                            ];
                        })
                    };
                    auto_fplace {
                        T.annotations = [];
                        decorators = [];
                        v = T.MethodDeclaration (auto_fplace {
                            T.annotations = [T.Visibility T.Public; T.Static];
                            decorators = [];
                            v = {
                                T.ret_type = auto_fplace (T.TRaw "CompletionStage<ServerBinding>");
                                name = Atom.builtin "run";
                                args = [ (auto_fplace (T.TRaw "ActorSystem"), att_system) ];
                                throws = [];
                                is_constructor = false;
                                body = AbstractImpl( 
                                    List.map spawn_service services
                                    @ [spawn_service_handlers services ]
                                    @ [
                                        (* Http.get(sys) .newServerAt("127.0.0.1", 8090) .bind(serviceHandlers) *)
                                        auto_fplace (T.ReturnStmt (
                                            T_A2.e2_e (T.CallExpr(
                                                T_A2.e2_e (T.AccessExpr(
                                                    T_A2.e2_e (T.AccessExpr(
                                                        T_A2.e2_e (T.CallExpr(
                                                            T_A2.e2_e (T.RawExpr "Http.get"),
                                                            [ T_A2.e2var att_system ]
                                                        )),
                                                        T_A2.e2_e (T.RawExpr "newServerAt(\"127.0.0.1\", 8090)")
                                                    )),
                                                    T_A2.e2_e (T.RawExpr "bind")
                                                )),
                                                [ T_A2.e2var local_service_handlers ]
                                            ))
                                        ))
                                    ]
                                )
                            }
                        })
                    }

                ]
            }
        }

    (* Stage 4 - 
    *)
    let generate_gRPC_client services =
        let main_client_name = Atom.fresh "MaingRPCClient" in
        (* Register main_client_name in istate,
           currently impl support at most one grpc_client *)
        assert(Bool.not(List.mem "grpc_client" (List.map fst !istate.jingoo_models)));
        istate := { !istate with
            jingoo_models = ("grpc_client", Jg_types.Tstr (Atom.to_string main_client_name)) ::(!istate.jingoo_models);
        };
        
        (* seed host *)
        let host = Atom.fresh "host" in
        (* seed port *)
        let port = Atom.fresh "port" in
        let system = Atom.fresh "system" in
        let e_system = T_A2.e2_e (T.AccessExpr (T_A2.e2_e T.This, T_A2.e2var system)) in
        let settings = Atom.fresh "settings" in
        let e_settings = T_A2.e2var settings in
        let client = Atom.fresh "client" in
        let e_client = T_A2.e2_e (T.AccessExpr (T_A2.e2_e T.This, T_A2.e2var client)) in

        let auto_annote x = {
            T.annotations = [];
            decorators = [];
            v = x
        } in

        let states : T.term list=  
            auto_fplace(auto_annote(T.Stmt(auto_fplace(T.LetStmt(
                auto_fplace(T.TRaw "akka.actor.ActorSystem"),
                system,
                None
            )))))
            :: List.map (function service -> 
                auto_fplace(auto_annote(T.Stmt(auto_fplace(T.LetStmt(
                    auto_fplace(T.TVar service.client_name),
                    service.let_client_name,
                    None
                )))))
            ) services
        in

        let main : T.term = 
            auto_fplace {
                T.annotations = [];
                decorators = [];
                v = T.MethodDeclaration (auto_fplace {
                    T.annotations = [T.Visibility T.Public];
                    decorators = [];
                    v = {
                        T.ret_type = auto_fplace T.TUnknown;
                        name = main_client_name;
                        args = [
                            auto_fplace (T.TRaw "String"), host;
                            auto_fplace (T.TRaw "int"), port;
                        ];
                        throws = [];
                        is_constructor = true;
                        body = AbstractImpl ([
                            auto_fplace(T.TemplateStmt({|
        this({{host}},{{port}},akka.actor.ActorSystem.create("{{main_client_name}}", 
            ConfigFactory.parseString(
                "akka.cluster.jmx.multi-mbeans-in-same-jvm=on \n" +
                "akka.remote.artery.canonical.hostname=127.0.0.1\n" +
                "akka.remote.artery.canonical.port=0 \n" +
                "akka.cluster.roles=[\"client\"] \n" +
                "akka.cluster.seed-nodes=[\"akka://{{actor_system}}@" + {{host}} + ":" + {{port}} + "\"] \n")));
                            |}, [
                                "host", Jingoo.Jg_types.Tstr (Atom.to_string host);
                                "port", Jingoo.Jg_types.Tstr (Atom.to_string port);
                                "main_client_name", Jingoo.Jg_types.Tstr (Atom.to_string main_client_name);
                                "system", Jingoo.Jg_types.Tstr (Atom.to_string system);
                            ]));
                        ])
                    }
                })  
            }
        in

        let main2 : T.term = 
            let att_system = Atom.fresh "system" in
            auto_fplace {
                T.annotations = [];
                decorators = [];
                v = T.MethodDeclaration (auto_fplace {
                    T.annotations = [T.Visibility T.Public];
                    decorators = [];
                    v = {
                        T.ret_type = auto_fplace T.TUnknown;
                        name = main_client_name;
                        args = [
                            auto_fplace (T.TRaw "String"), host;
                            auto_fplace (T.TRaw "int"), port;
                            auto_fplace (T.TRaw "akka.actor.ActorSystem"), att_system;
                        ];
                        throws = [];
                        is_constructor = true;
                        body = AbstractImpl ([
                            auto_fplace(T.TemplateStmt({|
        this.{{system}} = {{att_system}};
                            |}, [
                                "system", Jingoo.Jg_types.Tstr (Atom.to_string system);
                                "att_system", Jingoo.Jg_types.Tstr (Atom.to_string att_system);
                            ]));
                            (* configure the client by code *)
                            auto_fplace(T.LetStmt(
                                auto_fplace (T.TRaw "GrpcClientSettings"),
                                settings,
                                Some(T_A2.e2_e(T.AccessExpr(
                                    T_A2.e2_e (T.CallExpr(
                                        T_A2.e2_e (T.RawExpr "GrpcClientSettings.connectToServiceAt"),
                                        [
                                            T_A2.e2var host;
                                            T_A2.e2var port;
                                            e_system
                                        ]
                                    )),
                                    T_A2.e2_e (T.RawExpr "withTls(false)")
                                )))
                            ));
                        ]
                        @ List.map (function service -> 
                            (* create the client *)
                            auto_fplace(T.TryStmt(
                                auto_fplace(T.AssignExpr(
                                    T_A2.e2_e(T.AccessExpr(T_A2.e2_e T.This, T_A2.e2var service.let_client_name)),
                                    T_A2.e2_e (T.CallExpr(
                                        T_A2.e2_e (T.AccessExpr(
                                            T_A2.e2var service.client_name,
                                            T_A2.e2_e (T.RawExpr "create")
                                        )),
                                        [e_settings; e_system]
                                    ))
                                )),
                                [
                                    (auto_fplace (T.TRaw "Exception"), Atom.builtin "e", auto_fplace (T.RawStmt "System.out.println(e);"))
                                ]
                            ))
                        ) services)
                    }
                })  
            }
        in

        let generate_api_method service rpc : T.term = 
            let request = Atom.fresh "request" in
            let reply = Atom.fresh "reply" in
            let in_msg = 
                try Hashtbl.find grpc_messages rpc.in_type
                with Not_found -> raise (Error.DeadbranchError "rpc in_type must be registered into grpc_messages")
            in
            let out_msg = 
                try Hashtbl.find grpc_messages rpc.out_type
                with Not_found -> raise (Error.DeadbranchError "rpc out_type must be registered into grpc_messages")
            in




            (* 
                HelloRequest.newBuilder().setName("Alice").build()   
                this method apply setName("Alice") like for each field of the message on the previously build HelloRequest.newBuilder() as acc
            *)
            let aux (acc:T.expr) = 
                List.fold_left (fun (acc:T.expr) (field:msg_field) ->
                    T_A2.e2_e (T.CallExpr(
                        T_A2.e2_e (T.AccessExpr(
                            acc,
                            T_A2.e2_e(T.RawExpr ("set"^(String.capitalize_ascii (Atom.to_string field.name))))
                        )),
                        [ T_A2.e2var field.name ]
                    ))    
                ) acc in_msg.fields
            in

            auto_fplace (auto_annote(T.MethodDeclaration (
            auto_fplace(auto_annote {
                T.ret_type = (
                    match out_msg.fields with
                    | [f] -> f.ctype
                    | _ -> raise (Error.DeadbranchError "out_msg should have exactly one field, the rpc return value type")
                );
                name = rpc.m.value.name;
                args = List.map (function f -> (f.ctype, f.name)) in_msg.fields;
                is_constructor = false;
                throws = [
                    Atom.builtin "InterruptedException"; 
                    Atom.builtin "ExecutionException";
                    Atom.builtin "TimeoutException";
                ];
                body = T.AbstractImpl [
                    auto_fplace(
                        T.LetStmt(
                            auto_fplace(T.TVar rpc.in_type),
                            request,
                            Some(
                                T_A2.e2_e(T.AccessExpr(
                                    aux(T_A2.e2_e(T.AccessExpr(
                                        T_A2.e2var rpc.in_type,
                                        T_A2.e2_e (T.RawExpr "newBuilder()")
                                    ))),
                                    T_A2.e2_e (T.RawExpr "build()")
                                ))
                            )
                        )
                    );
                    (* CompletionStage<HelloReply> reply = client.sayHello(request); *)
                    auto_fplace(T.LetStmt(
                        auto_fplace(T.TParam (
                            auto_fplace (T.TRaw "CompletionStage"),
                            [ auto_fplace (T.TVar rpc.out_type) ]
                        )),
                        reply,
                        Some (
                            T_A2.e2_e(T.CallExpr(
                                T_A2.e2_e(T.AccessExpr(
                                    T_A2.e2_e(T.AccessExpr(T_A2.e2_e T.This, T_A2.e2var service.let_client_name)),
                                    T_A2.e2var rpc.name
                                )),
                                [ T_A2.e2var request]
                            ))
                        )
                    ));
                    (* Return reply *)
                    auto_fplace(T.ReturnStmt(
                        T_A2.e2_e(T.AccessExpr(
                            T_A2.e2_e(T.AccessExpr(
                                T_A2.e2var reply,
                                T_A2.e2_e (T.RawExpr "toCompletableFuture().get(5, TimeUnit.SECONDS)")
                            )),
                            (match out_msg.fields with
                            | [f] -> T_A2.e2_e (T.RawExpr ("get"^(String.capitalize_ascii (Atom.to_string f.name)^"()")))
                            | _ -> raise (Error.DeadbranchError "out_msg should have exactly one field, the rpc return value type")
                            )
                        ))
                    ))
                ];
            }))))
        in

        let api_methods : T.term list = 
            List.flatten (List.map (function service -> List.map (generate_api_method service) service.rpcs) services)
        in

        auto_fplace {
            T.annotations = [];     
            decorators = [];
            v = T.ClassOrInterfaceDeclaration {
                headers = [
                    "import akka.grpc.GrpcClientSettings;";
                    "import java.util.concurrent.CompletionStage;";
                    "import java.util.concurrent.TimeUnit;";
                    "import java.util.concurrent.TimeoutException;";
                ]
                @ List.map (function service -> 
                    Printf.sprintf 
                        "import %s.%s.grpc.*;"
                        (Config.author ())
                        (Config.project_name ())
                ) services;
                isInterface = false;
                name = main_client_name; 
                extended_types = [];
                implemented_types = [];
                body = states @ (main :: main2 :: api_methods) @ [
                    auto_fplace {
                        T.annotations = [];
                        decorators = [];
                        v = T.RawTerm (auto_fplace {
                            T.language = None;
                            body = [
                                T.Template ({|
    public void disconnect() {
        this.{{system}}.terminate();
    }
                                |}, [("system", Jg_types.Tstr (Atom.to_string system))])
                            ]
                        })
                    }
                ] 
            }
        }

    let finish_program program =  
        hydrate_grpc program;
        if Hashtbl.length grpc_services > 0 then
        begin
            proto_models := Some (generate_protobuf_interfaces_env build_dir program);
            let iri_program, res = generate_services_implementation program in
            let events, akka_terms = List.split res in 
            let events = List.flatten events in
            let target, server_program = generate_gRPC_server (List.of_seq (Hashtbl.to_seq_values grpc_services)) in
            let client_program = generate_gRPC_client (List.of_seq (Hashtbl.to_seq_values grpc_services)) in
            [ (target, iri_program), events@akka_terms@[server_program;client_program] ]
        end
        else  
        begin
            proto_models := Some [];
            [ (target, program), []]
        end

    (*****************************************************)
    let name = "Akka.Interfaces.GRPC"
    let displayed_pass_shortdescription = Printf.sprintf "Interface gRPC" 
    let displayed_ast_name = "Akka AST + gRPC interfaces"
    let show_ast = true
    let global_at_most_once_apply = true 

    let precondition program = program
    let postcondition program = program
    let apply_program = finish_program
end