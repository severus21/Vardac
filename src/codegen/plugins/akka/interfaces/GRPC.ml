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

    (**************************************************)

    module S = IRI
    module T = Ast

    type msg_field = {
        name: Atom.t;
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
                        | TStr -> "string"
                        | TInt -> "int32" (* since no long during conversion*)
                        | TFloat -> "float"
                        | TBool -> "bool"
                        | TVoid -> "NullValue"
                        | _ -> Error.perror place "Type unsupported for interface, should be a simple atomic type."
                    end
                    | S.CType {value=S.TDict (mt1, mt2)} ->
                        Printf.sprintf "map<%s,%s>" (gendef mt1)(gendef mt2)
                    | S.CType {value=S.TList _} -> "ListValue"
                    | _ -> Error.perror place "Type unsupported for interface, should be a simple atomic type."
                and gendef x = map0_place _gendef x in

                let to_field i ({value=mt,x}) : msg_field = 
                    { 
                        name = Atom.fresh (Atom.value x);
                        protobuf_type = gendef mt;
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
                let service = {
                    component_name;
                    service_name = service_name;
                    impl_name = Atom.fresh ((Atom.value component_name)^"ServiceImpl"); 
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



            let service2actor_event = auto_fplace {
                T.vis = T.Public;
                name = Atom.fresh "Service2Actor";
                args = [ 
                    (ct_msg_in, Atom.fresh "value");
                    (auto_fplace (T.Atomic "ActorRef"), Atom.fresh "replyTo")
                ];
                headers = [ (Printf.sprintf "import %s.%s.grpc.*;" (Config.author ()) (Config.project_name ()))];
            } in
            let actor2service_event = auto_fplace {
                T.vis = T.Public;
                name = Atom.fresh "Actor2Service";
                args = [ 
                    (ct_msg_out, Atom.fresh "value");
                    (auto_fplace (T.ActorRef (auto_fplace (T.TVar service.component_name))), Atom.fresh "replyTo")
                ];
                headers = [ (Printf.sprintf "import %s.%s.grpc.*;" (Config.author ()) (Config.project_name ()))];
            } in
            service2actor_events := (
                rpc.m, 
                (service2actor_event, Hashtbl.find grpc_messages rpc.in_type), (actor2service_event, Hashtbl.find grpc_messages rpc.out_type)
            ) :: !service2actor_events; 


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
                        is_constructor = false;
                        body = AbstractImpl [
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
                                            T_A2.e2_e (T.CallExpr (
                                                T_A2.e2_e (T.AccessExpr(
                                                    T_A2.e2var a_intermediate_msg,
                                                    T_A2.e2_e (T.RawExpr "_0_")
                                                )),
                                                []
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
                                                                [ 
                                                                    T_A2.e2var a_msg_in;
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
                                            S_A2.e2_e (S.RawExpr ("_1_()"))
                                        )),
                                        S_A2.e2_e (S.RawExpr ("tell"))
                                    )),
                                    [
                                        S_A2.e2_e (S.NewExpr(
                                            S_A2.e2var e2.value.name,
                                            [
                                                (*HelloReply.newBuilder()
                                                .setY_i(res).build*)
                                                S_A2.e2_e (S.CallExpr(
                                                    S_A2.e2_e (S.AccessExpr(
                                                        S_A2.e2_e (S.CallExpr(
                                                            S_A2.e2_e (S.AccessExpr(
                                                                S_A2.e2_e (S.AccessExpr(
                                                                    S_A2.e2var proto_msg2.name,
                                                                    S_A2.e2_e (S.RawExpr ("newBuilder()"))
                                                                )),
                                                                match proto_msg2.fields with
                                                                | [f] -> 
                                                                    S_A2.e2_e (S.RawExpr (Printf.sprintf "set%s" (String.capitalize_ascii (Atom.to_string f.name))))
                                                                | _ -> failwith "wrong number of gRPC message fields for response"
                                                            )),
                                                            [ 
                                                                S_A2.e2_e (S.CallExpr(
                                                                    S_A2.e2_e (S.AccessExpr( 
                                                                        S_A2.e2_e S.This, 
                                                                        S_A2.e2var m.value.name
                                                                    )),
                                                                    List.map (function (f:msg_field) -> 
                                                                        S_A2.e2_e (S.CallExpr(
                                                                            S_A2.e2_e (S.AccessExpr( 
                                                                                S_A2.e2_e (S.AccessExpr( 
                                                                                    S_A2.e2var lambda_e,
                                                                                    S_A2.e2_e (S.RawExpr "_0_()")
                                                                                )),
                                                                                S_A2.e2_e (S.RawExpr (
                                                                                    Printf.sprintf "get%s" (String.capitalize_ascii (Atom.to_string f.name))
                                                                                ))
                                                                            )),
                                                                            []
                                                                        ))
                                                                    ) proto_msg1.fields
                                                                ))
                                                            ]
                                                        )),
                                                        S_A2.e2_e (S.RawExpr ("build"))
                                                    )),
                                                    []
                                                ));
                                                S_A2.e2_e (S.RawExpr ("getContext().getSelf()"))
                                            ]
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

        ActorRef actor = null;

        Cluster cluster = Cluster.get({{actorSystem}});
        assert (null != cluster);
        ServiceKey key = PlaceDiscovery.activationsServiceKeyOf(cluster.selfMember().address());
        CompletionStage<Receptionist.Listing> result =
                AskPattern.ask({{actorSystem}}.receptionist(),
                        (ActorRef<Receptionist.Listing> replyTo) -> Receptionist.find(key, replyTo),
                        DEFAULT_TIMEOUT,
                        {{actorSystem}}.scheduler());

        try {
            // blocking call
            Set<ActorRef<{{componentName}}.Command>> listing = result.toCompletableFuture().get().getServiceInstances(key);
            if (listing.isEmpty()) {
                if (++{{retry}} < DEFAULT_MAX_RETRY + 1) {
                    final long timeout = DEFAULT_RETRY_TIMEOUT * {{retry}};
                    {{actorSystem}}.log().info("{{implName}}::getActor() retry " + {{retry}} + "/" + DEFAULT_MAX_RETRY + ", timeout=" + timeout);
                    // sleep and retry
                    Thread.sleep(timeout);
                    {{getActor}}({{actorSystem}}, {{retry}});
                } else {
                    throw new RuntimeException("Could not find TransactionManager after " + DEFAULT_MAX_RETRY + " retries.");
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

        (* TODO remove it
        let spawn_materializer () = auto_fplace(
            T.LetStmt (
                auto_fplace (T.TRaw "Materializer"),
                local_mat,
                Some (T_A2.e2_e (T.CallExpr( 
                    T_A2.e2_e (T.AccessExpr( 
                        T_A2.e2_e (T.CallExpr( 
                            T_A2.e2_e (T.RawExpr "SystemMaterializer.get"),
                            [ T_A2.e2var att_system ]
                        )),
                        T_A2.e2_e (T.RawExpr "materializer")
                    )),
                    []
                )))
            )
        ) in*)

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

    let finish_program program =  
        hydrate_grpc program;
        proto_models := Some (generate_protobuf_interfaces_env build_dir program);
        let iri_program, res = generate_services_implementation program in
        let events, akka_terms = List.split res in 
        let events = List.flatten events in
        let target, akka_term = generate_gRPC_server (List.of_seq (Hashtbl.to_seq_values grpc_services)) in
        [ (target, iri_program), events@akka_terms@[akka_term] ]

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