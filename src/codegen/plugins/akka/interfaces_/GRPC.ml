(* TODO create interface/GRPC where interface/ list interface generation plugin with external world *)

open Core
open AstUtils

open Jingoo

let name = "gRPC"

let fplace = (Error.forge_place ("Plg=Akka/interfaces/"^name) 0 0)
let auto_fplace smth = {place = fplace; value=smth}

module Make (Arg : sig
    val build_dir : Fpath.t 
end) = struct
    let build_dir = Arg.build_dir
    let [grpc_templates_location] =  Mysites.Sites.akka_interfaces_grpc
    (* TODO add proto into module not in template*)
    let proto_template = Fpath.to_string (List.fold_left Fpath.add_seg (Fpath.v grpc_templates_location) [ "grpc"; "proto.j2"])

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
                        | TInt -> "int64"
                        | TFloat -> "float"
                        | TBool -> "bool"
                        | TVoid -> "NullValue"
                        | _ -> Error.error place "Type unsupported for interface, should be a simple atomic type."
                    end
                    | S.CType {value=S.TDict (mt1, mt2)} ->
                        Printf.sprintf "map<%s,%s>" (gendef mt1)(gendef mt2)
                    | S.CType {value=S.TList _} -> "ListValue"
                    | _ -> Error.error place "Type unsupported for interface, should be a simple atomic type."
                and gendef x = map0_place _gendef x in

                let to_field i ({value=mt,x}) : msg_field = 
                    { 
                        name = Atom.fresh (Atom.value x);
                        protobuf_type = gendef mt;
                        protobuf_id = i+1; 
                    }
                in 

                let in_msg = {
                    name = Atom.fresh "ProtoMsg";
                    fields = List.mapi to_field m.value.args;
                } in
                let out_msg = {
                    name = Atom.fresh "ProtoMsg";
                    fields = List.mapi to_field [auto_fplace (m.value.ret_type, Atom.fresh "ret_value")];
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
                let service = {
                    component_name;
                    service_name = Atom.fresh ((Atom.value component_name)^"Service");
                    impl_name = Atom.fresh ((Atom.value component_name)^"ServiceImpl"); 
                    rpcs = rpcs;
                } in
                Hashtbl.add grpc_services service.service_name service
        ) collected_elts

    (* Stage 1 - Generating protobuf interfaces and stubs 
        in src/main/protobuf/*.proto
    *)
    let generate_protobuf_interfaces build_dir program =
        let protodir = List.fold_left Fpath.add_seg build_dir ["src"; "main"; "protobuf"] in
        Core.Utils.refresh_or_create_dir protodir;
        (* TODO create multiple file *)
        let protofile = Fpath.add_seg protodir "proto.proto" in

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
        
        let models = [
            ("project_name", Jg_types.Tstr (Config.project_name ()));
            ("author", Jg_types.Tstr (Config.author ()));

            ("services", Jg_types.Tlist services);
            ("msgs", Jg_types.Tlist messages)
        ] in

        let res = Jg_template.from_file proto_template ~models:models in
        let oc = open_out (Fpath.to_string protofile) in
        Printf.fprintf oc "%s" res; 
        close_out oc

    (* Stage 2 - Implementing the services *)
    let generate_service_implementation service =
        let att_system = Atom.fresh "system" in
        let att_actor = Atom.fresh "actor" in
        let constructor_arg_system = Atom.fresh "system" in

        let body_rpcs = List.map (function rpc ->
            let ct_msg_in = auto_fplace (T.TVar rpc.in_type) in
            let a_msg_in = Atom.fresh "in" in
            let ct_msg_out = auto_fplace (T.TVar rpc.out_type) in 

            let a_intermediate_msg = Atom.fresh "message" in

            auto_fplace {
                T.annotations = [];
                decorators = [];
                v = T.MethodDeclaration (auto_fplace {
                    T.annotations = [T.Visibility T.Public];
                    decorators = [];
                    v = {
                        T.ret_type = auto_fplace (T.TParam (auto_fplace (T.TRaw "CompletionStage"), [ct_msg_out]));
                        name = service.impl_name;
                        args = [ (ct_msg_in, a_msg_in) ];
                        is_constructor = false;
                        body = AbstractImpl [
                            auto_fplace (T.ReturnStmt(
                                auto_fplace (T.CallExpr(
                                    auto_fplace (T.AccessExpr(
                                        (* ask(greeterActor, GreeterActor.GET_GREETING, Duration.ofSeconds(5)) *)
                                        auto_fplace(T.CallExpr( 
                                            auto_fplace( T.RawExpr "ask", auto_fplace T.TUnknown),
                                            [
                                                auto_fplace (T.AccessExpr(
                                                    auto_fplace (T.This, auto_fplace T.TUnknown), 
                                                    auto_fplace (T.VarExpr att_actor, auto_fplace T.TUnknown)
                                                ), auto_fplace T.TUnknown);
                                                (* TODO *)
                                            ]
                                        ),auto_fplace T.TUnknown),
                                        auto_fplace( T.RawExpr "thenApply", auto_fplace T.TUnknown)
                                    ), auto_fplace T.TUnknown),
                                    [
                                        (* message ->
          HelloReply.newBuilder()
            .setMessage(((GreeterActor.Greeting) message).greeting)
            .build() *)
                                        auto_fplace (T.LambdaExpr(
                                            [
                                                (* TODO should be an event of component_name *)
                                                (auto_fplace T.TUnknown, a_intermediate_msg)
                                            ],
                                            auto_fplace (T.ReturnStmt(
                                                auto_fplace( T.CallExpr (
                                                    auto_fplace( T.AccessExpr(
                                                        auto_fplace( T.CallExpr (
                                                            auto_fplace( T.AccessExpr(
                                                                auto_fplace( T.CallExpr (
                                                                    auto_fplace (T.AccessExpr(
                                                                        auto_fplace (T.VarExpr a_msg_in, auto_fplace T.TUnknown),
                                                                        auto_fplace (T.RawExpr "newBuilder", auto_fplace T.TUnknown)
                                                                    ), auto_fplace T.TUnknown),
                                                                    []
                                                                ), auto_fplace T.TUnknown),
                                                                auto_fplace (T.RawExpr "setMessage", auto_fplace T.TUnknown)
                                                            ), auto_fplace T.TUnknown),
                                                            [
                                                                auto_fplace (T.CastExpr(
                                                                    auto_fplace (T.TRaw "EventTODO"),
                                                                    auto_fplace (T.VarExpr a_intermediate_msg, auto_fplace T.TUnknown)
                                                                ), auto_fplace T.TUnknown)

                                                            ]
                                                        ), auto_fplace T.TUnknown),
                                                        auto_fplace (T.RawExpr "build", auto_fplace T.TUnknown)
                                                    ), auto_fplace T.TUnknown),
                                                    []
                                                ), auto_fplace T.TUnknown)
                                            ))
                                        ), auto_fplace T.TUnknown)
                                    ]
                                ), auto_fplace T.TUnknown)
                            ))
                        ]
                    }
                });
            }
        ) service.rpcs in


        auto_fplace {
            T.annotations = [];     
            decorators = [];
            v = T.ClassOrInterfaceDeclaration {
                isInterface = false;
                name = service.impl_name; 
                extended_types = [];
                implemented_types = [ auto_fplace (T.TVar service.service_name) ];
                body = [
                    auto_fplace {
                        T.annotations = [T.Visibility T.Private; T.Final];
                        decorators = [];
                        v = T.Stmt (auto_fplace (T.LetStmt (
                            auto_fplace (T.TRaw "ActorSystem"), 
                            att_system,
                            None
                        )));
                    };
                    auto_fplace {
                        T.annotations = [T.Visibility T.Private; T.Final];
                        decorators = [];
                        v = T.Stmt (auto_fplace (T.LetStmt (
                            auto_fplace (T.TRaw "ActorRef"), 
                            att_actor,
                            None
                        )));
                    };
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
                                body = T.AbstractImpl [
                                    auto_fplace (T.AssignExpr(
                                        auto_fplace(T.AccessExpr(
                                            auto_fplace(T.This, auto_fplace T.TUnknown),
                                            auto_fplace (T.VarExpr att_system, auto_fplace T.TUnknown)
                                        ), auto_fplace T.TUnknown),
                                        auto_fplace(T.VarExpr constructor_arg_system, auto_fplace T.TUnknown)
                                    ));
                                    (* this.greeterActor = system.actorOf(GreeterActor.props("Hello"), "greeter"); *)
                                    auto_fplace (T.AssignExpr(
                                        auto_fplace(T.AccessExpr(
                                            auto_fplace(T.This, auto_fplace T.TUnknown),
                                            auto_fplace (T.VarExpr att_actor, auto_fplace T.TUnknown)
                                        ), auto_fplace T.TUnknown),
                                        auto_fplace(T.CallExpr (
                                            auto_fplace(T.AccessExpr(
                                                auto_fplace(T.This, auto_fplace T.TUnknown),
                                                auto_fplace (T.VarExpr constructor_arg_system, auto_fplace T.TUnknown)
                                            ), auto_fplace T.TUnknown),
                                            [
                                                auto_fplace(T.CallExpr (
                                                    auto_fplace(T.AccessExpr(
                                                        auto_fplace (T.VarExpr service.component_name, auto_fplace T.TUnknown),
                                                        auto_fplace(T.RawExpr "props", auto_fplace T.TUnknown)
                                                    ), auto_fplace T.TUnknown),
                                                    [
                                                        (* FIXME args for actor creation *)
                                                    ]
                                                ), auto_fplace T.TUnknown);
                                                auto_fplace(T.LitExpr (auto_fplace(T.StringLit (Atom.to_string service.component_name))), auto_fplace T.TUnknown)
                                            ]
                                        ), auto_fplace T.TUnknown)
                                    ))
                                ]
                            }
                        });
                    };

                ] @ body_rpcs;
            }
        }


    (**
        @return [ Akka program of the ServiceImpl ]
    *)
    let generate_services_implementation () =
        List.map generate_service_implementation (List.of_seq (Hashtbl.to_seq_values grpc_services))

    (* Stage 3 - generate and bind to the HTTP part *)


    let finish_program program : T.program =  
        hydrate_grpc program;
        generate_protobuf_interfaces build_dir program;
        (generate_services_implementation ())

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