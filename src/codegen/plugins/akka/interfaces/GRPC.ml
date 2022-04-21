(* TODO create interface/GRPC where interface/ list interface generation plugin with external world *)

open Core
open IRI
open IRI.IRUtils

open Jingoo

let name = "Interface<Akka,gRPC>"

module Make (Arg : sig
    val build_dir : Fpath.t 
end) = struct
    let build_dir = Arg.build_dir
    let [grpc_templates_location] =  Mysites.Sites.akka_interfaces_grpc
    (* TODO add proto into module not in template*)
    let proto_template = Fpath.to_string (List.fold_left Fpath.add_seg (Fpath.v grpc_templates_location) [ "grpc"; "proto.j2"])

    module S = IRI

    (* Stage 1 - generate protobuf interfaces in src/main/proto *)
    let generate_protobuf_interfaces build_dir program =
        let protofile = List.fold_left Fpath.add_seg build_dir ["src"; "main"; "proto"] in

        let selector = function _ -> true in
        
        (* for each component returns (component_name, expose methods) *)
        let collector _ _ = function
            | Component {value= ComponentStructure cstruct} -> 
                [(cstruct.name, List.filter_map (function 
                    | {value= Method m} when List.mem S.Expose m.value.annotations -> Some m
                    | _ -> None
                ) cstruct.body)]
            | _ -> []
        in

        let collected_elts = collect_term_program true selector collector program in


        (*let services = List.map (function (name, exported_methods) ->
            failwith "TODOXX"
        ) collected_elts in*)
        let services = [] in
        let messages = [] in

        let models = [
            ("project_name", Jg_types.Tstr (Config.project_name ()));
            ("author", Jg_types.Tstr (Config.author ()));

            ("services", Jg_types.Tlist services);
            ("msgs", Jg_types.Tlist messages)
        ] in

        Jg_template.from_file proto_template ~models:models


    let finish_program program =  
        generate_protobuf_interfaces build_dir program;
        program

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