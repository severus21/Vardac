open Core
open AstUtils
open IR

(*
    cname: component to derive 
*)
let derive_program program cname = 
    let rpc_methods = ref [] in 
    let rpc_events = ref [] in
    let rpc_protocol = ref None in
    let rpc_bridge = ref None in 

    (* Rewrite the component *)
    let cstruct_selector (cstruct:component_structure) = cname = cstruct.name in
    let cstruct_rewriter place (cstruct:component_structure) =
            (* TODO add annotations to restrict method concerned by rpc *)
            rpc_methods := List.map (function |{value=Method m} -> [m] |_ -> []) cstruct.body; 


        (* Events generation -> should be used outside *)
        (* Protocol definition - should be used outside*)
        (* Bridge should be used outside maybe an issue in multi JVM*)

        (* RPC Inner port *)

        (* RPC Callback *)

        [cstruct]
    in
    let program : program = rewrite_component_program cstruct_selector cstruct_rewriter program in

    (* Rewrite the remaining part of the code *)
    failwith "TODO RPC derive"

    program
