open Core
open AstUtils
open IR
open Easy_logging

let logger = Logging.make_logger ("_1_ compspec.derive") Debug [];;

let fplace = (Error.forge_place "Plg=AkkaJava/derive_rpc" 0 0) 
let auto_fplace smth = {place = fplace; value=smth}


(* load mtype_of_... *)
include AstUtils2.Mtype.Make(struct let fplace = fplace end)

type method_rpc_entry = {
    method_name: Atom.atom;
    e_call: Atom.atom;
    e_call_def: term;
    e_ret: Atom.atom;
    e_ret_def: term;
    l_selector: Atom.atom;
    expecting_st: session_type;
    port: component_item;
    callback: component_item;
}


(*
    cname: component to derive 
*)
let derive_program program cname = 
    let rpc_entries = Hashtbl.create 8 in
    let a_rpc_protocol = Atom.fresh ((Atom.to_string cname)^"__rpc_protocol") in
    let mt_rpc_protocol = mtype_of_var a_rpc_protocol in
    let a_rpc_bridge = Atom.fresh ((Atom.to_string cname)^"__rpc_bridge") in
   
    (* TODO list of all components that can call an rpc method *)
    let callers : Atom.atom list = [] in 
    (* TODO check non empty*)
    let caller0::callers = callers in


    let mt_rpc_bridge = mtype_of_ct (TBridge {
        in_type = List.fold_left (fun mt name -> mtype_of_ct (TUnion (mt, mtype_of_cvar name))) (mtype_of_cvar caller0) callers; 
        out_type = mtype_of_cvar cname; 
        protocol = mt_rpc_protocol
    }) in
    let let_rpc_bridge = auto_fplace (LetExpr (
        mt_rpc_bridge, a_rpc_bridge, auto_fplace (VarExpr (Atom.fresh_builtin "bridge"), mt_rpc_bridge)
    )) in

    (* Rewrite the component *)
    let cstruct_selector (cstruct:component_structure) = cname = cstruct.name in
    let cstruct_rewriter place (cstruct:component_structure) =
        (* TODO add annotations to restrict method concerned by rpc *)
        let rpc_methods = List.flatten (List.map (function |{value=Method m} -> [m] |_ -> []) cstruct.body) in
        
        let process_method (m:method0) =
            (* Events generation -> should be used outside *)
            let prefix = Printf.sprintf "%s__%s__" (Atom.to_string cname) (Atom.to_string m.value.name) in
            let e_call = Atom.fresh (prefix^"call") in
            let mt_call = mtype_of_var e_call in
            let e_ret = Atom.fresh (prefix^"ret") in
            let mt_ret = mtype_of_var e_ret in
            
            let _expecting_st2 = auto_fplace (STRecv (
                mt_ret,
                auto_fplace STEnd
            )) in
            let _expecting_st = auto_fplace (STSend (
                mt_call,
                _expecting_st2
            )) in



            (* RPC Callback *)
            let a_event = Atom.fresh "e" in
            let a_session = Atom.fresh "s" in
            let a_tmp = Atom.fresh "tmp" in
            let mt_event = mt_call  in
            let callback  = auto_fplace { 
                ghost = false;
                ret_type = mtype_of_ft TVoid;
                name =  Atom.fresh (prefix^"callback");
                args = [ 
                    auto_fplace(mt_event, a_event); 
                    auto_fplace (mtype_of_st (dual _expecting_st2).value, a_session)
                ];
                body = [
                    (* T tmp = this.method(e._0_, e._1_, ...); *)
                    auto_fplace(LetExpr (
                        m.value.ret_type, 
                        a_tmp, 
                        auto_fplace(CallExpr(
                            auto_fplace (VarExpr m.value.name, m.value.ret_type), (* TODO type should be a signature*)
                            List.mapi (fun i {value=(mt_arg,_)} -> 
                                auto_fplace(AccessExpr (
                                    auto_fplace (VarExpr a_event, mt_event), 
                                    auto_fplace (VarExpr (Atom.fresh_builtin (Printf.sprintf "_%d_" i)), mt_arg) 
                                ), mt_arg)    
                            ) m.value.args
                        ), m.value.ret_type)
                    )); 
                    (* fire(s, ret(tmp)) *)
                    auto_fplace(ExpressionStmt(
                        auto_fplace(CallExpr(
                            auto_fplace (VarExpr e_ret, mt_ret),
                            [ auto_fplace (VarExpr a_tmp, m.value.ret_type) ]
                        ), mtype_of_st STEnd)
                    ))
                ];
                contract_opt = None;
                on_destroy = false;
                on_startup = false;
            } in
            let callback_sign = 
                List.fold_right (fun {value=(mt1,_)} mt2 -> mtype_of_ct (TArrow (mt1, mt2))) m.value.args m.value.ret_type
            in

            (* One port per method*)
            let mt_port = mtype_of_ct (TPort (
                mtype_of_st _expecting_st.value,
                mt_rpc_bridge
            )) in
            let a_port = Atom.fresh (prefix^"port") in
            let port = auto_fplace (Port(auto_fplace ({
                    name = a_port;
                    input = auto_fplace (VarExpr a_rpc_bridge, mt_rpc_bridge);
                    expecting_st = mtype_of_st (dual _expecting_st).value;
                    callback = auto_fplace (AccessExpr(
                        auto_fplace (This, mtype_of_ct (TActivationInfo (mtype_of_cvar cname))), 
                        auto_fplace (VarExpr callback.value.name, callback_sign)
                    ), callback_sign); 
                }, mt_port))) in

            let entry = {
                method_name = m.value.name;
                e_call;
                e_call_def = auto_fplace (Typedef (auto_fplace (EventDef (e_call, List.map (function {value} -> fst value) m.value.args, ()))));
                e_ret;
                e_ret_def = auto_fplace (Typedef (auto_fplace (EventDef (e_ret, [m.value.ret_type], ()))));


                l_selector = Atom.fresh (prefix^"label");

                expecting_st = _expecting_st; 
                port = port;
                callback = auto_fplace (Method callback);
            } in
            Hashtbl.add rpc_entries m.value.name entry

        in
        List.iter process_method rpc_methods;


        let prefix = Printf.sprintf "%s__" (Atom.to_string cname) in
        (* RPC Inner port *)
        let full_expecting_st = auto_fplace (STBranch (List.of_seq (Seq.map (function entry ->
            (entry.l_selector, entry.expecting_st, None)    
        ) (Hashtbl.to_seq_values rpc_entries)))) in

        (* Utils *)
        let map_values fct =  (List.of_seq (Seq.map fct (Hashtbl.to_seq_values rpc_entries))) in

        let cstruct = {cstruct with
            body = 
            map_values (function (entry:method_rpc_entry) -> entry.callback) @
            map_values (function entry -> entry.port) @
            cstruct.body 
        } in
      

        (*  FIXME where to put things 
            generate bridge top level ??? -> nop because of implicit 
        *)
        let top_level_terms = 
            auto_fplace (Stmt let_rpc_bridge) ::
            map_values (function entry -> entry.e_call_def) @
            map_values (function entry -> entry.e_ret_def)
        in 

        [cstruct]
    in


        (* Bridge should be used outside maybe an issue in multi JVM*)
            (* Protocol definition - should be used outside*)
        (*let st_rpc = in
        let rpc_protocol = auto_fplace (Typedef (auto_fplace (ProtocolDef (a_rpc_protocol, st_rpc)))) in*)
    let program : program = rewrite_component_program cstruct_selector cstruct_rewriter program in

    (* Rewrite the remaining part of the code *)
    failwith "TODO RPC derive";

    program
