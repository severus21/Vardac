(** Compile away and apply RPC derivation *)

open Core
open AstUtils
open IR
open Easy_logging
open IRMisc
open Common
 

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
    local_function: Atom.atom -> function_dcl
}

module type Args = sig
    (* cname: component to derive *)
    val cname : Atom.atom
end

module type Sig = sig
    include IRCompilationPass.Pass
end

module Make(Args:Args) : Sig = struct
    include Args 

    (* cname -> rpc outputport name *)
    let rpc_outports_translator = Hashtbl.create 16 

    let derive_program program = 
        let rpc_entries = Hashtbl.create 8 in
        let a_rpc_protocol = Atom.fresh ((Atom.to_string cname)^"__rpc_protocol") in
        let mt_rpc_protocol = mtype_of_svar a_rpc_protocol in
        let a_rpc_bridge = Atom.fresh ((Atom.to_string cname)^"__rpc_bridge") in
    
        (* Collect list of all components that can call an rpc method *)
        let select_caller = function
            | ActivationAccessExpr (_cname, _, _) -> cname =_cname
            | _ -> false
        in
        let collect_caller parent_opt _ _ = [parent_opt] in 
        let _, callers, _ = collect_expr_program Atom.Set.empty select_caller collect_caller program in
        let callers : Atom.atom list = List.map Option.get (List.filter (function x -> x <> None) callers) in 
        (if callers = [] then 
            Error.perror fplace "There is no RPC call for %s" (Atom.value cname) (* FIXME compute the correct place to improve error reporting *)
        );
        let caller0::callers = callers in


        (* Populate the rpc_outports_translator, after this it is read only *)
        List.iter (function caller_name ->
            Hashtbl.add rpc_outports_translator caller_name (Atom.fresh ((Atom.to_string caller_name)^"__rpc_outport"))
        ) callers;


        let mt_rpc_bridge = mtype_of_ct (TBridge {
            in_type = List.fold_left (fun mt name -> mtype_of_ct (TUnion (mt, mtype_of_cvar name))) (mtype_of_cvar caller0) callers; 
            out_type = mtype_of_cvar cname; 
            protocol = mt_rpc_protocol
        }) in
        let let_rpc_bridge = auto_fplace (LetStmt ( (* Create a static bridge to be able to autocross target boundaries *)
            mt_rpc_bridge, a_rpc_bridge, auto_fplace (LitExpr (auto_fplace (StaticBridge{
                id = Atom.fresh (Atom.value a_rpc_bridge);
                protocol_name = a_rpc_protocol;
            })), mt_rpc_bridge)
        )) in


        let top_level_terms = ref [] in

        (* Rewrite the component *)
        let cstruct_selector (cstruct:component_structure) = cname = cstruct.name in
        let cstruct_rewriter place (cstruct:component_structure) =
            (* TODO add annotations to restrict method concerned by rpc *)
            let rpc_methods = List.flatten (List.map (
                function 
                |{value=Method m} when false = (m.value.on_startup || m.value.on_destroy) -> [m] 
                |_ -> []
            ) cstruct.body) in

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
                    annotations = [];
                    ghost = false;
                    ret_type = mtype_of_ft TVoid;
                    name =  Atom.fresh (prefix^"callback");
                    args = [ 
                        auto_fplace(mt_event, a_event); 
                        auto_fplace (mtype_of_st (dual _expecting_st2).value, a_session)
                    ];
                    body = [
                        (* T tmp = this.method(e._0_, e._1_, ...); *)
                        auto_fplace(LetStmt (
                            m.value.ret_type, 
                            a_tmp, 
                            auto_fplace(CallExpr(
                                auto_fplace (VarExpr m.value.name, m.value.ret_type), (* TODO type should be a signature*)
                                List.mapi (fun i {value=(mt_arg,_)} -> 
                                    auto_fplace(AccessExpr (
                                        auto_fplace (VarExpr a_event, mt_event), 
                                        auto_fplace (VarExpr (Atom.builtin (Printf.sprintf "_%d_" i)), mt_arg) 
                                    ), mt_arg)    
                                ) m.value.args
                            ), m.value.ret_type)
                        )); 
                        (* fire(s, ret(tmp)) *)
                        auto_fplace(ExpressionStmt(
                            auto_fplace(CallExpr(
                                auto_fplace (VarExpr (Atom.builtin "fire"), auto_fplace EmptyMainType),
                                [
                                    auto_fplace (VarExpr a_session, auto_fplace EmptyMainType);
                                    auto_fplace (NewExpr(
                                        auto_fplace (VarExpr e_ret, mt_ret),
                                        [ auto_fplace (VarExpr a_tmp, m.value.ret_type) ]
                                    ), auto_fplace EmptyMainType)
                                ]
                            ), mtype_of_st STEnd)
                        ))
                    ];
                    contract_opt = None;
                    on_destroy = false;
                    on_startup = false;
                } in
                let callback_sign = mtype_of_fun m.value.args m.value.ret_type in

                (* One port per method*)
                let mt_port = mtype_of_ct (TInport (
                    mtype_of_st _expecting_st.value
                    (*mt_rpc_bridge*)
                )) in
                let a_port = Atom.fresh (prefix^"port") in
                let port = auto_fplace (Inport(auto_fplace ({
                        name = a_port;
                        (*input = auto_fplace (VarExpr a_rpc_bridge, mt_rpc_bridge);*)
                        expecting_st = mtype_of_st (dual _expecting_st).value;
                        callback = auto_fplace (AccessExpr(
                            auto_fplace (This, mtype_of_ct (TActivationRef (mtype_of_cvar cname))), 
                            auto_fplace (VarExpr callback.value.name, callback_sign)
                        ), callback_sign); 
                        _disable_session = false;
                        _children = [];
                        _is_intermediate = false;
                    }, mt_port))) in


                let l_selector = Atom.fresh (prefix^"label") in 

                (* The function used by the caller to do the communication *)
                let local_function_activation = Atom.fresh "a" in
                let s0 = Atom.fresh "s" in
                let s1 = Atom.fresh "s" in
                let s2 = Atom.fresh "s" in
                let res = Atom.fresh "res" in

                let refreshed_args = (List.map (function {value=(mt, x)} -> auto_fplace (mt, Atom.fresh (Atom.value x))) m.value.args) in
                let local_function_args = 
                    (auto_fplace (mtype_of_ct (TActivationRef (mtype_of_cvar cname)), local_function_activation))::
                    refreshed_args 
                in
                let local_function caller_name = auto_fplace {
                    targs = [];
                    name = Atom.fresh (prefix^"localfct");
                    ret_type = m.value.ret_type;
                    args = local_function_args;
                    body = [
                        (* s0 = initiate_session_with(counter_bridge, this.instance); *)
                        auto_fplace (LetStmt(mt_rpc_protocol, s0, auto_fplace (CallExpr(
                            auto_fplace (VarExpr (Atom.builtin "initiate_session_with"), auto_fplace EmptyMainType),
                            [
                                auto_fplace (
                                    AccessExpr (
                                        auto_fplace( This, auto_fplace EmptyMainType), 
                                        auto_fplace(VarExpr (Hashtbl.find rpc_outports_translator caller_name), auto_fplace EmptyMainType)
                                    ), 
                                    auto_fplace EmptyMainType
                                );
                                auto_fplace (VarExpr local_function_activation, auto_fplace EmptyMainType);
                            ]
                        ), auto_fplace EmptyMainType)));
                        (* s1 = select(s0, counter__incr__label); *)
                        auto_fplace (LetStmt(
                            mtype_of_st _expecting_st.value, s1, 
                            e2_e (UnopExpr(
                                UnpackOrPropagateResult,
                                e2_e (CallExpr(
                                    e2var (Atom.builtin "select"),
                                    [
                                        e2var s0; 
                                        e2_lit (StringLit (Atom.to_string l_selector))
                                    ]
                                ))
                            ))
                        ));
                        (* s2 = fire(s1, counter__incr__call(args_as_tuple))?; *)
                        auto_fplace (LetStmt(mtype_of_st _expecting_st2.value, s2, auto_fplace (CallExpr(
                            auto_fplace (VarExpr (Atom.builtin "fire"), auto_fplace EmptyMainType),
                            [
                                auto_fplace (VarExpr s1, auto_fplace EmptyMainType); 
                                auto_fplace (NewExpr (
                                    auto_fplace (VarExpr e_call, auto_fplace EmptyMainType),
                                    List.map (function x -> auto_fplace (VarExpr (snd x.value), auto_fplace EmptyMainType)) refreshed_args
                                ), auto_fplace EmptyMainType)
                            ]

                        ), auto_fplace EmptyMainType)));
                        (* return first(receive(s2))._0_; *)
                        auto_fplace (ReturnStmt (
                            auto_fplace (AccessExpr (
                                auto_fplace (CallExpr (
                                    auto_fplace (VarExpr (Atom.builtin "first"), auto_fplace EmptyMainType),
                                    [
                                        auto_fplace (CallExpr (
                                            auto_fplace (VarExpr (Atom.builtin "receive"), auto_fplace EmptyMainType),
                                            [
                                                auto_fplace (VarExpr s2, auto_fplace EmptyMainType);
                                                auto_fplace (VarExpr a_rpc_bridge, mt_rpc_bridge)
                                            ]
                                        ), 
                                            (* receive needs a real and correct type for Rewrite.ml - needs to be hardcoded since we do not have a working type inference system*)
                                            mtype_of_ct (TTuple [
                                                mtype_of_var e_ret;
                                                mtype_of_st STEnd 
                                            ])
                                        )
                                    ]
                                ), auto_fplace EmptyMainType),
                                auto_fplace (VarExpr (Atom.builtin "_0_"), auto_fplace EmptyMainType)
                            ), auto_fplace EmptyMainType)
                        ));
                    ]
                } in 
                let entry = {
                    method_name = m.value.name;
                    e_call;
                    e_call_def = auto_fplace (Typedef (auto_fplace (EventDef (e_call, List.map (function {value} -> fst value) m.value.args, ()))));
                    e_ret;
                    e_ret_def = auto_fplace (Typedef (auto_fplace (EventDef (e_ret, [m.value.ret_type], ()))));


                    l_selector;

                    expecting_st = _expecting_st; 
                    port = port;
                    callback = auto_fplace (Method callback);

                    local_function
                } in
                Hashtbl.add rpc_entries m.value.name entry

            in
            List.iter process_method rpc_methods;


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
                cname_scope {
                    top_level_terms // because those who can use "rpc method" must have cname in the scope 
                    compoent cname {
                        .... rewritten 
                    }
                }

                FIXME TODO ID could be duplicated ........
                generate bridge top level ??? -> nop because of implicit 
            *)

            top_level_terms := (* Dependency order *) 
                map_values (function entry -> entry.e_call_def) @
                map_values (function entry -> entry.e_ret_def) @ 
                [
                    auto_fplace (Typedef (auto_fplace(ProtocolDef(
                        a_rpc_protocol,
                        mtype_of_st full_expecting_st.value
                    ))));
                    auto_fplace (Stmt let_rpc_bridge);
                ]
                (* N.B: Local function is not defined as a fct but inlined where need since we can not have receive inside a toplevel fct (Rewrite - at least for Akka: receive => port + async) *)
            ;


            (* STInlined generated during the derivation should be erased and replace by the full definition
            since the all subsequent passes expect that there is no more STInline in the IR. 
            *)
            let selector_inline_protocol = function
            | SType {value=STInline x} -> x = a_rpc_protocol
            | _ -> false
            in
            let rewriter_inline_protocol = function
            | SType {value=STInline x} when x = a_rpc_protocol ->
                SType full_expecting_st
            in
            top_level_terms := rewrite_type_program selector_inline_protocol rewriter_inline_protocol !top_level_terms;
            let cstruct = { cstruct with
                body = List.map (rewrite_type_component_item selector_inline_protocol rewriter_inline_protocol) cstruct.body 
            } in


            [cstruct]
        in

        let scope_selector = function 
            | {value=Component {value=ComponentStructure {name}}} -> cname = name
            | _ -> false
        in

        let scope_rewriter (terms : term list) =
            IRUtils.insert_in_terms (!top_level_terms) terms
        in

        (* TODO FIXME Bridge should be used outside maybe an issue in multi JVM*)
        let program : program = rewrite_component_program cstruct_selector cstruct_rewriter program in
        let program : program = rewrite_scopeterm_program scope_selector scope_rewriter program in
        
        (* Rewrite the callers - add them an output port and bind it to [a_rpc_bridge] *)
        let caller_selector (cstruct:component_structure) = 
            (Hashtbl.find_opt rpc_outports_translator cstruct.name) <> None
        in
        let caller_rewriter place (cstruct:component_structure) = 
            let caller_name = cstruct.name in


            (*
                
                RPC bridge is a static bridge
                because using implicit is not enough, it can not breach binary fronter. 
                A unique bridge id is given at compile time to a static RPC bridge  and partial-evaluation inline it everywhere.
            *)
            let rpc_outport = auto_fplace (Outport (auto_fplace (
                { 
                    name = Hashtbl.find rpc_outports_translator caller_name;
                    (*input = auto_fplace (VarExpr a_rpc_bridge, mt_rpc_bridge);*)
                },    
                mtype_of_ct (TOutport (*mt_rpc_protocol*))
            ))) in
            [
                {cstruct with 
                    body = rpc_outport :: cstruct.body;
                }
            ]
        in
        let program = rewrite_component_program caller_selector caller_rewriter program in

        (* Rewrite the remaining part of the code *)
        let select_call_site = function
            | CallExpr ({value=ActivationAccessExpr (_cname, _, _),_}, args) -> cname =_cname
            | _ -> false
        in

        let rewrite_call_site (Some caller_name) mt_e = function
            | CallExpr ({value=ActivationAccessExpr (_cname, activation, mname),_}, args) when cname =_cname -> 
                let entry = Hashtbl.find rpc_entries mname in

                let inline_fdcl fdcl (args: expr list) : stmt list * (_expr * main_type)= 
                    let replace_stmt stmts (fdcl_param1, arg1) : stmt list = List.map (replace_expr_stmt (snd fdcl_param1.value) (None, Some (fst arg1.value))) stmts in
                    let stmts = List.fold_left replace_stmt fdcl.value.body (List.combine fdcl.value.args args) in

                    (* Each return is rewritten to an assign of [inline_ret] *)
                    let a_ret = Atom.fresh "inline_ret" in
                    let stmts = (auto_fplace (LetStmt(
                        fdcl.value.ret_type, 
                        a_ret, 
                        (auto_fplace ((LitExpr (auto_fplace VoidLit)), fdcl.value.ret_type)) (* Works in Java since null can be of any type - FIXME maybe we will need to use an option type initialized to None *)
                    )))::stmts in
                    let stmts = List.flatten (List.map (rewrite_stmt_stmt false 
                        (function |ReturnStmt _ -> true | _ -> false) 
                        ( fun place -> function | ReturnStmt e -> [AssignExpr (a_ret, e)] )) stmts)
                    in
                    
                    stmts, (VarExpr a_ret, fdcl.value.ret_type)
                in
                inline_fdcl (entry.local_function caller_name) ([
                    activation;
                ] @ args)


                (* 
                At this point two cases:
                    a) call(args); => ....; ret; Error - since ret as (except if void) a non void type
                    b) e[call(args)] => ....; e[ret/call(args)] Ok

                    b) should be left unchange and a) should be rewritten to call(args); => ....; (remove the Expression VarExpr ret)

                    Two options do it manually here or rely on the clean pass to remove ExpressionStmt VarExpr since no side effect + no value.
                    At this point we implement it inside the clean pass - NB. this implies that we keep garbage inside IR until running cleansing
                    TODO FIXME why not running cleansing after this pass (perf ?)
                *)
        in

        (*let program = rewrite_expr_program select_call_site rewrite_call_site program in*)
        let program = rewrite_exprstmts_program (function _ -> false) select_call_site rewrite_call_site program in

        program

    (**********************************************************)
    let name = "Derivations.RPC"
    let displayed_pass_shortdescription = Printf.sprintf "RPC derivation have been applied IR for %s" (Atom.to_string cname)
    let displayed_ast_name = "IR RPC-derived"
    let show_ast = true 
    let global_at_most_once_apply = false


    let precondition program = program
    let postcondition program = program
    let apply_program = derive_program
end
