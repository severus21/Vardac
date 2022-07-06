(**
*)

open Core
open AstUtils
open IR
 
open Easy_logging


let register_cl2c_entry clitems2citems (cl_name, c_name) (item_name, oitem_name) = 
    match Hashtbl.find_opt clitems2citems cl_name with
    | None ->
        let entries = Hashtbl.create 16 in
        Hashtbl.add clitems2citems cl_name entries;
        Hashtbl.add entries item_name (c_name, oitem_name)
    | Some entries ->
        Hashtbl.add entries item_name (c_name, oitem_name)

(* return list of c_name *)
let cl2c_get_origins clitems2citems cl_name = 
    try
        let entries = Hashtbl.find clitems2citems cl_name in
        Atom.Set.of_seq (Seq.map fst (Hashtbl.to_seq_values entries))
    with Not_found ->
        failwith (Printf.sprintf "cl_name [%s] not in clitems2citems {%s}" (Atom.to_string cl_name) (Atom.show_list ";" (List.of_seq (Hashtbl.to_seq_keys clitems2citems))))

module Make () = struct
    let logger = Core.Utils.make_log_of "InlineElim"

    let fplace = (Error.forge_place "InlineElim" 0 0) 
    let auto_fplace smth = {place = fplace; value=smth}
    include AstUtils2.Mtype.Make(struct let fplace = fplace end)

    (* External state 
        clitems2citems[cl.name]:
            item_name -> (origin_component_name, origin_item_name)
    *)
    let clitems2citems = Hashtbl.create 16


    (* B inlineable in A => "B->Set{A::..}"*)
    let is_inlineable_in = Hashtbl.create 256
    (* A -> Set(B) *)
    let inv_is_inlineable_in = Hashtbl.create 256 

    (* B -> bstruct *)
    let inlineable_cstructs = Hashtbl.create 256

    let tdefs = Hashtbl.create 256 

    let parent2spawn_static_bridges = Hashtbl.create 256

    let rec extract_ainline = function
        | [] -> []
        | InlinableIn schemas::xs -> schemas@(extract_ainline xs)

    let select_component_with_inlinable = function
        | Component {value=ComponentStructure cstruct} -> 
            extract_ainline cstruct.annotations <> []
        | _ -> false

    let select_component_inline_in = function
        | Component {value=ComponentStructure cstruct} -> begin
            match Hashtbl.find_opt inv_is_inlineable_in cstruct.name with
            | Some xs -> Atom.Set.empty <> xs
            | None -> false
        end
        | _ -> false


    let select_spawn_with_inline_in = function
        | Spawn {inline_in = Some _} -> true
        | _ -> false

    let renaming = Hashtbl.create 256
    let refresh_atom freevars freetvars x = 
        logger#debug "try to refresh x=[%s]" (Atom.to_string x);
        if Atom.is_builtin x then x (* TODO guarantee *) 
        else
            match Hashtbl.find_opt renaming x with
            | Some y -> 
                logger#debug "refresh x=[%s] -> y=[%s]" (Atom.to_string x) (Atom.to_string y);
                y
            | None -> 

                (* x should not be a variable binded outside the included citems *)
                if Atom.Set.find_opt x freevars = None && Atom.Set.find_opt x freetvars = None then 
                begin
                    let y = Atom.fresh (Atom.value x) in
                    logger#debug "refresh x=[%s] -> y=[%s]" (Atom.to_string x) (Atom.to_string y);
                    Hashtbl.add renaming x y;
                    y
                end
                else x

    let cl_name = 
        let state = Hashtbl.create 16 in
        fun schema schema_in ->
            match Hashtbl.find_opt state (schema, schema_in) with
            | None -> 
                let x = Atom.fresh (Printf.sprintf "Inline_%s_in_%s_" (Atom.to_string schema) (Atom.to_string schema_in)) in
                Hashtbl.add state (schema, schema_in) x;
                x
            | Some x -> x

    let name_spawn_inline = 
        let state = Hashtbl.create 16 in
        fun schema schema_in ->
            match Hashtbl.find_opt state (schema, schema_in) with
            | None -> 
                let x = Atom.fresh (Printf.sprintf "spawn_%s_in_%s_" (Atom.to_string schema) (Atom.to_string schema_in)) in
                Hashtbl.add state (schema, schema_in) x;
                x
            | Some x -> x
        
        

    let port_name = 
        let state = Hashtbl.create 16 in
        fun schema schema_in pname->
            match Hashtbl.find_opt state (schema, schema_in, pname) with
            | None -> 
                let x = Atom.fresh (Printf.sprintf "p_%s_in_%s__%s_" (Atom.to_string schema) (Atom.to_string schema_in) (Atom.to_string pname)) in
                Hashtbl.add state (schema, schema_in, pname) x;
                x
            | Some x -> x

    let spawn_request = 
        let state = Hashtbl.create 16 in
        fun schema ->
            match Hashtbl.find_opt state (schema) with
            | None -> 
                let x = Atom.fresh (Printf.sprintf "spawn_request_%s_" (Atom.to_string schema)) in
                Hashtbl.add state (schema) x;
                x
            | Some x -> x

    let spawn_response = 
        let state = Hashtbl.create 16 in
        fun schema ->
            match Hashtbl.find_opt state (schema) with
            | None -> 
                let x = Atom.fresh (Printf.sprintf "spawn_response_%s_" (Atom.to_string schema)) in
                Hashtbl.add state (schema) x;
                x
            | Some x -> x

    let spawn_protocol = 
        let state = Hashtbl.create 16 in
        fun schema ->
            match Hashtbl.find_opt state (schema) with
            | None -> 
                let x = Atom.fresh (Printf.sprintf "spawn_protocol_%s_" (Atom.to_string schema)) in
                Hashtbl.add state (schema) x;
                x
            | Some x -> x

    let spawn_protocol_st schema = 
        (* !spawn_request?spawn_response. *)
        auto_fplace (STSend(
            mtype_of_var (spawn_request schema), 
            auto_fplace (STRecv (
                mtype_of_var (spawn_response schema),
                auto_fplace STEnd
            ))))

    let spawn_static_bridge = 
        let state = Hashtbl.create 16 in
        fun schema schema_in ->
            match Hashtbl.find_opt state (schema, schema_in) with
            | None -> 
                let x = Atom.fresh (Printf.sprintf "spawn_static_bridge_%s_in_%s_" (Atom.to_string schema) (Atom.to_string schema_in)) in
                Hashtbl.add state (schema, schema_in) x;
                x
            | Some x -> x

    let spawn_static_bridge_mt schema schema_in = 
        mtype_of_ct (TBridge{ 
            in_type = mtype_of_cmt CompTBottom;
            out_type = mtype_of_cvar schema_in; 
            protocol = mtype_of_st (spawn_protocol_st schema).value;
        })

    let extract_cl_callback (cl:class_structure) port_callback= 
        let cl_callback_name = Hashtbl.find renaming (match fst port_callback.value with
            | AccessExpr ({value=This,_}, {value=VarExpr name, _}) -> name 
            | _ -> failwith "Other callback for inlined inport are not supported yet"
        ) in
        let [cl_callback_in] : method0 list = List.filter_map (map0_place(transparent0_plgannot(function place -> function | CLMethod m when m.value.name = cl_callback_name -> Some m | _-> None))) cl.body in

        cl_callback_in

    let eliminate_static_inlinable program = 
        let rewriter_inlinable parent_opt place = function
            | Component {place; value=ComponentStructure cstruct} as c -> 
                logger#info "static inline %s in " (Atom.to_string cstruct.name);
                let schemas = extract_ainline cstruct.annotations in

                (* register in is_inlineable_in *)
                begin
                    let schemas_in = Atom.Set.of_seq (List.to_seq schemas) in
                    match Hashtbl.find_opt is_inlineable_in cstruct.name with
                        | None -> Hashtbl.add is_inlineable_in cstruct.name schemas_in
                        | Some xs -> Hashtbl.add is_inlineable_in cstruct.name (Atom.Set.union schemas_in xs)
                end;

                (* register in inv_is_inlineable_in *)
                begin
                    List.iter (function schema_in ->
                        match Hashtbl.find_opt inv_is_inlineable_in schema_in with
                            | None -> Hashtbl.add inv_is_inlineable_in schema_in (Atom.Set.singleton cstruct.name)
                            | Some xs -> Hashtbl.add is_inlineable_in schema_in (Atom.Set.add cstruct.name xs)
                    ) schemas
                end;

                Hashtbl.add inlineable_cstructs cstruct.name cstruct;
                [ Component {place; value=ComponentStructure {
                    cstruct with
                        (* remove inline_in annotations *)
                        annotations = List.filter (function | InlinableIn _-> false | _ -> true) cstruct.annotations
                    }} ]
        in

        let rewriter_inline_in parent_opt place = function
            | Component {place; value=ComponentStructure cstruct_in} -> 
                let schema_in = cstruct_in.name in
                let schemas = List.of_seq (Atom.Set.to_seq (Hashtbl.find inv_is_inlineable_in schema_in)) in
                
                let spawn_static_bridges = ref [] in

                let n_body = List.map (function schema -> 
                    let cstruct = Hashtbl.find inlineable_cstructs schema in 

                    (*** derive a "class" [InlineB15A] ***)
                    let cl_item_names = List.fold_left (fun env -> map0_place(transparent0_plgannot(fun place -> function 
                        | Method item -> Atom.Set.add item.value.name env
                        | State item -> Atom.Set.add item.value.name env
                        | _-> env
                    ))) Atom.Set.empty cstruct.body in
                    let body = List.flatten (List.map (map_places(transparent_plgannots(function place -> function 
                            | Method item -> 
                                [CLMethod item] 
                            | State item -> 
                                [CLState item]
                            | Term _ -> Error.perror place "component with inner term can not be inlined yet!"
                            | Contract _ -> raise (Error.PlacedDeadbranchError (place, "contract should be paired with method before eliminating inlinin"))
                            | _ -> [] 
                        ))) 

                        (* refresh atom to preserve binding unicity *)
                        (
                            let _, freevars = List.split (List.map (free_vars_component_item Atom.Set.empty) cstruct.body) in
                            let freevars = Atom.Set.of_list (List.map snd (List.flatten freevars)) in

                            let _, freetvars = List.split (List.map (free_tvars_component_item ~flag_tcvar:true Atom.Set.empty) cstruct.body) in
                            let freetvars = Atom.Set.of_list (List.flatten freetvars) in

                            (List.map (rename_component_item ~flag_rename_attribute:true (function x ->
                                let y = refresh_atom freevars freetvars x in

                                logger#debug "cl_item_names {%s}" (Atom.show_list "; " (Atom.Set.to_list cl_item_names));
                                if Atom.Set.mem x cl_item_names then
                                    register_cl2c_entry clitems2citems (cl_name schema schema_in, schema) (x, y);
                                y
                            ))
                            cstruct.body))
                        )
                    in
                    let body_elmts_set = Atom.Set.of_seq (List.to_seq(List.map (map0_place(transparent0_plgannot(
                        function place -> function 
                        | CLMethod m -> 
                            m.value.name
                        | CLState s -> s.value.name
                    ))) body)) in


                    (* 
                        * This -> Self 
                        * TUID B15 -> InlineB15 *)
                    let cl_body = 
                        List.map 
                            (rewrite_type_class_item 
                                (function 
                                    | CompType{value = CompTUid name} -> name = schema
                                    | CompType{value = TStruct (name,_)} -> name = schema
                                    | _ -> false)
                                (function 
                                    | CompType{value = CompTUid name; place} when name = schema -> 
                                        logger#debug "updating CompType in CL (UID)";
                                        ClType {place = place@fplace; value = CompTUid (cl_name schema schema_in)} 
                                    | CompType{value = TStruct (name,tstruct)} when name = schema ->
                                        logger#debug "updating CompType in CL (STRUCT)";
                                        (* Clean subterms/ports from tstuct*)
                                        let n_tstruct = 
                                            Atom.VMap.filter 
                                                (fun key _ -> 
                                                    Atom.Set.find_opt key body_elmts_set <> None
                                                ) 
                                                tstruct in 
                                        ClType {place = place@fplace; value = TStruct (cl_name schema schema_in, n_tstruct)}
                                )
                            ) 
                            (List.map (rewrite_expr_class_item (function |This -> true | _ -> false) (function mt -> function | This -> Self)) body)
                    in

                    let cl_get_activation_ref = Atom.fresh "get_activation_ref" in
                    let a_cl_activation_ref = Atom.fresh "mocked_inlined_activation_ref" in
                    let a_parent_activation_ref = Atom.fresh "parent_activation_ref" in
                    let cl = {
                        annotations = cstruct.annotations;
                        name = cl_name schema schema_in;
                        body = 
                        [
                            (* Add schema to cl since component has this field *)
                            auto_fplace(auto_plgannot(CLState (auto_fplace{
                                ghost = false;
                                type0 = mtype_of_ft TStr;
                                name = Atom.builtin "schema";
                                body = Some(e2_lit (StringLit (Atom.to_string schema)));
                            })));
                            (* Add an get_activation_ref method: parent_activation_ref -> mocked activation_ref *)
                            auto_fplace(auto_plgannot(CLState (auto_fplace {
                                ghost = false;
                                type0 = mtype_of_ct (TActivationRef (mtype_of_cvar schema));
                                name = a_cl_activation_ref;
                                body = Some(e2_e(CallExpr(
                                    e2var (Atom.builtin "forge_activation_ref"),
                                    []
                                )))
                            })));
                            auto_fplace (auto_plgannot(CLMethod (auto_fplace {
                                annotations = [];
                                ghost = false;
                                ret_type = mtype_of_ct (TActivationRef (mtype_of_cvar schema));
                                name = cl_get_activation_ref;
                                args = [ auto_fplace (mtype_of_ct (TActivationRef (mtype_of_cvar schema_in)), a_parent_activation_ref)] ;
                                body = [
                                    auto_fplace(ReturnStmt(e2_e(CallExpr(
                                        e2var (Atom.builtin "one_hop_activation_ref"),
                                        [
                                            e2_e (CallExpr(e2var (Atom.builtin "current_activation"), []));
                                            e2_e (OptionExpr (Some (
                                                e2_e (AccessExpr(
                                                    e2_e Self,
                                                    e2var a_cl_activation_ref
                                                ))
                                            )))
                                        ]
                                    ))))
                                ];
                                contract_opt = None;
                                on_destroy = false;
                                on_startup = false;
                            })))
                        ] @ cl_body
                    } in
                    
                    (* Update cl constructor to refresh activation_ref identity for each object *)
                    let cl_constructor = match get_clconstructor cl with
                        | None -> failwith "TODO add a default constructor for inlined cl"
                        | Some constructor -> 
                            { constructor with
                                value = {constructor.value with
                                body = (
                                    auto_fplace(AssignSelfExpr(
                                        a_cl_activation_ref,
                                        e2_e(CallExpr(
                                            e2var (Atom.builtin "forge_activation_ref"),
                                            []
                                        ))
                                    ))
                                )::constructor.value.body 
                                }}
                    in
                    let cl = replace_clconstructor cl cl_constructor in

                    (*** add state [instances_B15] in A ***)
                    let name_inlined_instances = Atom.fresh (Printf.sprintf "instances_%s_" (Atom.to_string schema)) in
                    let inlined_instances = {
                        ghost = false;
                        type0 = mtype_of_ct (TDict (
                            mtype_of_ft TActivationID,
                            mtype_of_ct (TObject (cl_name schema schema_in))
                        ));
                        name = name_inlined_instances;
                        body = Some (e2_e (Block2Expr(Dict,[])))
                    } in

                    (*** add method [spawn_B15] in A 
                        spawn_B15 (args of spawn B15) -> ActivationRef<A> 
                        return this;
                    ***)
                    let spawn_inline_args = 
                        match  get_onstartup cstruct with
                        | Some schema_onstartup -> List.map (map_place(fun place (mt, x) -> (mt, refresh_atom Atom.Set.empty Atom.Set.empty x))) schema_onstartup.value.args
                        | None -> []
                    in

                    logger#debug "spawn_inline_args %d" (List.length spawn_inline_args);

                    let a_instance = Atom.fresh "instance" in
                    let spawn_inline = {
                        annotations = [];
                        ghost = false;
                        ret_type = mtype_of_ct (TActivationRef (mtype_of_cvar schema));
                        name = name_spawn_inline schema schema_in;
                        args = spawn_inline_args;
                        body = [
                            (*
                                obj<InlinedB15> instance = new InlinedB15A(...);
                                this.instances_B15[instance.activation_ref(this)] = instance;
                                return instance.activation_ref(this);  
                            *)
                            auto_fplace (LetStmt(
                                mtype_of_ct (TObject (cl_name schema schema_in)),
                                a_instance,
                                e2_e (Create {
                                    c = cl.name;
                                    args = List.map (function {value=(mt, x)} -> e2var ~mt:mt x) spawn_inline_args;
                                })
                            ));
                            auto_fplace (ExpressionStmt(e2_e(CallExpr (
                                e2var (Atom.builtin "add2dict"),
                                [
                                    e2var name_inlined_instances;
                                    e2_e (CallExpr(
                                        e2var (Atom.builtin "activationid"),
                                        [
                                            e2_e (CallExpr(
                                                e2_e (AccessExpr(
                                                    e2var a_instance,
                                                    e2var cl_get_activation_ref
                                                )),
                                                [ 
                                                    e2_e (CallExpr(e2var (Atom.builtin "current_activation"), []));
                                                ]
                                            ))
                                        ]
                                    ));
                                    e2var a_instance;
                                ]
                            ))));
                            auto_fplace (ReturnStmt (e2_e (InterceptedActivationRef (
                                e2_e (CallExpr(e2var (Atom.builtin "current_activation"), [])), 
                                (Some (e2_e (CallExpr(
                                    e2_e (AccessExpr(
                                        e2var a_instance,
                                        e2var cl_get_activation_ref
                                    )),
                                    [ e2_e (CallExpr(e2var (Atom.builtin "current_activation"), [])) ]
                                ))))
                            ))));
                        ];
                        contract_opt = None;
                        on_destroy = false;
                        on_startup = false;
                    }
                    in


                    (*** add inports [p_B15_pb15name] in A + routing to the corresponding instance in [instances_B15] ***)
                    let n_inports = 
                        let inports = List.filter (function 
                            | {value={v=Inport _}} -> true
                            | _ -> false
                        ) cstruct.body in
                        List.map (map_place(transparent_plgannot(function place ->function
                            | Inport {value=port,mt;place} ->
                                assert(port._children = []);

                                let cl_callback_in = extract_cl_callback cl port.callback in

                                let a_objid = Atom.fresh "objid" in
                                let n_args = (List.map (map_place(fun place (mt, x) -> (mt, refresh_atom Atom.Set.empty Atom.Set.empty x))) cl_callback_in.value.args) in
                                let a_session = snd (List.nth n_args 1).value in
                                let a_obj = Atom.fresh "obj" in
                                let n_callback = {
                                    annotations = [];
                                    ghost = false;
                                    ret_type = mtype_of_ft TVoid;
                                    name = Atom.fresh (Printf.sprintf "callback_%s_in_%s" (Atom.to_string schema) (Atom.to_string schema_in));
                                    args = n_args;
                                    body = [
                                        (*
                                            1) obj = instances_B15A[obj_id]
                                            2) return obj.callback(msg, ret); 
                                        *)
                                        auto_fplace (LetStmt(
                                            mtype_of_ft TActivationID,
                                            a_objid,
                                            e2_e(CallExpr(
                                                e2var (Atom.builtin "activationid"),
                                                [
                                                    e2_e(CallExpr(
                                                        e2var (Atom.builtin "option_get"),
                                                        [
                                                            e2_e(CallExpr(
                                                                e2var (Atom.builtin "session_to_2_"),
                                                                [
                                                                    e2var a_session 
                                                                ]
                                                            ))
                                                        ]
                                                    ))
                                                ]
                                            ))
                                        ));
                                        auto_fplace (LetStmt(
                                            mtype_of_ct (TObject (cl_name schema schema_in)),
                                            a_obj,
                                            e2_e(CallExpr(
                                                e2_e(AccessExpr(
                                                    e2_e(AccessExpr(
                                                        e2_e This,
                                                        e2var name_inlined_instances
                                                    )),
                                                    e2var (Atom.builtin "get2dict")
                                                )),
                                                [
                                                    e2var a_objid
                                                ]
                                            ))
                                        ));
                                        auto_fplace( ReturnStmt(
                                            e2_e (CallExpr(
                                                e2_e (AccessExpr (
                                                    e2var a_obj,
                                                    e2var cl_callback_in.value.name
                                                )),
                                                List.map (function {place; value=(mt,x)} -> {place = place@fplace; value=(VarExpr x,mt)}) n_args
                                            ))
                                        ));
                                    ];
                                    contract_opt = None;
                                    on_destroy = false;
                                    on_startup = false;

                                } in
                                let n_port = Inport (auto_fplace ({
                                    name = port_name schema schema_in port.name;
                                    expecting_st = port.expecting_st;
                                    callback = e2_e (AccessExpr(e2_e This, e2var n_callback.name));
                                    _children = [];
                                    _disable_session = port._disable_session;
                                    _is_intermediate = port._is_intermediate;

                                }, auto_fplace EmptyMainType)) in
                                n_port
                        ))) inports 
                    in
                    
                    (*** add outports [p_B15_pb15name] in A ***)
                    let cl_renaming = Hashtbl.create 16 in
                    let n_outports = 
                        let outports = List.filter (function 
                            | {value={v=Outport _}} -> true
                            | _ -> false
                        ) cstruct.body in
                        List.map (map_place(transparent_plgannot(function place ->function
                            | Outport {value=port,mt;place} ->
                                assert(port._children = []);
                                let n_port = Outport (auto_fplace({
                                    name = port_name schema schema_in port.name;
                                    protocol = port.protocol;
                                    _children = [];
                                }, auto_fplace EmptyMainType)) in

                                Hashtbl.add cl_renaming 
                                    (Hashtbl.find renaming port.name) 
                                    (port_name schema schema_in port.name); 

                                n_port
                        ))) outports 
                    in
                    (*** apply cl_renaming i.e. rename output ports ***)
                    let cl = {cl with 
                        body = List.map (rename_class_item (function x -> 
                            match Hashtbl.find_opt cl_renaming x with
                            | Some y -> y
                            | None -> x
                        )) cl.body
                    } in

                    (*** add eports [p_B15_pb15name] in A + routing to ******all****** instances in [instances_B15]***)
                    let n_eports : component_item list = 
                        let eports = List.filter (function 
                            | {value={v=Eport _}} -> true
                            | _ -> false
                        ) cstruct.body in

                        List.map (map_place(transparent_plgannot(function place ->function
                            | Eport {value=port,mt;place} ->
                                let cl_callback_in = extract_cl_callback cl port.callback in

                                let n_args = List.map (map_place(fun place (mt, x) -> (mt, refresh_atom Atom.Set.empty Atom.Set.empty x))) cl_callback_in.value.args in
                                let a_obj = Atom.fresh "obj" in
                                let n_callback = {
                                    annotations = [];
                                    ghost = false;
                                    ret_type = mtype_of_ft TVoid;
                                    name = Atom.fresh (Printf.sprintf "callback_%s_in_%s" (Atom.to_string schema) (Atom.to_string schema_in));
                                    args = n_args;
                                    body = [
                                        (*
                                            1) for obj in instances_B15A[obj_id] {
                                                obj.callback(args)? == (); 
                                            }
                                            return ok(());
                                        *)
                                        auto_fplace (ForeachStmt(
                                            mtype_of_ct (TObject (cl_name schema schema_in)),
                                            a_obj,
                                            e2_e (AccessExpr(e2_e This, e2var name_inlined_instances)),
                                            auto_fplace(ExpressionStmt(
                                                e2_e (BinopExpr(
                                                    e2_e (UnopExpr(
                                                        UnpackOrPropagateResult,
                                                        e2_e (CallExpr(
                                                            e2_e (AccessExpr (
                                                                e2var a_obj,
                                                                e2var cl_callback_in.value.name
                                                            )),
                                                            List.map (function {place; value=(mt,x)} -> {place = place@fplace; value=(VarExpr x,mt)}) n_args
                                                        ))
                                                    )),
                                                    Equal,
                                                    e2_lit VoidLit
                                                ))
                                            ))
                                        ));
                                        auto_fplace(ReturnStmt(e2_lit VoidLit));
                                    ];
                                    contract_opt = None;
                                    on_destroy = false;
                                    on_startup = false;

                                } in
                                let n_port : _component_item = Eport (auto_fplace ({
                                    name = port_name schema schema_in port.name;
                                    expecting_mt = port.expecting_mt;
                                    callback = e2_e (AccessExpr(e2_e This, e2var n_callback.name));
                                }, auto_fplace EmptyMainType)) in
                                n_port
                        ))) eports 
                    in

                    (*** Spwan request/response + inport + callback ***)
                    let spawn_request_tdef = Typedef (auto_fplace (EventDef( 
                        spawn_request schema,
                        List.map (function {value=(mt, x)} -> mt) spawn_inline_args
                        ,
                        ()
                    ))) in
                    let spawn_response_tdef = Typedef (auto_fplace (EventDef( 
                        spawn_response schema,
                        [
                            mtype_of_ct (TActivationRef (mtype_of_cvar schema));
                        ],
                        ()
                    ))) in
                    let spawn_protocol_tdef = Typedef (auto_fplace (ProtocolDef(
                        spawn_protocol schema,
                        mtype_of_st (spawn_protocol_st schema).value
                    ))) in

                    let let_static_bridge = 
                        let x = spawn_static_bridge schema schema_in in
                        let mt_bridge = spawn_static_bridge_mt schema schema_in in
                        Stmt (auto_fplace (LetStmt(
                            mt_bridge,
                            x,
                            e2_lit ~mt:mt_bridge (StaticBridge{
                                id = Atom.fresh (Atom.value x);
                                protocol_name = spawn_protocol schema;
                            })
                        )))
                    in


                    (* register for inclusion *)
                    Hashtbl.add tdefs (spawn_response schema) [spawn_response_tdef; spawn_request_tdef; spawn_protocol_tdef; let_static_bridge];

                    
                    let a_ref = Atom.fresh "ref" in
                    let a_request = Atom.fresh "request" in
                    let a_session = Atom.fresh "s" in
                    let spawn_callback = {
                        annotations = [];
                        ghost = false;
                        ret_type = mtype_of_ft TVoid;
                        name = Atom.fresh (Printf.sprintf "spawn_callback_%s" (Atom.to_string schema));
                        args = [
                            auto_fplace (mtype_of_var (spawn_request schema),a_request);
                            auto_fplace (mtype_of_st (STSend (
                                mtype_of_var (spawn_response schema),
                                auto_fplace STEnd
                            )), a_session);
                        ];
                        body = [
                            (* Activation_ref<B> ref = this.spawn_B15(...); *)
                            auto_fplace(LetStmt(
                                mtype_of_ct (TActivationRef (mtype_of_cvar schema)),
                                a_ref,
                                e2_e(CallExpr(
                                    e2_e(AccessExpr(
                                        e2_e This,
                                        e2var spawn_inline.name
                                    )),
                                    (List.mapi (fun i {value=(mt, x)} -> 
                                        e2_e (AccessExpr(
                                            e2var a_request,
                                            e2var (Atom.builtin (Printf.sprintf "_%d_" i))
                                        ))
                                    ) spawn_inline_args)
                                ))
                            ));
                            (* fire(s, response(ref));*)
                            auto_fplace (ExpressionStmt(
                                e2_e (CallExpr(
                                    e2var (Atom.builtin "fire"),
                                    [
                                        e2var a_session;
                                        e2_e (NewExpr(
                                            e2var (spawn_response schema),
                                            [
                                                e2var a_ref
                                            ]
                                        ))
                                    ]
                                ))
                            ));
                        ];
                        contract_opt = None;
                        on_destroy = false;
                        on_startup = false;
                    } 
                    in
                    
                    let spawn_port_name = Atom.fresh (Printf.sprintf "spawn_port_%s_" (Atom.to_string schema)) in
                    let spawn_port = (
                        {
                            name = spawn_port_name;
                            expecting_st = mtype_of_st (IRMisc.dual (spawn_protocol_st schema)).value;
                            callback = e2_e(AccessExpr(e2_e This, e2var spawn_callback.name));
                            _children = [];
                            _disable_session = false;
                            _is_intermediate = false;
                        },
                        mtype_of_ct (TInport (mtype_of_st (IRMisc.dual (spawn_protocol_st schema)).value))
                    ) in

                    (*** Register a spawn_static_bridge_B argument to A constructor + bind it with spawn port in constructor ***)
                    spawn_static_bridges := (   
                        spawn_port_name,
                        spawn_static_bridge schema schema_in,
                        spawn_static_bridge_mt schema schema_in
                    ) :: !spawn_static_bridges;


                    (*** returns ***)
                    (List.map (function x -> auto_fplace (auto_plgannot x))
                    ([
                        Term (auto_fplace (auto_plgannot (Class cl))); 
                        State (auto_fplace inlined_instances); 
                        Method (auto_fplace spawn_inline);
                        Method (auto_fplace spawn_callback);
                        Inport (auto_fplace spawn_port);
                    ]))
                    @n_inports@n_outports@n_eports
                ) schemas in

                (*** Add a spawn_static_bridge_B argument to A constructor + bind it with spawn port in constructor ***)
                let cstruct_in = List.fold_left (fun cstruct_in (spawn_port_name, spawn_static_bridge_name, spawn_static_bridge_mt) -> 
                    match get_onstartup cstruct_in with
                    | Some onstartup -> 
                        (* Refresh atom for binder *)
                        let a_bridge = Atom.fresh (Atom.hint spawn_static_bridge_name) in
                        replace_onstartup cstruct_in { onstartup with
                            value = {onstartup.value with 
                                args = auto_fplace (spawn_static_bridge_mt, a_bridge)::onstartup.value.args;
                                body = (
                                    auto_fplace(ExpressionStmt(
                                        e2_e(
                                            CallExpr(
                                                e2var (Atom.builtin "bind"),
                                                [ 
                                                    e2var spawn_port_name;
                                                    e2var a_bridge
                                                ]
                                            )
                                        )
                                    ))
                                )::onstartup.value.body;
                            }
                        }
                    | None -> failwith "TODO no onstartup not supported yet, for inline elim" 
                    
                ) cstruct_in !spawn_static_bridges in 

                (* Used to rewrite new A15() -> new A15(bridges) *)
                Hashtbl.add parent2spawn_static_bridges schema_in !spawn_static_bridges;

                [ Component {
                    place = place @ fplace; 
                    value=ComponentStructure { cstruct_in with
                        body = cstruct_in.body @ (List.flatten n_body);
                    }} ]
        in



        program
        |> rewrite_term_program select_component_with_inlinable rewriter_inlinable
        |> rewrite_term_program select_component_inline_in rewriter_inline_in
        (* add sspawn_request/respons_tdef/protocol_def lca in program *)
        (* + Insert static bridge definition (lca) in program *)
        |> insert_terms_into_lca [None] (List.map (fun tdef -> auto_fplace (auto_plgannot tdef)) (List.flatten (List.of_seq (Hashtbl.to_seq_values tdefs))))

    let eliminate_dynamic_inline_in program = 
        (*** Hydrate before doing parent rewriting ***)
        let host_inline_in = Hashtbl.create 16 in
        collect_expr_program Atom.Set.empty select_spawn_with_inline_in (fun parent_opt _ -> function 
            | {value=Spawn {c; args; inline_in = Some ({place; value=e, {value=CType{value=TActivationRef{value=CompType {value=CompTUid schema_in}}}}} as inline_in)},_} -> begin 
                let schema = match fst c.value with
                    | VarCExpr schema -> schema
                    | _ -> raise (Error.DeadbranchError "Unsupported [c] in spawn !") 
                in

                let parent = match parent_opt with
                    | Some parent -> parent
                    | _ -> Error.error "spawn inline in outside of parent scope"
                in
                logger#debug "hydrating host_inline_in of %s" (Atom.to_string parent);
                
                begin
                    match Hashtbl.find_opt host_inline_in parent with
                    | Some xs   -> Hashtbl.add host_inline_in parent (Atom.Set2.add (schema, schema_in) xs)
                    | None      -> Hashtbl.add host_inline_in parent (Atom.Set2.singleton (schema, schema_in))
                end;

                []
            end
        ) program;

        (*** Rewrite parent of spawn in in order to add outport ***)
        let parent_selector (cstruct:component_structure) = 
            logger#debug "parent_selector [host_inline_in %d]" (Hashtbl.length host_inline_in);
            Hashtbl.iter (fun key set ->
                logger#debug "host_inline_in of %s" (Atom.to_string key);
                Atom.Set2.iter (function (a,b) -> logger#debug "- %s -> %s\n" (Atom.to_string a) (Atom.to_string b)) set;
            ) host_inline_in;
            match Hashtbl.find_opt host_inline_in cstruct.name with
            | Some set -> set <> Atom.Set2.empty 
            | None -> false
        in 
        (* schema_host is used to dedup on host basis *)
        let spawn_outport = 
            let state = Hashtbl.create 16 in
            fun schema schema_in schema_host ->
                match Hashtbl.find_opt state (schema, schema_in, schema_host) with
                | None -> 
                    let x = Atom.fresh (Printf.sprintf "p_outspawn_%s_in_%s_" (Atom.to_string schema) (Atom.to_string schema_in)) in
                    Hashtbl.add state (schema, schema_in, schema_host) x;
                    x
                | Some x -> x
        in
        let parent_rewriter parent_opt place (cstruct:component_structure) = 
            (* Generate outports *)
            let static_bridges__outports = 
                List.map
                    (function (schema, schema_in) -> 
                        spawn_static_bridge schema schema_in, 
                        auto_fplace ({
                            name        = spawn_outport schema schema_in cstruct.name;
                            protocol    = mtype_of_st (spawn_protocol_st schema).value;
                            _children   = [];
                        }, mtype_of_ct (TOutport (mtype_of_st (spawn_protocol_st schema).value)))    
                    )
                    (Atom.Set2.to_list (Hashtbl.find host_inline_in cstruct.name))
            in

            logger#debug "static_bridges__outports %d" (List.length static_bridges__outports);

            (* Bind outports at startup *)
            let onstartup   = 
                match get_onstartup cstruct with
                | Some onstartup ->
                    List.fold_left (fun (onstartup:method0) ((static_bridge, outport):Atom.atom * outport) -> 
                        { onstartup with 
                            value = {onstartup.value with
                                body = [
                                    auto_fplace (ExpressionStmt(e2_e (CallExpr (
                                        e2var (Atom.builtin "bind"),
                                        [
                                            e2_e (AccessExpr(
                                                e2_e This,
                                                e2var (fst outport.value).name 
                                            ));
                                            e2var static_bridge 
                                        ]
                                    ))))
                                ] @ onstartup.value.body 
                            }
                        }
                        
                    ) onstartup static_bridges__outports 
                | None -> failwith "TODO no onstartup not supported yet, for inline elim" 
            in
            let cstruct     = replace_onstartup cstruct onstartup in
            
            let outports = List.map (function (_,port) -> auto_fplace( auto_plgannot(Outport port))) static_bridges__outports in
            [{cstruct with body = cstruct.body @ outports }]
        in

        let select_host_spawn = function
            | Spawn {c = {value=VarCExpr name, _ }} ->
               Hashtbl.find_opt parent2spawn_static_bridges name <> None
            | Spawn _ -> 
                failwith "spawn with complex component_expr is not yet support by InlineElim" 
            | _ -> false
        in

        let rewrite_host_spawn _ = function
            | Spawn ({c = {value=VarCExpr name, _ }} as spawn)-> 
                logger#debug "rewrite_host_spawn of [%s]" (Atom.to_string name);
                let static_bridges = Hashtbl.find parent2spawn_static_bridges name in
                Spawn { spawn with
                    args  = List.map (function (_, b_name, b_mt) -> e2var b_name) static_bridges @ spawn.args}
        in

        (*** Eliminate inline in***)
        let rewriter parent_opt mt = function
            | Spawn {c; args; inline_in = Some ({place; value=e, {value=CType{value=TActivationRef{value=CompType {value=CompTUid schema_in}}}}} as inline_in)} -> begin 
                let schema = match fst c.value with
                    | VarCExpr schema -> schema
                    | _ -> raise (Error.DeadbranchError "Unsupported [c] in spawn !") 
                in
                let parent = match parent_opt with
                    | Some parent -> parent
                    | _ -> Error.error "spawn inline in outside of parent scope"
                in

                logger#debug "spawn inline %s in %s" (Atom.to_string schema) (Atom.to_string schema_in);


                (* That it is a valid inline according to static_elim *)
                begin
                    try 
                        let possible_schemas_in = Hashtbl.find is_inlineable_in schema in
                        ignore (Atom.Set.find schema_in possible_schemas_in)
                    with Not_found -> Error.error "Can not inline %s in %s ! Use @inline_in annotations." (Atom.to_string schema) (Atom.to_string schema_in)
                end;

                let a_session_0 = Atom.fresh "s" in
                let a_session_1 = Atom.fresh "s" in
                let a_res       = Atom.fresh "res" in
                let _tmp_res = Atom.fresh "tmp_res" in (* Needed since RecvElim does not handle correctly receive hidden in expression *)
                [
                    (* Initiate session *)
                    auto_fplace (LetStmt(
                        mtype_of_st (spawn_protocol_st schema).value,
                        a_session_0,
                        e2_e(CallExpr(
                            e2var (Atom.builtin "initiate_session_with"),
                            [
                                e2_e(AccessExpr(
                                    e2_e This,
                                    e2var (spawn_outport schema schema_in parent)
                                ));
                                inline_in 
                            ]
                        ))
                    ));
                    (* Send spawn_request to a *)
                    auto_fplace (LetStmt(
                        mtype_of_ct(
                            TResult (
                                mtype_of_st (List.nth (IRMisc.stages_of_st (spawn_protocol_st schema)) 1),
                                mtype_of_var (Atom.builtin "error")
                            )),
                        a_session_1,
                        e2_e(CallExpr(
                            e2var (Atom.builtin "fire"),
                            [
                                e2var a_session_0;
                                e2_e(NewExpr(
                                    e2var (spawn_request schema),
                                    args 
                                ))
                            ]
                        ))
                    ));
                    auto_fplace(IfStmt(
                        e2_e (CallExpr(
                            e2var (Atom.builtin "is_ok"),
                            [ e2var a_session_1 ]
                        )),
                        (* let res  = (receive(s2)?)._0;*)
                        auto_fplace (BlockStmt [
                            auto_fplace(LetStmt(
                                mtype_of_ct (TTuple [mtype_of_var (spawn_response schema); mtype_of_st STEnd]),
                                _tmp_res,
                                e2_e(CallExpr(
                                    e2var (Atom.builtin "receive"),
                                    [
                                        e2_e(CallExpr(
                                            e2var (Atom.builtin "get_ok"),
                                            [e2var a_session_1]
                                        ));
                                    ]
                                ))
                            ));
                            auto_fplace(LetStmt(
                                mtype_of_var (spawn_response schema),
                                a_res,
                                e2_e(AccessExpr(
                                    e2var _tmp_res,
                                    e2var (Atom.builtin "_0")
                                ))
                            ));
                        ]),
                        None
                    ));
                ],
                (* Wait for response and get activation_id *)
                (* (receive(s2)?)._0._0_ *)
                (e2_e(AccessExpr(
                    e2var a_res,
                    e2var (Atom.builtin "_0_")
                ))).value
            end
            | Spawn {inline_in = Some {place; value=_, mtt} } -> Error.perror place "inline_in is ill-typed! %s" (show_main_type mtt) 
        in

        program
        |> rewrite_component_program parent_selector parent_rewriter 
        |> rewrite_exprstmts_program (function _ -> false) select_spawn_with_inline_in rewriter
        |> rewrite_expr_program select_host_spawn rewrite_host_spawn 

    let rewrite_program program =  
        program
        |> eliminate_static_inlinable
        |> eliminate_dynamic_inline_in
        
    (*********************************************************)
    let name = "InlineElim"
    let displayed_pass_shortdescription = "inlining(s) have been eliminated"
    let displayed_ast_name = "IR without inline"
    let show_ast = true 
    let global_at_most_once_apply = false 

    let precondition program = program

    let postcondition program = 
        let error_expr_collector msg parent_opt env e = 
            let parent = match parent_opt with | None -> "Toplevel" | Some p -> Atom.to_string p in
            Error.perror e.place "%s. Parent = %s" msg parent
        in
        let error_term_collector msg parents = 
            let parent = match parents with 
                | [] -> "Toplevel" 
                | xs -> Atom.show_list "::" xs
            in
            Error.error "%s. Parent = %s" msg parent
        in
        (* Check: no more spawn with inline_in  *)
        ignore (collect_expr_program Atom.Set.empty select_spawn_with_inline_in (error_expr_collector "Spawn with inline_in remains in IR after InlineElim") program);
        (* Check: no more inlinable_in *)
        ignore (collect_term_program true select_component_with_inlinable (error_term_collector "Inlinable_in remains in IR after InlineElim") program);
        program 

    let apply_program = rewrite_program
end