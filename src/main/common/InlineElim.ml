(**
    Annotate and transformate AST to add dynamic reflexivity information
    - access list of ports
    - ??
    - TODO maybe add here intermediate_states = { ... }

    Warning:
        Should be one of the last Varda transformation pass, in order not to miss anything
*)

open Core
open AstUtils
open IR
 
open Easy_logging

let logger = Logging.make_logger ("_1_ vardac.InlineElim") Debug [];;

let fplace = (Error.forge_place "InlineElim" 0 0) 
let auto_fplace smth = {place = fplace; value=smth}
include AstUtils2.Mtype.Make(struct let fplace = fplace end)

module Make () = struct
    (* B inlineable in A => "B->Set{A::..}"*)
    let is_inlineable_in = Hashtbl.create 256
    (* A -> Set(B) *)
    let inv_is_inlineable_in = Hashtbl.create 256 

    (* B -> bstruct *)
    let inlineable_cstructs = Hashtbl.create 256

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
    let refresh_atom x = 
        match Hashtbl.find_opt renaming x with
        | Some y -> y
        | None -> 
            let y = Atom.fresh (Atom.value x) in
            Hashtbl.add renaming x y;
            y

    let cl_name schema schema_in = Atom.builtin (Printf.sprintf "Inline_%s_in_%s" (Atom.to_string schema) (Atom.to_string schema_in))

    let name_spawn_inline schema schema_in = Atom.builtin (Printf.sprintf "spawn_%s_in_%s" (Atom.to_string schema) (Atom.to_string schema_in))

    let port_name schema schema_in pname= Atom.builtin (Printf.sprintf "p_%s_in_%s__%s" (Atom.to_string schema) (Atom.to_string schema_in) (Atom.to_string pname))

    let extract_cl_callback (cl:class_structure) port_callback= 
        let cl_callback_name = Hashtbl.find renaming (match fst port_callback.value with
            | AccessExpr ({value=This,_}, {value=VarExpr name, _}) -> name 
            | _ -> failwith "Other callback for inlined inport are not supported yet"
        ) in
        let [cl_callback_in] : method0 list = List.filter_map (map0_place(map0_plgannot(function place -> function | CLMethod m when m.value.name = cl_callback_name -> Some m | _-> None))) cl.body in

        cl_callback_in

    let eliminate_static_inlinable program = 
        let rewriter_inlinable place = function
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
                [ c ]
        in

        let rewriter_inline_in place = function
            | Component {place; value=ComponentStructure cstruct_in} -> 
                let schema_in = cstruct_in.name in
                let schemas = List.of_seq (Atom.Set.to_seq (Hashtbl.find inv_is_inlineable_in schema_in)) in

                let n_body = List.map (function schema -> 
                    let cstruct = Hashtbl.find inlineable_cstructs schema in 

                    (* derive a "class" [InlineB15A] *)
                    let body = List.flatten (List.map (map_places(map_plgannots(function place -> function 
                            | Contract item -> [CLContract item]
                            | Method item -> [CLMethod item] 
                            | State item -> [CLState item]
                            | Term _ -> Error.perror place "component with inner term can not be inlined yet!"
                            | _ -> [] 
                        ))) 

                        (* refresh atom to preserve binding unicity *)
                        (List.map (rename_component_item refresh_atom)
                        cstruct.body))
                    in

                    let cl = {
                        annotations = cstruct.annotations;
                        name = cl_name schema schema_in;
                        body =  body;
                    } in

                    (* add state [instances_B15] in A *)
                    let name_inlined_instances = Atom.builtin (Printf.sprintf "instances_%s" (Atom.to_string schema)) in
                    let inlined_instances = {
                        ghost = false;
                        type0 = mtype_of_ct (TDict (
                            mtype_of_ft TActivationID,
                            mtype_of_ct (TObject (cl_name schema schema_in))
                        ));
                        name = name_inlined_instances;
                        body = Some (e2_e (Block2Expr(Dict,[])))
                    } in

                    (* add method [spawn_B15] in A 
                        spawn_B15 (args of spawn B15) -> ActivationRef<A> 
                        return this;
                    *)
                    let spawn_inline_args = 
                        match  IRMisc.get_onstartup cstruct with
                        | Some schema_onstartup -> List.map (rename_param refresh_atom) schema_onstartup.value.args
                        | None -> []
                    in
                    let spawn_inline = {
                        annotations = [];
                        ghost = false;
                        ret_type = mtype_of_ct (TActivationRef (mtype_of_cvar schema_in));
                        name = name_spawn_inline schema schema_in;
                        args = spawn_inline_args;
                        body = [
                            (*
                                id = ...
                                this.instances_B15[...] = new InlinedB15A(...);
                                return (this with id)  
                            *)
                            failwith "HOW TO ID"
                        ];
                        contract_opt = None;
                        on_destroy = false;
                        on_startup = false;
                    }
                    in


                    (* add inports [p_B15_pb15name] in A + routing to the corresponding instance in [instances_B15] *)
                    let n_inports = 
                        let inports = List.filter (function 
                            | {value={v=Inport _}} -> true
                            | _ -> false
                        ) cstruct.body in
                        List.map (map_place(map_plgannot(function place ->function
                            | Inport {value=port,mt;place} ->
                                assert(port._children = []);

                                let cl_callback_in = extract_cl_callback cl port.callback in

                                let n_args = List.map (rename_param refresh_atom) cl_callback_in.value.args in
                                let a_objid = Atom.fresh "objid" in
                                let a_obj = Atom.fresh "obj" in
                                let n_callback = {
                                    annotations = [];
                                    ghost = false;
                                    ret_type = mtype_of_ft TVoid;
                                    name = Atom.fresh (Printf.sprintf "callback_%s_in_%s" (Atom.to_string schema) (Atom.to_string schema_in));
                                    args = n_args;
                                    body = [
                                        (*
                                            1) get obj_id : sid -> obj_id 
                                            2) obj = instances_B15A[obj_id]
                                            3) return obj.callback(msg, ret); 
                                        *)
                                        auto_fplace (LetStmt(
                                            mtype_of_ft TActivationID,
                                            a_objid,
                                            failwith "get id from session, maybe using hidden_right from interception read docs!"
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
                                failwith "TODO inports inline"
                        ))) inports 
                    in
                    
                    (* add outports [p_B15_pb15name] in A *)
                    let cl_renaming = Hashtbl.create 16 in
                    let n_outports = 
                        let outports = List.filter (function 
                            | {value={v=Outport _}} -> true
                            | _ -> false
                        ) cstruct.body in
                        List.map (map_place(map_plgannot(function place ->function
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
                    (* apply cl_renaming i.e. rename output ports *)
                    let cl = {cl with 
                        body = List.map (rename_class_item (function x -> 
                            match Hashtbl.find_opt cl_renaming x with
                            | Some y -> y
                            | None -> x
                        )) cl.body
                    } in

                    (* add eports [p_B15_pb15name] in A + routing to **all** instances in [instances_B15]*)
                    let n_eports : component_item list = 
                        let eports = List.filter (function 
                            | {value={v=Eport _}} -> true
                            | _ -> false
                        ) cstruct.body in

                        List.map (map_place(map_plgannot(function place ->function
                            | Eport {value=port,mt;place} ->
                                let cl_callback_in = extract_cl_callback cl port.callback in

                                let n_args = List.map (rename_param refresh_atom) cl_callback_in.value.args in
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

                    (* returns *)
                    (List.map (function x -> auto_fplace (auto_plgannot x))
                    ([
                        Term (auto_fplace (auto_plgannot (Class cl))); 
                        State (auto_fplace inlined_instances); 
                        Method (auto_fplace spawn_inline)]))
                    @n_inports@n_outports@n_eports
                ) schemas in

                [ Component {
                    place = place @ fplace; 
                    value=ComponentStructure { cstruct_in with
                        body = cstruct_in.body @ (List.flatten n_body);
                    }} ]

        in
        program
        |> rewrite_term_program select_component_with_inlinable rewriter_inlinable
        |> rewrite_term_program select_component_inline_in rewriter_inline_in
    let eliminate_dynamic_inline_in program = 

        let rewriter mt = function
            | Spawn {inline_in = Some {place; value=e, {value=CType{value=TActivationRef{value=CompType {value=CompTUid schema_in}}}}}} -> begin 
                let schema = match mt with
                    |{value=CType{value=TActivationRef{value=CompType {value=CompTUid schema}}}}
                    |{value=CType{value=TActivationRef{value=CompType {value=TStruct (schema,_)}}}}  -> schema
                    | _ -> Error.perror mt.place "spawn is ill-typed! %s" (show_main_type mt)
                in

                logger#debug "spawn inline %s in %s" (Atom.to_string schema) (Atom.to_string schema_in);


                (* That it is a valid inline according to static_elim *)
                begin
                    try 
                        let possible_schemas_in = Hashtbl.find is_inlineable_in schema in
                        ignore (Atom.Set.find schema_in possible_schemas_in)
                    with Not_found -> Error.error "Can not inline %s in %s ! Use @inline_in annotations." (Atom.to_string schema) (Atom.to_string schema_in)
                end;

                failwith "TODO"
            end
            | Spawn {inline_in = Some {place; value=_, mtt} } -> Error.perror place "inline_in is ill-typed! %s" (show_main_type mtt) 
        in

        rewrite_expr_program select_spawn_with_inline_in rewriter program

    let rewrite_program program =  
        program
        |> eliminate_static_inlinable
        |> eliminate_dynamic_inline_in
        |> failwith "TODO inline"
        
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
                | xs ->
                    Format.fprintf Format.str_formatter "%a" (Error.pp_list "::" (fun out x -> Format.fprintf out "%s" (Atom.to_string x)));
                    Format.flush_str_formatter ()
            in
            Error.error "%s. Parent = %s" msg parent
        in
        (* Check: no more spawn with inline_in  *)
        ignore (collect_expr_program Atom.Set.empty select_spawn_with_inline_in (error_expr_collector "Spawn with inline_in remains in IR after InlineElim") program);
        (* Check: no more inlinable_in *)
        ignore (collect_term_program true select_component_with_inlinable (error_term_collector "Spawn with inlinable_in remains in IR after InlineElim") program);
        program 

    let apply_program = rewrite_program
end