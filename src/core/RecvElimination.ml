open IR
open Easy_logging
open Utils
open AstUtils

let logger = Logging.make_logger "_1_ compspec" Debug [];;
module type Params = sig
    (* Gamma is not used anymore - since IR is annotated with types
    TODO refactor can be removed *)
    val gamma : (IR.expr_variable, IR.main_type) Hashtbl.t
end

type _intermediate_entry1 = ((expr_variable * main_type * main_type) option)
and intermediate_entry1 = ((expr_variable * main_type * main_type) option * expr option * _method0)
and intermediate_entries1 = intermediate_entry1 list
[@@deriving show { with_path = false }]

let print_entry1 (s1_opt, _, _) = show__intermediate_entry1 (s1_opt) 

type intermediate_entry2 = ((expr_variable * main_type * main_type) option * expr option * _method0 * (main_type * expr_variable) list )
and intermediate_entries2 = intermediate_entry2 list
[@@deriving show { with_path = false }]

let print_entries2 entries= 
    let print_entry2 out ((i, (x1_opt,_,_m,fvars)):int*intermediate_entry2) = 
        Format.fprintf out " - Entry %d [%s] <%a> - %a\n" i (Atom.to_string _m.name) (fun out -> function |None -> () | Some (x1, _, _) -> Format.fprintf out "%s" (Atom.to_string x1)) x1_opt (Error.pp_list ";" (fun out (_,x) -> Format.fprintf out "%s" (Atom.to_string x))) fvars 
    in
    Format.fprintf Format.std_formatter "Entries : \n%a\n\n" (Error.pp_list "\n" print_entry2) (List.mapi (fun i e -> (i,e)) entries)



module type Sig = sig
    include IRCompilationPass.Pass
end
let troloc = ref 0 (* TODO remove *)

module Make (Args : Params ) : Sig = struct
    (*
        Architecture remains unchanged - rewriting architecture is done in an other module (to be written)
        - get ride of session.receive only use ports
            TODO FIXME only scan component method at this point
    *)
    include Args


    (*******************************************************)

    (*
        return (new_ports, ((variable hosting the result of the receive, msgt, continuation), session of the receive, method) list)
    *)
    let rec split_body (m:_method0) acc_stmts (acc_method:_method0) : stmt list -> port list *  ((expr_variable * main_type * main_type) option * expr option * _method0) list =
        let fplace = (Error.forge_place "Core.Rewrite.split_body" 0 0) in
        let auto_fplace smth = {place = fplace; value=smth} in

        function
        | [] -> 
            [], [ 
                None, None, { acc_method with 
                        body = acc_method.body @ (List.rev acc_stmts)
                }
            ]
        | {place; value=LetExpr ({value=CType{value=TTuple [msg_t;{value = SType continuation_st}]}}, let_x, {value=(CallExpr ({value=(VarExpr x, _)}, [s; bridge]),_) as e})}::stmts  when Atom.is_builtin x && Atom.hint x = "receive" -> 
        (*
            N.B. We use exatcly one [s] in the final AST when storing the intermediate arguments
        *)
            logger#error "acc_method with %d stmts" (List.length acc_method.body);
            let intermediate_method_name = Atom.fresh ((Atom.hint m.name)^"_intermediate") in
            let intermediate_port_name = Atom.fresh ((Atom.hint m.name)^"_intermediate_port") in
            (*let intermediate_state_name = Areceivetom.fresh ((Atom.hint m.name)^"_intermediate_state") in*)
            

            let intermediate_port = auto_fplace ({
                name = intermediate_port_name;
                input = bridge;
                expecting_st = auto_fplace (SType( auto_fplace (STRecv (msg_t, continuation_st)))); (* TODO need type annotation to support more cases *)
                callback = auto_fplace (AccessExpr (
                    auto_fplace (
                        This, 
                        auto_fplace EmptyMainType
                    ), 
                    auto_fplace (
                        VarExpr intermediate_method_name, 
                        auto_fplace EmptyMainType
                    )
                ), auto_fplace EmptyMainType);
            }, auto_fplace EmptyMainType) in
            
            let tmp_event = Atom.builtin "e" in
            let tmp_session = Atom.builtin "session" in
            (*let tmp_args = Atom.builtin "tmp_args" in*)
            let intermediate_method = {
                annotations = m.annotations;
                ghost = false;
                ret_type = m.ret_type; (* Will be updated lated if needed *) 
                name = intermediate_method_name;
                args = [
                    auto_fplace ( msg_t, tmp_event);
                    auto_fplace ( auto_fplace (SType continuation_st), tmp_session);
                ]; 
                body = []; (*Will be update later*)
                contract_opt = None;
                on_destroy = false;
                on_startup = false;
            } in
            
            let acc_method = { acc_method with 
                ret_type = auto_fplace (CType(auto_fplace(TFlatType TVoid)));
                body = 
                    acc_method.body @
                    (List.rev acc_stmts);
            } in
            let _ports, _methods = split_body m [] intermediate_method stmts in
            intermediate_port::_ports, (Some (let_x, msg_t, auto_fplace (SType continuation_st)), Some s, acc_method)::_methods
        | stmt::stmts -> split_body m (stmt::acc_stmts) acc_method stmts


    (*  Add free vars of each layer

        Example method body:
            1) .... 
                receive
            2) .... free_var a b 
                receive
            3) ....  free_vars x, y, z

            2) full_free_vars = a,b,x,y,z since 2) -> 3) must transmit x, y, z

            therefore we start applying "rewrite_intermediate" from the bottom (3) to the top (1) and we propagate from
            (n) to (n-1) the free_vars of (n)
    *)
    let rec annotate_intermediate (m:_method0) previous_intermediate_args ((x1_opt, s1_opt, m1) : intermediate_entry1)  : (main_type * expr_variable) list * intermediate_entry2 =
        let already_binded = Atom.Set.empty in
        let already_binded, intermediate_args = List.fold_left_map free_vars_stmt already_binded m1.body in
        logger#debug "already binded in annotate_intermediate %s : %s " (Atom.to_string m1.name) (Atom.Set.show already_binded);
        let intermediate_args : (main_type * expr_variable) list =  (List.flatten intermediate_args) in

        (* Correct unspecified type - dirty fix easiest than correct all EmptyMainType (should de done some day) *)
        let mainargs = Hashtbl.of_seq (List.to_seq (List.map (function {value=(mt, x)} -> x, (mt, x)) m.args)) in
        let intermediate_args =  List.map (
            function
            | ({value=EmptyMainType}, x) as arg -> begin 
                match (Hashtbl.find_opt mainargs x) with
                | None -> arg
                | Some arg -> arg
            end
            | arg -> arg
        ) intermediate_args in

        
        (* Safety check *)
        List.iter (function |({value=EmptyMainType}, x) -> logger#error "%s" (Atom.to_string x);assert(false) | _ -> ()) intermediate_args;

        (* Remove components *)
        let intermediate_args = List.filter (function (_, x) -> 
            logger#debug "> 0 - intermediate_arg %s" (Atom.to_string x);
            (Str.string_match (Str.regexp "^[A-Z].*") (Atom.hint x) 0) = false) intermediate_args in

        (* Case "let res = receive(..)"
            res = Tuple.of(e, session); res must not be loaded from an intermediate state
        *)
        let remove_recvbinder vars = match x1_opt with 
        | Some (x1, _, _) -> List.filter (function (_, x) -> 
            logger#debug "> 1 - intermediate_arg %s" (Atom.to_string x);
            logger#debug "> 1 - res var %s" (Atom.to_string x1);
            x <> x1
            ) vars
            | _ -> vars
        in
        let intermediate_args = remove_recvbinder intermediate_args in
        let previous_intermediate_args = remove_recvbinder previous_intermediate_args in

        (* Union previous_intermediate_args and current - preivous is here to propagate the needs of free vars *)
        let union_htbl = Hashtbl.of_seq (List.to_seq (List.map (function (mt, x) -> x, mt) intermediate_args)) in
        Hashtbl.replace_seq union_htbl (List.to_seq (List.map (function (mt, x) -> x, mt) previous_intermediate_args));
        let intermediate_args = List.of_seq (Seq.map (function (x, mt) -> (mt, x)) (Hashtbl.to_seq union_htbl)) in
        
        (* Remove args that are binded inside m1 body *)
        let bubble_intermediate_args = List.filter (function (_, x) -> 
            Atom.Set.find_opt x already_binded = None
        ) intermediate_args in
        bubble_intermediate_args, (x1_opt, s1_opt, m1, intermediate_args) 
        



    (* Add header and footer for each method (i.e. how to propagate arguments) + update args *)
    let rec rewrite_intermediate place (m:_method0) : intermediate_entry2 list -> state list * _method0 list = 
        let fplace = (Error.forge_place "Core.Rewrite.rewrite_intermediate" 0 0) in
        let auto_place smth = {place = place; value=smth} in
        let auto_fplace smth = {place = fplace; value=smth} in
    function
    | [] -> [], []
    | [ (_, _, m, _) ] -> [], [ m ]
    | (x1_opt, s1_opt, m1, intermediate_args1)::(x2_opt, s2_opt, m2, intermediate_args2)::ms -> begin
        logger#debug "rewrite_intermediate between %s and %s" (Atom.to_string m1.name) (Atom.to_string m2.name);
        let intermediate_args = List.map auto_fplace intermediate_args1 in



        let ctype_intermediate_args = auto_fplace (CType (auto_fplace (TTuple (List.map (function arg -> fst arg.value) intermediate_args)))) in
        let tuple_intermediate_args = auto_fplace (BlockExpr (
            Tuple, 
            List.map (function arg -> auto_fplace ((VarExpr (snd arg.value), fst arg.value))) intermediate_args
        ), ctype_intermediate_args) in

        (* let res = receive ()
            build res variable in 2th method
        *)
        let tmp_event = Atom.builtin "e" in
        let tmp_session = Atom.builtin "session" in
        let load_recv_result : stmt list = match x1_opt with
            | None -> []
            | Some (x1, msg_t, continuation_st) -> begin
                (* Tuple<MsgT, continuation_st> x1 = tuple(tmp_event, tmp_session) *)
                [
                    auto_fplace(LetExpr(
                        auto_fplace (CType(auto_fplace (TTuple[ msg_t; continuation_st]))),
                        x1,
                        auto_fplace(BlockExpr(
                            Tuple,
                            [
                                auto_fplace (VarExpr tmp_event, auto_fplace EmptyMainType);
                                auto_fplace (VarExpr tmp_session, auto_fplace EmptyMainType)
                            ]
                        ), auto_fplace EmptyMainType)
                    ))
                ]
            end
        in 

        match intermediate_args with
        |[] ->
            let m2 = { m2 with 
                (* Load args from state TODO add cleansing when timeout *)
                body = load_recv_result @ m2.body} in
            let _states, _methods = rewrite_intermediate place m ((x2_opt, s2_opt,m2, intermediate_args2)::ms) in
            _states, m1::_methods
        | _ -> begin
            let tmp_args = Atom.builtin "tmp_args" in

            let intermediate_state_name = Atom.fresh ((Atom.hint m.name)^"_intermediate_state") in
            (* use to store args between acc_methd and intermediate_method*)
            let intermediate_state_type = 
                auto_fplace(CType(auto_fplace(TDict(
                    auto_fplace(CType (auto_fplace(TFlatType(TUUID)))), (*session id*)
                    ctype_intermediate_args)) 
                ))
            in
            let intermediate_state = auto_fplace (StateDcl {
                ghost = false;
                type0 = intermediate_state_type;
                name = intermediate_state_name;
                body =  Some (auto_fplace (
                    CallExpr (
                        auto_place (
                            VarExpr (Atom.builtin "dict"), auto_place EmptyMainType
                        ), 
                        []
                    ),
                    intermediate_state_type
                ))
            }) in


            let m1 = { m1 with 
                body = m1.body @ [
                    (* Store args in state TODO add cleansing when timeout *)
                    auto_fplace (ExpressionStmt (auto_fplace(CallExpr(
                        auto_fplace(VarExpr (Atom.builtin "add2dict"), auto_fplace EmptyMainType),
                        [
                            auto_fplace(AccessExpr(
                                auto_fplace( This, auto_fplace EmptyMainType),
                                auto_fplace (VarExpr intermediate_state_name, auto_fplace EmptyMainType)
                            ), auto_fplace EmptyMainType);
                            auto_fplace(CallExpr(
                                auto_fplace(VarExpr (Atom.builtin "sessionid"), auto_fplace EmptyMainType),
                                [ match s1_opt with
                                    | Some session -> session (* when we are in the first method of the list*)
                                    | None -> auto_fplace (VarExpr tmp_session, auto_fplace EmptyMainType) (* for all the intermediate (and last) methods *) 
                                ]
                            ), auto_fplace EmptyMainType);
                            tuple_intermediate_args;
                        ]
                    ), auto_fplace EmptyMainType
                    )))
                ]
            } in

            let m2 = { m2 with 
                (* Load args from state TODO add cleansing when timeout *)
                body = 
                load_recv_result @
                [
                    auto_fplace (LetExpr (ctype_intermediate_args, tmp_args, (auto_fplace(CallExpr(
                        auto_fplace(VarExpr (Atom.builtin "remove2dict"), auto_fplace EmptyMainType),
                        [
                            auto_fplace(AccessExpr(
                                auto_fplace (This, auto_fplace EmptyMainType),
                                auto_fplace (VarExpr intermediate_state_name, auto_fplace EmptyMainType)
                            ), auto_fplace EmptyMainType);
                            auto_fplace(CallExpr(
                                auto_fplace(VarExpr (Atom.builtin "sessionid"), auto_fplace EmptyMainType),
                                [ auto_fplace (VarExpr tmp_session, auto_fplace EmptyMainType) ]
                            ), auto_fplace EmptyMainType);
                        ]
                    ), auto_fplace EmptyMainType))));
                ] @ (
                    List.mapi (fun i {value=(mt, x)} ->
                        auto_fplace (LetExpr (mt, x, 
                            auto_fplace( CallExpr(
                                auto_fplace (VarExpr (Atom.builtin "nth"), auto_fplace EmptyMainType),
                                [ 
                                    auto_fplace (LitExpr (auto_fplace (IntLit i)), auto_fplace EmptyMainType);
                                    auto_fplace (VarExpr tmp_args, auto_fplace EmptyMainType) 
                                ]
                            ), auto_fplace EmptyMainType)
                        )) 
                    ) intermediate_args
                )
                @ m2.body; 
            } in


            let _states, _methods = rewrite_intermediate place m ((x2_opt, s2_opt, m2, intermediate_args2)::ms) in
            intermediate_state::_states, m1::_methods
        end
    end

    (* rcev -> toplevel let or toplevel expression statement (return etc ...)*)
    let rec to_X_form place stmt : stmt list =
        let fplace = (Error.forge_place "Core.Rewrite.to_X_form" 0 0) in
        let auto_place smth = {place = place; value=smth} in
        let auto_fplace smth = {place = fplace; value=smth} in

        let flag_debug = ref false in

        (* 
            extract_recv "f(s.recv(...))"
            =>
            [ "fresh_name = s.recv(...)"], "f(fresh_name)"
        *)
        let recv_selector = function
        | (CallExpr ({value=(VarExpr x, _)}, [s; bridge])as e) when Atom.is_builtin x && Atom.hint x = "receive" -> true
        | _ -> false
        in
        let recv_rewriter mt_e = function
        | (CallExpr ({value=(VarExpr x, _)}, [s; bridge]) as e) when Atom.is_builtin x && Atom.hint x = "receive" ->
            begin
                match mt_e.value with
                | CType {value=TTuple [_; {value=SType _}]} -> ()
                | _ -> raise (Error.PlacedDeadbranchError (mt_e.place, Printf.sprintf "to_X_form: type of the receive() is incorrect\n%s\n it should match the following pattern\nCType {value=TTuple [_; {value=SType _}]}" (show_main_type mt_e)))
            end;

            logger#warning ">>>> extract_recvs -> detect receive %s"(Error.show place);
            flag_debug := true;
            let tmp = Atom.fresh "tmp_receive" in
            let recv = auto_place (e, mt_e) in 
            [
                auto_fplace (LetExpr (
                    mt_e,
                    tmp, 
                    recv)
                );
            ], (VarExpr tmp, mt_e )
        in

        let stmt_exclude = function
        | LetExpr (_, _, {value=(CallExpr ({value=(VarExpr x, _)}, [s; bridge]),_) as e}) as stmt  when Atom.is_builtin x && Atom.hint x = "receive" ->
            logger#warning ">>>> to_X_form -> detect receive";
            true 
        | _ -> false 
        in

        let stmts = rewrite_exprstmts_stmt stmt_exclude recv_selector recv_rewriter {place; value=stmt} in
        (* Debug *)
        (*if !flag_debug then (
            (Format.fprintf Format.std_formatter "%a" (Error.pp_list "\n" (fun out stmt -> Format.fprintf out "%s " (show_stmt stmt))) stmts);
            failwith "";
        );*)
        stmts


    let rec rewrite_method0 place (m:_method0) : port list * state list * method0 list = 
        let fplace = (Error.forge_place "Core.Rewrite.rewrite_method0" 0 0) in
        let auto_place smth = {place = place; value=smth} in
        let auto_fplace smth = {place = fplace; value=smth} in

        
        let stmts = List.flatten (List.map (function stmt -> to_X_form stmt.place stmt.value) m.body) in

        (* Debug *)
        let m = {m with body = []} in
        let blblbl = function
        | LetExpr (_, _, {value=(CallExpr ({value=(VarExpr x, _)}, [s; bridge]),_) as e}) as stmt  when Atom.is_builtin x && Atom.hint x = "receive" ->
            incr troloc;
            logger#warning ">>>> to_X_form -> correct at the end %d" !troloc;
            true 
        | _ -> false 
        in
        List.map (rewrite_stmt_stmt false blblbl (fun place stmt -> [stmt])) stmts;
        (* End debug *)


        let intermediate_ports, intermediate_methods = split_body m [] {m with body = []} stmts in
        logger#debug "nbr intermediate_methods %d" (List.length intermediate_methods);
        let _, intermediate_methods = List.fold_left_map (annotate_intermediate m) [] (List.rev intermediate_methods) in
        let intermediate_methods = List.rev intermediate_methods in
        print_entries2 intermediate_methods;

        let intermediate_states, intermediate_methods = rewrite_intermediate place m intermediate_methods in
        let intermediate_methods = List.map auto_place intermediate_methods in

        intermediate_ports, intermediate_states, intermediate_methods
    and rmethod0 m = rewrite_method0 m.place m.value

    (* return name of intermediate states * citems *)
    and rewrite_component_item place : _component_item -> expr_variable list * component_item list = 
    let fplace = (Error.forge_place "Core.Rewrite" 0 0) in
    let auto_place smth = {place = place; value=smth} in
    let auto_fplace smth = {place = fplace; value=smth} in
    function
    | State _ as citem -> [], [auto_place citem]
    | Contract _ as citem -> [], [auto_place citem] 
    | Method m as citem -> 
        let intermediate_ports, intermediate_states, intermediate_methods = rmethod0 m in
        if intermediate_ports = [] then 
            [], [auto_place citem]
        else 
            List.map (function | {value=StateDcl s} -> s.name) intermediate_states
            , (List.map (function p-> auto_fplace (Port p)) intermediate_ports) @ 
            (List.map (function s-> auto_fplace (State s)) intermediate_states)
            @
            (List.map (function m-> auto_fplace (Method m)) intermediate_methods)
    | Port _ as citem -> [], [auto_place citem]
    | Term t -> [], [auto_place (Term (rterm t))]
    | Include _ as citem -> [], [auto_place citem]
    and rcitem citem : expr_variable list * component_item list = rewrite_component_item citem.place citem.value 

    and rewrite_component_dcl place : _component_dcl -> _component_dcl = 
    let fplace = (Error.forge_place "Core.Rewrite" 0 0) in
    let auto_place smth = {place = place; value=smth} in
    let auto_fplace smth = {place = fplace; value=smth} in
    function
    | ComponentAssign _ as cdcl -> cdcl
    | ComponentStructure cdcl -> 
        let body = cdcl.body in

        (* Elimination of sync receiv *)
        let intermediate_state_names, body = List.split(List.map rcitem body) in
        let intermediate_state_names = List.flatten(intermediate_state_names) in
        let body = List.flatten body in

        (*List<Map<UUID, ?>> this.intermediate_states = new ArrayList(); [this. ....]; registration at each creation*)
        let a_intermediate_states = Atom.builtin "intermediate_states" in
        let intermediate_states_index = auto_place(State( auto_place(StateDcl { 
            ghost = false;
            type0 = auto_fplace(CType(auto_fplace (TList(
                        auto_fplace(CType(auto_fplace (TDict(
                            auto_fplace (CType (auto_fplace (TFlatType TUUID))), 
                            auto_fplace (CType (auto_fplace (TFlatType TWildcard))) 
                        ))))
            ))));
            name = a_intermediate_states;
            body = Some (auto_fplace(BlockExpr(List, []), auto_fplace EmptyMainType))
        }))) in
        let body = body @ [intermediate_states_index] in 

        ComponentStructure { cdcl with body }
    and rcdcl cdcl = map_place rewrite_component_dcl cdcl 

    and rewrite_term place = function
    | EmptyTerm -> EmptyTerm
    | Comments c -> Comments c
    | Stmt stmt -> Stmt stmt
    | Component cdcl -> Component (rcdcl cdcl)
    | Function fcdcl -> 
        (* Precondition ensures there is no receive in function_declaration*)
        Function fcdcl
    | Typealias _ as t -> t
    | Typedef _ as t -> t
    | Derive _ as t -> t
    and rterm term = map_place rewrite_term term

    and rewrite_program program = 
        List.map rterm program
    
    (*****************************************************)

    let displayed_pass_shortdescription = "recv has been eliminated from IR"
    let displayed_ast_name = "IR recvelim"
    let show_ast = true

    let receive_selector = function 
        | (CallExpr ({value=(VarExpr x, _)}, [s; bridge])as e) when Atom.is_builtin x && Atom.hint x = "receive" -> true
        | _ -> false
    let receive_collector msg parent_opt env e = 
        let parent = match parent_opt with | None -> "Toplevel" | Some p -> Atom.to_string p in
        Error.error e.place "%s. Parent = %s" msg parent


    let precondition program = 
        (* Check: no receive in function_declaration
                since we can not create async port to handle it (not in a component)
        *)
        let fdcls = List.filter (function |{value=Function _} -> true |_ -> false) program in
        let fdcls = List.map (function |{value=Function fdcl} -> fdcl.value) fdcls in
        List.iter (function (fdcl:_function_dcl) -> 
            List.iter 
                (function stmt -> ignore (collect_expr_stmt (Some fdcl.name) Atom.Set.empty receive_selector (receive_collector "receive can not be defined into function (only inside component methods)") stmt))
                fdcl.body
            ) fdcls;

        program
    let postcondition program = 
        (* Check: no more receive *)
        ignore (collect_expr_program Atom.Set.empty receive_selector (receive_collector "receive() remains in IR after Rewriting") program);
        
        program

    let apply_program = rewrite_program
end