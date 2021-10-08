open IR
open Easy_logging
open Utils
open AstUtils

let logger = Logging.make_logger "_1_ compspec" Debug [];;
module type Params = sig
    val gamma : (IR.variable, IR.main_type) Hashtbl.t
end

module type Sig = sig
    val rewrite_program: IR.program -> IR.program
end

module Make (Args : Params ) : Sig = struct
    (*
        - get ride of session.receive only use ports
            TODO FIXME only scan component method at this point
    *)
    let gamma = Args.gamma
    let rec rewrite_place rewrite_value ({ AstUtils.place ; AstUtils.value}: 'a AstUtils.placed) = 
        let value = rewrite_value place value in
        {AstUtils.place; AstUtils.value}

    let rec rewrite_method0 place : _method0 -> port list * state list * method0 list = 
    let fplace = (Error.forge_place "Core.Rewrite.rewrite_method0" 0 0) in
    let auto_place smth = {place = place; value=smth} in
    let auto_fplace smth = {place = fplace; value=smth} in
    function
    | CustomMethod ({ghost;} as m) -> begin  
        (* 
            extract_recv "f(s.recv(...))"
            =>
            [ "fresh_name = s.recv(...)"], "f(fresh_name)"
        *)
        let rec extract_recvs place : _expr -> stmt list * expr = 
        let auto_place smth = {place = place; value=smth} in
        let auto_fplace smth = {place = fplace; value=smth} in
        function
        | (CallExpr ({value=VarExpr x}, [s; bridge]) as e) when Atom.is_builtin x && Atom.hint x = "receive" ->
            logger#warning ">>>> extract_recvs -> detect receive";
            let tmp = Atom.fresh "tmp_receive" in
            let recv = auto_place e in 
            let e = auto_fplace (VarExpr tmp) in
            (* TODO FIXME Need type annotations at this point *)
            [
                auto_fplace (LetExpr (
                    auto_fplace (CType (auto_fplace(TVar (Atom.fresh_builtin "TODDO")))),
                    tmp, 
                    recv)
                );
            ], e
        (* Classical expr *)
        | VarExpr _ as e -> [], auto_place e
        | AccessExpr (e1, e2) ->
            let stmts1, e1 = extract_recvs e1.place e1.value in
            let stmts2, e2 = extract_recvs e2.place e2.value in
            stmts1@stmts2, auto_fplace (AccessExpr (e1, e2))
        | BinopExpr (e1, op, e2) ->
            let stmts1, e1 = extract_recvs e1.place e1.value in
            let stmts2, e2 = extract_recvs e2.place e2.value in
            stmts1@stmts2, auto_fplace (BinopExpr (e1, op, e2))
        | (CallExpr (e1, es) as e) | (NewExpr (e1, es) as e)->
            let stmts1, e1 = extract_recvs e1.place e1.value in
            let stmtses = List.map (function e -> extract_recvs e.place e.value) es in
            let stmts_n = List.map fst stmtses in
            let es_n = List.map snd stmtses in

            stmts1 @ (List.flatten stmts_n),
            auto_fplace (match e with
                | CallExpr _ -> CallExpr (e1, es_n)
                | NewExpr _ -> NewExpr (e1, es_n)
            )
        | (LambdaExpr _ as e) | (LitExpr _ as e) | (This as e) -> (* Interaction primtives are forbidden in LambdaExpr *)
            [], auto_place e
        | UnopExpr (op, e) -> 
            let stmts, e = extract_recvs e.place e.value in
            stmts, auto_fplace (UnopExpr(op, e))
        | Spawn sp -> begin
            let opt = Option.map (function e -> extract_recvs e.place e.value) sp.at in
            let stmtses = List.map (function e -> extract_recvs e.place e.value) sp.args in
            let stmts_n = List.flatten (List.map fst stmtses) in
            let es_n = List.map snd stmtses in
            
            match opt with
            | None -> stmts_n, auto_fplace (Spawn {
                sp with args = es_n
            })
            | Some (stmts, e) -> stmts_n@stmts, auto_fplace (Spawn {
                sp with 
                    args = es_n;
                    at = Some e;
            })
        end
        | OptionExpr None as e -> [], auto_place e
        | OptionExpr Some e->
            let stmts, e = extract_recvs e.place e.value in
            stmts, auto_fplace (OptionExpr(Some e))
        | ResultExpr (None, None) as e -> [], auto_place e
        | ResultExpr (Some e, None) ->
            let stmts, e = extract_recvs e.place e.value in
            stmts, auto_fplace (ResultExpr(Some e, None))
        | ResultExpr (None, Some e) ->
            let stmts, e = extract_recvs e.place e.value in
            stmts, auto_fplace (ResultExpr(None, Some e))
        | ResultExpr (Some e1, Some e2) ->
            let stmts1, e1 = extract_recvs e1.place e1.value in
            let stmts2, e2 = extract_recvs e2.place e2.value in
            stmts1@stmts2, auto_fplace (ResultExpr(Some e1, Some e2))
        | BlockExpr (b, es) ->
            let stmtses = List.map (function e -> extract_recvs e.place e.value) es in
            let stmts_n = List.flatten (List.map fst stmtses) in
            let es_n = List.map snd stmtses in
            stmts_n, auto_fplace (BlockExpr (b, es_n))
        | Block2Expr (b, ees) ->
            let stmtses = List.map (function (e1, e2) -> 
                extract_recvs e1.place e1.value, 
                extract_recvs e2.place e2.value 
            ) ees in
            let stmts_n = List.flatten (List.map (function (a,b) -> fst a @ fst b) stmtses) in
            let ees_n = List.map  (function (a,b) -> snd a, snd b) stmtses in
            stmts_n, auto_fplace (Block2Expr (b, ees_n))
        in

        (* rcev -> toplevel let or toplevel expression statement (return etc ...)*)
        let rec to_X_form place : _stmt -> stmt list =
        let auto_place smth = {place = place; value=smth} in
        let auto_fplace smth = {place = fplace; value=smth} in
        function
        | LetExpr (_, _, {value=CallExpr ({value=VarExpr x}, [s; bridge]) as e}) as stmt  when Atom.is_builtin x && Atom.hint x = "receive" ->
            logger#warning ">>>> to_X_form -> detect receive";
            [ auto_place stmt ]

        (* Classical expr *)
        | EmptyStmt -> [auto_place EmptyStmt]
        | AssignExpr (x, e) ->
            let estmts, e = extract_recvs e.place e.value in
            estmts @ [ auto_fplace (AssignExpr (x,e)) ]
        | AssignThisExpr (x, e) ->
            let estmts, e = extract_recvs e.place e.value in
            estmts @ [ auto_fplace(AssignThisExpr (x,e)) ]
        | BreakStmt -> [auto_place BreakStmt]
        | ContinueStmt -> [auto_place ContinueStmt]
        | CommentsStmt c -> [auto_place (CommentsStmt c)]
        | ExitStmt i -> [auto_place (ExitStmt i)]
        | ExpressionStmt e -> 
            let estmts, e = extract_recvs e.place e.value in
            estmts @ [ auto_fplace (ExpressionStmt e)]
        | ForStmt (mt, x, e, stmt) ->
            let estmts, e = extract_recvs e.place e.value in
            estmts @ [
                auto_fplace (ForStmt(
                    mt,
                    x, 
                    e, 
                    match to_X_form stmt.place stmt.value with
                    | [stmt] -> stmt
                    | stmts -> auto_fplace (BlockStmt stmts)
                ))
            ]
        | IfStmt (e, stmt1, stmt2_opt) ->
            let estmts, e = extract_recvs e.place e.value in
            estmts @ [auto_fplace (IfStmt (
                e,
                (match to_X_form stmt1.place stmt1.value with
                | [stmt1] -> stmt1
                | stmts1 -> auto_fplace (BlockStmt stmts1))
                ,
                Option.map (function stmt2 ->
                    match to_X_form stmt2.place stmt2.value with
                    | [stmt2] -> stmt2
                    | stmts2 -> auto_fplace (BlockStmt stmts2)
                ) stmt2_opt
            ))]    
        | LetExpr (mt, x, e) ->
            let estmts, e = extract_recvs e.place e.value in
            estmts @ [auto_fplace (LetExpr (mt, x, e))]
        | MatchStmt (e, branches) ->
            let estmts, e = extract_recvs e.place e.value in
            (* interaction primitives are disallowed in pattern *)
            estmts @ [ auto_fplace (MatchStmt (
                e,
                List.map (function (pattern, stmt) ->
                    (
                        pattern,
                        match to_X_form stmt.place stmt.value with
                        | [stmt] -> stmt
                        | stmts -> auto_fplace (BlockStmt stmts)
                        )        
                ) branches
            ))] 
        | ReturnStmt e -> 
            let estmts, e = extract_recvs e.place e.value in
            estmts @ [auto_fplace (ReturnStmt e) ]
        | BlockStmt stmts ->
            [auto_fplace(
                BlockStmt (List.flatten (List.map (function stmt -> to_X_form stmt.place stmt.value) stmts))
            )]
        | GhostStmt stmt -> (* since interaction primitives are forbidden in ghost *)
            [ auto_fplace (
                match to_X_form stmt.place stmt.value with
                | [stmt] -> GhostStmt stmt
                | stmts -> GhostStmt (auto_fplace(BlockStmt stmts))
            ) ]
        in
        
        let stmts = List.flatten (List.map (function stmt -> to_X_form stmt.place stmt.value) m.body) in
        (*
            return (new_ports, ((variable hosting the result of the receive, msgt, continuation), session of the receive, method) list)
        *)
        let rec split_body acc_stmts (acc_method:__method0) : stmt list -> port list *  ((variable * main_type * main_type) option * expr option * __method0) list = function
        | [] -> 
            [], [ 
                None, None, { acc_method with 
                        body = acc_method.body @ (List.rev acc_stmts)
                }
            ]
        | {place; value=LetExpr ({value=CType{value=TTuple [msg_t;{value = SType continuation_st}]}}, let_x, {value=CallExpr ({value=VarExpr x}, [s; bridge]) as e})}::stmts  when Atom.is_builtin x && Atom.hint x = "receive" -> 
        (*
            N.B. We use extacly one [s] in the final AST when storing the intermediate arguments
        *)
            logger#error "acc_mthode with %d stmts" (List.length acc_method.body);
            let intermediate_method_name = Atom.fresh ((Atom.hint m.name)^"_intermediate") in
            let intermediate_port_name = Atom.fresh ((Atom.hint m.name)^"_intermediate_port") in
            (*let intermediate_state_name = Areceivetom.fresh ((Atom.hint m.name)^"_intermediate_state") in*)
            

            let intermediate_port = auto_fplace {
                name = intermediate_port_name;
                input = bridge;
                expecting_st = auto_fplace (SType( auto_fplace (STRecv (msg_t, continuation_st)))); (* TODO need type annotation to support more cases *)
                callback = auto_fplace (AccessExpr (
                    auto_fplace This, 
                    auto_fplace (VarExpr intermediate_method_name)
                ));
            } in
            
            let tmp_event = Atom.fresh_builtin "e" in
            let tmp_session = Atom.fresh_builtin "session" in
            (*let tmp_args = Atom.fresh_builtin "tmp_args" in*)
            let intermediate_method = {
                ghost = false;
                ret_type = m.ret_type; (* Will be updated lated if needed *) 
                name = intermediate_method_name;
                args = [
                    auto_fplace ( msg_t, tmp_event);
                    auto_fplace ( auto_fplace (SType continuation_st), tmp_session);
                ]; 
                body = []; (*Will be update later*)
                contract_opt = None
            } in
            
            let acc_method = { acc_method with 
                ret_type = auto_fplace (CType(auto_fplace(TFlatType TVoid)));
                body = 
                    acc_method.body @
                    (List.rev acc_stmts);
            } in
            let _ports, _methods = split_body [] intermediate_method stmts in
            intermediate_port::_ports, (Some (let_x, msg_t, auto_fplace (SType continuation_st)), Some s, acc_method)::_methods
        | stmt::stmts -> split_body (stmt::acc_stmts) acc_method stmts
        in

        let intermediate_ports, intermediate_methods =  split_body [] {m with body = []} stmts in
        
        (* Add header and footer for each method (i.e. how to propagate arguments) + update args *)
        let rec rewrite_intermediate : ((variable * main_type * main_type) option * expr option * __method0) list -> state list * __method0 list = function
        | [] -> [], []
        | [ (_, _, m) ] -> [], [ m ]
        | (x1_opt, s1_opt, m1)::(x2_opt, s2_opt, m2)::ms -> begin
            (*let already_binded = Atom.Set.of_seq (List.to_seq (List.map (function {value=(_,x)} -> x) m.args)) in
            Atom.Set.iter (function x -> logger#error "already binded2 %s" (Atom.to_string x)) already_binded;*)
            let already_binded = Atom.Set.empty in
            let _, intermediate_args = List.fold_left_map free_vars_stmt already_binded m2.body in
            let intermediate_args : variable list = List.flatten intermediate_args in

            (* Remove components *)
            let intermediate_args = List.filter (function x -> 
                logger#error "<<>> %s" (Atom.to_string x);
                (Str.string_match (Str.regexp "^[A-Z].*") (Atom.hint x) 0) = false) intermediate_args in
            
            let intermediate_args = match x1_opt with 
                | Some (x1, _, _) -> List.filter (function x -> x <> x1) intermediate_args
                | _ -> intermediate_args
            in
            let intermediate_args = (List.map 
                (function x -> (
                    auto_fplace(
                        Hashtbl.find gamma x
                        ,x
                    )
                ))
                intermediate_args
            ) in

            let ctype_intermediate_args = auto_fplace (CType (auto_fplace (TTuple (List.map (function arg -> fst arg.value) intermediate_args)))) in
            let tuple_intermediate_args = auto_fplace (BlockExpr (
                Tuple, 
                List.map (function arg -> auto_fplace (VarExpr (snd arg.value))) intermediate_args
            )) in

            match intermediate_args with
            |[] ->
                let _states, _methods = rewrite_intermediate ((x2_opt, s2_opt,m2)::ms) in
                _states, m1::_methods
            | _ -> begin
                let tmp_event = Atom.fresh_builtin "e" in
                let tmp_session = Atom.fresh_builtin "session" in
                let tmp_args = Atom.fresh_builtin "tmp_args" in

                let intermediate_state_name = Atom.fresh ((Atom.hint m.name)^"_intermediate_state") in
                (* use to store args between acc_methd and intermediate_method*)
                let intermediate_state = auto_fplace (StateDcl {
                    ghost = false;
                    kind = Local;
                    type0 = auto_fplace(CType (auto_fplace(TDict(
                        auto_fplace(CType (auto_fplace(TFlatType(TUUID)))), (*session id*)
                        ctype_intermediate_args
                    ))));
                    name = intermediate_state_name;
                    body =  Some (auto_fplace (CallExpr (
                        auto_place (VarExpr (Atom.fresh_builtin "dict")), [])))
                }) in

                let m1 = { m1 with 
                    body = m1.body @ [
                        (* Store args in state TODO add cleansing when timeout *)
                        auto_fplace (ExpressionStmt (auto_fplace(CallExpr(
                            auto_fplace(VarExpr (Atom.fresh_builtin "add2dict")),
                            [
                                auto_fplace(AccessExpr(
                                    auto_fplace This,
                                    auto_fplace (VarExpr intermediate_state_name)
                                ));
                                auto_fplace(CallExpr(
                                    auto_fplace(VarExpr (Atom.fresh_builtin "sessionid")),
                                    [ match s1_opt with
                                        | Some session -> session (* when we are in the first method of the list*)
                                        | None -> auto_fplace (VarExpr tmp_session) (* for all the intermediate (and last) methods *) 
                                    ]
                                ));
                                tuple_intermediate_args;
                            ]
                        ))))
                    ]
                } in

                let m2 = { m2 with 
                    (* Load args from state TODO add cleansing when timeout *)
                    body = 
                    (match x1_opt with
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
                                            auto_fplace (VarExpr tmp_event);
                                            auto_fplace (VarExpr tmp_session)
                                        ]
                                    ))
                                ))
                            ]
                        end
                    )@
                    [
                        auto_fplace (LetExpr (ctype_intermediate_args, tmp_args, (auto_fplace(CallExpr(
                            auto_fplace(VarExpr (Atom.fresh_builtin "remove2dict")),
                            [
                                auto_fplace(AccessExpr(
                                    auto_fplace This,
                                    auto_fplace (VarExpr intermediate_state_name)
                                ));
                                auto_fplace(CallExpr(
                                    auto_fplace(VarExpr (Atom.fresh_builtin "sessionid")),
                                    [ auto_fplace (VarExpr tmp_session) ]
                                ));
                            ]
                        )))));
                    ] @ (
                        List.mapi (fun i {value=(mt, x)} ->
                            auto_fplace (LetExpr (mt, x, 
                                auto_fplace( CallExpr(
                                    auto_fplace (VarExpr (Atom.fresh_builtin "nth")),
                                    [ 
                                        auto_fplace (LitExpr (auto_fplace (IntLit i)));
                                        auto_fplace (VarExpr tmp_args) 
                                    ]
                                ))
                            )) 
                        ) intermediate_args
                    )
                    @ m2.body; 
                } in


                let _states, _methods = rewrite_intermediate ((x2_opt, s2_opt, m2)::ms) in
                intermediate_state::_states, m1::_methods
            end
        end
        in

        let intermediate_states, intermediate_methods = rewrite_intermediate intermediate_methods in
        let intermediate_methods = List.map (function m -> auto_fplace (CustomMethod m)) intermediate_methods in

        intermediate_ports, intermediate_states, intermediate_methods
    end
    | OnStartup m -> 
        let ps, ss, ms = rmethod0 m in
        assert( ms <> []);
        ps, ss, auto_fplace (OnStartup (List.hd ms)) :: (List.tl ms)
    | OnDestroy m -> 
        let ps, ss, ms = rmethod0 m in
        assert( ms <> []);
        ps, ss,  auto_fplace (OnDestroy (List.hd ms)) :: (List.tl ms)
    and rmethod0 m = rewrite_method0 m.place m.value

    and rewrite_component_item place : _component_item -> component_item list = 
    let fplace = (Error.forge_place "Core.Rewrite" 0 0) in
    let auto_place smth = {place = place; value=smth} in
    let auto_fplace smth = {place = fplace; value=smth} in
    function
    | State _ as citem -> [auto_place citem]
    | Contract _ as citem -> [auto_place citem] 
    | Method m as citem -> 
        let intermediate_ports, intermediate_states, intermediate_methods = rmethod0 m in
        if intermediate_ports = [] then [auto_place citem]
        else (List.map (function p-> auto_fplace (Port p)) intermediate_ports) @ 
        (List.map (function s-> auto_fplace (State s)) intermediate_states)
        @
        (List.map (function m-> auto_fplace (Method m)) intermediate_methods)
    | Port _ as citem -> [auto_place citem]
    | Term t -> [auto_place (Term (rterm t))]
    | Include _ as citem -> [auto_place citem]
    and rcitem citem : component_item list = rewrite_component_item citem.place citem.value 

    and rewrite_component_dcl place : _component_dcl -> _component_dcl = function
    | ComponentAssign _ as cdcl -> cdcl
    | ComponentStructure cdcl -> 
        ComponentStructure {
            cdcl with 
                body = List.flatten (List.map rcitem cdcl.body) 
        }
    and rcdcl cdcl = rewrite_place rewrite_component_dcl cdcl 

    and rewrite_term place = function
    | EmptyTerm -> EmptyTerm
    | Comments c -> Comments c
    | Stmt stmt -> Stmt stmt
    | Component cdcl -> Component (rcdcl cdcl)
    | Function fcdcl -> Function fcdcl
    | Typealias _ as t -> t
    | Typedef _ as t -> t
    and rterm term = rewrite_place rewrite_term term

    and rewrite_program terms = 
        List.map rterm terms
end