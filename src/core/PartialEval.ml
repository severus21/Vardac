open IR
open Easy_logging
let logger = Logging.make_logger "_1_ compspec" Debug [Cli Debug];;

(* The source calculus. *)
module S = IR 
(* The target calculus. *)
module T = IR 

(* Environment *)
module Env = Atom.AtomMap 

type env = { 
    named_types: (main_type option) Env.t (* namely the list of typedef with their current definition, 
    none means that it is an abstract type *)
} [@@deriving fields] 

let fresh_env () = { 
    named_types = Env.empty;
}

let bind_named_types (env:env) key value : env = 
    { env with named_types = Env.add key value env.named_types}

let map_snd f = function (x,y) -> (x, f y)

(*debug only
let print_env env =
    let print_keys env = Env.iter (fun x _ -> Printf.printf "%s;" (Atom.atom_to_str x)) env in

    print_newline ();
    print_string "Env = {";

    List.iter (
        function (name, l) -> print_string ("\t"^name^"\n\t\t"); print_keys (l env);print_newline (); 
    ) [("named_types", named_types)];
    
    print_string "}";
    print_newline ()
*)

let rec peval_place peval_value env ({ AstUtils.place ; AstUtils.value}: 'a AstUtils.placed) = 
    let env, value = peval_value env place value in
    env, {AstUtils.place; AstUtils.value}

let rec peval_composed_type env place : _composed_type -> env * _composed_type = function ct -> env, ct
and pe_ctype env: composed_type -> env * composed_type = peval_place peval_composed_type env


and peval_stype env place : _session_type -> env * _session_type = 
    let aux_entry (x, st, cst_opt) = 
        let _, st' = pe_stype env st in
        let cst_opt' = Option.map (fun elmt -> snd (peval_applied_constraint env elmt)) cst_opt in
        (x, st', cst_opt')
    in function  
    | STEnd -> env, STEnd
    | STInline x -> begin
        (*
        let rec assert_is_stype = function  
            | SType _ -> () 
            | ConstrainedType (mt, _) -> assert_is_stype mt.value  
            | _ -> Error.error place "STInline parameter can not be resolved to a session types. It is resolved to %s" (Error.show place)
        in
        *)

        try
            let mt = Env.find x env.named_types in
            (* assert_is_stype mt.value;
            env, mt.value*)
            match mt with 
            | Some {value=SType st; _} -> env, st.value 
            (* FIXME do we want to have the place of the inline (current behviour) or the place of the typedef ??*)
            | _ -> Error.error place "STInline parameter can not be resolved to a session types. It is resolved to %s" (Error.show place)
            
        with Not_found -> raise (Error.DeadbranchError "Unbounded inline variable, this should have been checked by the cook pass.")
    end
    | STBranch entries -> 
        env, STBranch (List.map aux_entry entries) 
    | STRec (x, st) -> 
        env, STRec (x, snd (pe_stype env st))
    | STRecv (mt, st) -> 
        let _, mt' = pe_mtype env mt in
        let _, st' = pe_stype env st in
        env, STRecv (mt', st')
    | STSelect entries -> 
        env, STSelect (List.map aux_entry entries) 
    | STSend (mt, st) -> 
        let _, mt' = pe_mtype env mt in
        let _, st' = pe_stype env st in
        env, STSend (mt', st')
    | STVar (x, constraint_opt) -> 
        env, STVar (x, Option.map (function elmt -> snd (peval_applied_constraint env elmt)) constraint_opt )

and pe_stype env: session_type -> env * session_type = peval_place peval_stype env

and peval_mtype env place : _main_type -> env * _main_type = function 
| CType {value=TVar x; _} as mt when  not(Atom.is_builtin x) ->
    (* get ride of session types aliasing *)
    (* TODO rename aux *)
    logger#info "TVar %s" (Atom.value x);

    let rec aux x =
        try 
            let mt' = Env.find x env.named_types in    
            match mt' with 
            | Some {value=SType st; _} -> 
                SType st (* NB: we use the place of the alias and not the place of the definition here *)
            | Some{value=CType {value=TVar y; _}; _} -> aux y (* keep searching the root *)
            | _ -> mt
        with Not_found -> raise (Error.DeadbranchError "Unbounded type variable, this should have been checked by the cook pass.")
    in
    env, aux x 
| CType ct -> env, CType (snd (pe_ctype env ct))
| SType st -> env, SType (snd(pe_stype env st)) 
| ConstrainedType (mt, cst) -> env, ConstrainedType (
    snd(pe_mtype env mt), 
    snd (peval_applied_constraint env cst)) 
| elmt -> env, elmt (*TODO*)
and pe_mtype env: main_type -> env * main_type = peval_place peval_mtype env

(******************************** Constraints ********************************)
and peval_applied_constraint env : applied_constraint -> env * applied_constraint = function x -> env, x 

(************************************ Expr & Stmt *****************************)
and peval_expr env place : _expr -> env * _expr = function 
| CallExpr ({value=T.VarExpr name; _}, []) when Atom.hint(name) = "bridge" ->
     Error.error place "bridge expression must not be used outside the right-handside of a let"  
| x -> env, x 
and pe_expr env: expr -> env * expr = peval_place peval_expr env

and peval_stmt env place : _stmt -> env * _stmt = function 
| EmptyStmt -> env, EmptyStmt
| AssignExpr (x, e) -> env, AssignExpr (x,  snd(pe_expr env e ))
| AssignThisExpr (x, e) -> env, AssignExpr (x,  snd(pe_expr env e ))
| BlockStmt stmts -> 
    env, BlockStmt (List.map (function stmt -> snd ((pe_stmt env) stmt)) stmts)
| BreakStmt -> env, BreakStmt
| CommentsStmt c -> env, CommentsStmt c 
| ContinueStmt -> env, ContinueStmt
| ExitStmt i -> env, ExitStmt i
| ExpressionStmt e -> env, ExpressionStmt (snd(pe_expr env e))
| ForStmt (x, e, stmt) -> env, ForStmt (x, snd (pe_expr env e), snd (pe_stmt env stmt))
| IfStmt (e, stmt, stmt_opt) -> 
    env, IfStmt (
        snd (pe_expr env e), 
        snd (pe_stmt env stmt),
        Option.map (function elmt -> snd (pe_stmt env elmt)) stmt_opt
    )
| LetExpr ({value=CType {value= TBridge t_b; _}; _ } as let_left, let_x, e_b) -> begin 
    match e_b.value with
    | CallExpr ({value=T.VarExpr name; _}, []) when Atom.hint(name) = "bridge" -> 
        let protocol = match t_b.protocol.value with
        | SType st -> st
        | _ -> Error.error t_b.protocol.place "Third argument of Bridge<_,_,_> must be (partially-evaluated> to a session type"
        in

        env, LetExpr (
            let_left,
            let_x, 
            {
                place = e_b.place;
                value = LitExpr {
                    place = e_b.place;    
                    value = Bridge {
                        id  = Atom.fresh "bridge";
                        protocol = protocol;
                    } 
                }
            }
        )
    | _ -> Error.error place "The right-handside of a Bridge<_,_,_> must be partially evaluated to a bridge literal" 
end
| MatchStmt (e, entries) -> 
    env, MatchStmt (
        snd(pe_expr env e), 
        List.map ( function (e,stmt) -> snd(pe_expr env e), snd(pe_stmt env stmt)) entries
    )
| LetExpr (mt, x, e) -> env, LetExpr( snd(pe_mtype env mt), x, snd(pe_expr env e))
| ExpressionStmt e -> 
    let new_env, new_e = pe_expr env e in
    new_env, (ExpressionStmt new_e)
| x -> env, x
and pe_stmt env: stmt -> env * stmt = peval_place peval_stmt env


(************************************ Component *****************************)
and peval_contract env place contract =
    let pre_binders = List.map (
        function  (mt, x, e) -> 
            snd (pe_mtype env mt),
            x,
            snd (pe_expr env e )
        ) contract.pre_binders in
    let ensures = Option.map (function e -> snd (pe_expr env e )) contract.ensures in
    let returns = Option.map (function e -> snd (pe_expr env e )) contract.returns in

    env, {contract with
        pre_binders;
        ensures;
        returns 
    }
and pe_contract env: contract -> env * contract = peval_place peval_contract env

and peval_param env place (mt, x) = 
    env, (snd(pe_mtype env mt), x)
and pe_param env: param -> env * param = peval_place peval_param env

and peval_method env place = function
| CustomMethod m -> 
    env, CustomMethod {m with
            ret_type = snd(pe_mtype env m.ret_type);
            args = List.map (function param -> snd(pe_param env param)) m.args;
            abstract_impl = Option.map (function stmt -> snd(pe_stmt env stmt)) m.abstract_impl;
            contract_opt = Option.map (function c -> snd(pe_contract env c)) m.contract_opt

    } 
| OnStartup m -> env, OnStartup (snd (pe_method env m)) 
| OnDestroy m -> env, OnDestroy (snd (pe_method env m))
and pe_method env: method0 -> env * method0 = peval_place peval_method env

and peval_port env place port = 
    let expecting_st = snd(pe_mtype env port.expecting_st) in 
    
    begin
        match expecting_st.value with
        | SType _ -> ()
        | _ -> Error.error place "port expecting value must be a session type"
    end;

    env, { port with
        input =  snd(pe_expr env port.input);
        expecting_st;
        callback = snd(pe_expr env port.callback)
    }
and pe_port env: port -> env * port = peval_place peval_port env

and peval_state env place = function 
| StateDcl s -> env, StateDcl {s with 
    type0 = snd(pe_mtype env s.type0);
    init_opt = Option.map (function e -> snd(pe_expr env e)) s.init_opt
} 
| StateAlias _ -> failwith "partial-evaluation does not support yet StateAlias" 
and pe_state env: state -> env * state = peval_place peval_state env

and peval_component_item env place : _component_item -> env * _component_item = function 
| Contract c -> env, Contract (snd(pe_contract env c))
| Include cexpr -> env, Include (snd(pe_component_expr env cexpr))
| Method m -> env, Method (snd(pe_method env m))
| Port p -> env, Port (snd(pe_port env p))
| State s -> env, State (snd(pe_state env s))
| Term t -> env, Term (snd(pe_term env t))

and pe_component_item env: component_item -> env * component_item = peval_place peval_component_item env

and peval_component_dcl env place : _component_dcl -> env * _component_dcl = function  
| ComponentAssign {name; args; value} -> env, ComponentAssign {
    name;
    args = List.map (function param -> snd(pe_param env param)) args;
    value = snd(pe_component_expr env value) 
} 
| ComponentStructure cdcl ->
    (* Collect contracts *)
    let collect_contracts env (x:component_item) = 
        match x.value with
        | Contract c -> Env.add c.value.method_name c env 
        | _ -> env 
    in
    let contracts : IR.contract Env.t = List.fold_left collect_contracts Env.empty cdcl.body in (* method_name -> contract *)

    (* Remove contracts from body and pair method with contracts *)
    let rec get_method_name (m: method0) = 
        match m.value with
        | CustomMethod m -> m.name
        | OnDestroy m | OnStartup m -> get_method_name m
    in
    let body = List.filter_map (function (item:component_item) ->
        match item.value with 
        | Contract _ -> None 
        | Method m -> begin
            let rec aux (m: method0) = 
                match m.value with
                | CustomMethod _m -> begin
                        let contract : contract = (Env.find _m.name contracts) in
                        { AstUtils.place; value = (CustomMethod { _m with contract_opt = Some contract }) }
                end 
                | OnDestroy m -> { AstUtils.place; value = OnDestroy (aux m) }
                | OnStartup m -> { AstUtils.place; value = OnStartup (aux m) }
            in
            try
                Some { AstUtils.place; value = Method (aux m) }
            with Not_found ->
                Some item 
        end
        | x -> Some item 
        ) cdcl.body in
    
    let new_env, citems = List.fold_left_map pe_component_item env body in 
    new_env, ComponentStructure {cdcl with body = citems }

and pe_component_dcl env: component_dcl -> env * component_dcl = peval_place peval_component_dcl env

(********************** Manipulating component structure *********************)
and peval_component_expr env place = function
| VarCExpr x -> env, VarCExpr x
| AppCExpr (cexpr1, cexpr2) -> env, AppCExpr (snd (pe_component_expr env cexpr1), snd (pe_component_expr env cexpr2)) 
| UnboxCExpr e -> env, UnboxCExpr (snd(pe_expr env e)) 
| AnyExpr e -> env, AnyExpr (snd(pe_expr env e)) 
and pe_component_expr env: component_expr -> env * component_expr = peval_place peval_component_expr env

(************************************ Program *****************************)
and peval_term env place : _term -> env * _term = function
| Comments c -> env, Comments c
| Stmt stmt -> map_snd (fun x -> Stmt x) (pe_stmt env stmt)
| Component comp -> map_snd (fun x -> Component x) (pe_component_dcl env comp)
| Typedef (x, mt_opt) -> begin
    match mt_opt with
    | None -> 
        let new_env = bind_named_types env x None in
        new_env, Typedef (x, None)
    | Some mt -> 
        let _, mt = pe_mtype env mt in 
        let new_env = bind_named_types env x (Some mt) in
        new_env, Typedef (x, Some mt)
end
and pe_term env: term -> env * term = peval_place peval_term env

and peval_program (terms: IR.program) : IR.program = 
    (*  Hydrate env, namely:
        -  collect the contract
        And remove contracts from component_item + add contract inside method0 structure *)
    let env, program = List.fold_left_map pe_term (fresh_env ()) terms in
    program

