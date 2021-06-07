open Core
open Easy_logging
open Fieldslib

let plg_name = "Akka"
let logger = Logging.make_logger ("_1_ compspec.plg."^plg_name) Debug [];;

(* The source calculus. *)
module S = IRI 
(* The target calculus. *)
module T = Ast 

(* The translation of a complete program. *)

let fst3 (x,y,z) = x

(* Environments map strings to atoms. *)
module AtomEnv = Atom.AtomMap 
module LabelsEnv = Atom.AtomsMap 

type finish_env = {
    labels: T.variable LabelsEnv.t; 
    events: T.event AtomEnv.t; 
} [@@deriving fields] 

let fresh_fenv () = {
    labels      = LabelsEnv.empty;
    events      = AtomEnv.empty;
}

let bind_event (env:finish_env) key value =
    { env with events = AtomEnv.add key value env.events }

(* FIXME: do we need to make fenv depend of the scope, if yes follow the cook architecture *)
let general_fenv = ref (fresh_fenv())

(* XXXX *)

type items_grps = { 
    methods: S.method0 list; 
    states: S.state list; 
    nested: S.component_dcl list; 
    ports: S.port list;
    typedefs: (S.variable * S._typedef_body) list;
    others: S.term list
}

let fresh_items_grp () = { 
    methods     = [];
    states      = [];
    nested      = [];
    ports       = [];
    typedefs    = [];
    others      = [];
}

let group_cdcl_by (citems:  S.component_item list) : items_grps =
    let dispatch grp (citem: S.component_item) = match citem.value with
        | S.Contract _ -> raise (Core.Error.DeadbranchError  "Contract term should have been remove from AST by the cook pass and binded to a method")
        | S.Include _ -> Core.Error.error citem.place "Include is not yet supported in Akka plg"
        | S.Method  m-> {grp with methods=m::grp.methods}
        | S.State f-> {grp with states=f::grp.states}
        | S.Port p -> {grp with ports=p::grp.ports}
        (* Shallow search of Typedef, FIXME do we need deep search ?*)
        | S.Term {place; value=S.Typedef (v,mt)} -> {grp with typedefs=(v,mt)::grp.typedefs}
        | S.Term {place; value=S.Component cdcl} -> {grp with nested=cdcl::grp.nested}
        | S.Term t -> {grp with others=t::grp.others}
    in

    let grp = (List.fold_left  dispatch (fresh_items_grp ()) citems) in 
        {
            methods=(List.rev grp.methods);
            states=(List.rev grp.states) ; 
            nested=(List.rev grp.nested) ; 
            ports=(List.rev grp.ports) ; 
            typedefs=(List.rev grp.typedefs) ; 
            others=(List.rev grp.others)
        }

(************************************* Base types ****************************)


(************************************ Types **********************************)
let rec finish_ctype place : S._composed_type ->  T.ctype = function
    | S.TArrow (m1, m2) -> T.TFunction (
        fst(fmtype m1), 
        fst(fmtype m2)
    )

    | S.TVar x -> T.TVar x 
    | S.TFlatType ft -> begin match ft with  
        | S.TBool -> T.Atomic "boolean"
        | S.TInt -> T.Atomic "int"
        | S.TFloat -> T.Atomic "float"
        | S.TStr -> T.Atomic "String"
        | S.TVoid -> TVoid
        | _ -> Core.Error.error place "TActivationInfo/Place/VPlace/Label type not yey supported."
    end
    | S.TDict (m1, m2) -> T.TMap (fst(fmtype m1), fst(fmtype m2))
    | S.TList mt -> T.TList (fst(fmtype mt))
    | S.TOption mt -> T.TOption (fst(fmtype mt))
    | S.TResult (m1, m2) -> T.TResult (fst(fmtype m1), fst(fmtype m2))
    | S.TSet mt -> T.TSet (fst(fmtype mt))
    | S.TTuple mts ->  T.TTuple (List.map (fun x -> (fst(fmtype x))) mts)
    | S.TBridge b -> T.Atomic "lg4dc.protocol.Bridge" (*TODO*)
and fctype : S.composed_type ->  T.ctype = function ct -> finish_ctype ct.place ct.value 

and event_name_of_ftype place : S.flat_type -> string = function
| S.TActivationInfo -> Core.Error.error place "TActivationInfo type can not be translated to a serializable Akka event."
| S.TBool -> "Bool"
| S.TInt -> "Int"
| S.TFloat -> "Float"
| S.TStr -> "Str"
| S.TLabel -> "Label"
| S.TVoid -> "Void"
| S.TPlace -> "Place"
| S.TVPlace -> "VPlace"

and event_name_of_ctype place : S._composed_type -> string = function 
| S.TArrow _ -> Core.Error.error place "Arrow type can not be translated to a serializable Akka event."
| S.TVar x -> Atom.value x (* FIXME do we need also the hint ?? *)
| S.TFlatType ft -> event_name_of_ftype place ft
| S.TDict (mt1, mt2) | TResult (mt1, mt2) -> (_event_name_of_mtype mt1) ^ (_event_name_of_mtype mt2) 
| S.TList mt | S.TOption mt | S.TSet mt -> _event_name_of_mtype mt 
| S.TTuple mts -> List.fold_left (fun acc mt -> acc ^ (_event_name_of_mtype mt)) "" mts

and _event_name_of_mtype : S.main_type ->  string = function
| {place; value=S.CType ct} -> event_name_of_ctype ct.place ct.value
| {place; _} -> Core.Error.error place "Session types, component types can not be translated to a serializable Akka event."
and event_name_of_mtype  mt : T.variable = 
(* TODO we should traverse all the recurisve type to ensure that we do not send/receive unserializable things -> maybe we should do this verification after the partial evaluation pass *)
Atom.fresh (Printf.sprintf "%sEvent" (_event_name_of_mtype mt ))

(*
take the list of labels of a STBranch/STSelect and generate a related EventName. This Event will have a value of type string with the label value inside.
*)
and event_name_of_labels (labels: S.variable list) : T.variable = 
    try
        LabelsEnv.find labels !general_fenv.labels
    with Not_found -> begin
        let name = Atom.fresh "LabelEvent" in

        (* Update *)
        let labels_env = LabelsEnv.add labels name !general_fenv.labels in 
        general_fenv := {!general_fenv with labels = labels_env };

        name
    end

(* Represent an ST object in Java type *)
and encode_stype ({place;value} : S.session_type) : T.ctype = 
match value with
    | S.STEnd -> T.TVar (Atom.fresh_builtin "lg4dc.protocol.STEnd") 
    | (S.STSend (mt, st) as st0) | (S.STRecv (mt, st) as st0) -> begin 
        match fmtype mt with
        | ct, [] ->
            T.TParam (
                T.TVar (Atom.fresh_builtin (match st0 with | S.STSend _ -> "lg4dc.protocol.STSend" | STRecv _ -> "lg4dc.protocol.STSend")),
                [ct; encode_stype st]
            )
        | _,_ -> raise (Core.Error.DeadbranchError "finish_stype : STSend/STRecv type should not be a session type.")
    end
    | (S.STBranch xs as st0) | (S.STSelect xs as st0) ->
        let rec built_t_hlist = function
            | [] -> T.TVar (Atom.fresh_builtin "lg4dc.protocol.HNil")
            | st::sts -> T.TParam( 
                T.TVar (Atom.fresh_builtin "lg4dc.protocol.HCons"), [
                    T.TParam( 
                        T.TVar (Atom.fresh_builtin "lg4dc.protocol.STEntry"),
                        [encode_stype st]
                    )
                ] @ [(built_t_hlist sts)] 
            )
        in

        let continuation_st = built_t_hlist (List.map (fun (_, st, _) -> st) xs) in

        T.TParam (
            T.TVar (Atom.fresh_builtin (match st0 with | S.STBranch _ -> "lg4dc.protocol.STBranch" | S.STSelect _ -> "lg4dc.protocol.STSelect")),
            [continuation_st]
        )
    | S.STVar _ -> T.TVar (Atom.fresh_builtin ("lg4dc.protocol.STVar"))
    | S.STRec (_,st) ->
        T.TParam (
            T.TVar (Atom.fresh_builtin "lg4dc.protocol.STRec"),
            [ encode_stype st]
        )

    | S.STInline x -> 
        raise (Error.DeadbranchError "STInline should remains outside the codegen part, it should have been resolve during the partial evaluation pass.")
(* @param k order of the mtype in the protocol *)
(*
        @return (ct_opt, events) -> ct_opt is 
*)
and stype_to_events ?k:(k=0) ({place;value} : S.session_type) : T.event list  = 
match value with 
    | S.STEnd -> [] (* Optimization: generation of a mock EndEvent is not needed *)
    | S.STSend (mt, st) | S.STRecv (mt, st) -> begin 
        let name =  event_name_of_mtype mt in
        let events = stype_to_events ~k:(k+1) st in 

        match fmtype mt with
        | ct, [] ->
            {
                T.vis=T.Public; 
                T.name= name;
                T.kind=T.Event; 
                T.args=[(ct, Atom.fresh_builtin "value")]
            }::events
        | _,_ -> raise (Core.Error.DeadbranchError "finish_stype : STSend/STRecv type should not be a session type.")
    end
    | (S.STBranch xs as st0 )| (S.STSelect xs as st0) ->
        let labels = List.fold_left (fun acc (label, _,_) -> label::acc) [] xs in

        let mock_place : Error.place = Error.forge_place "plugins/akka/Finish.ml" 0 0 in
        let ct = fst(finish_mtype mock_place (S.CType ({place=mock_place; value=S.TFlatType S.TStr}))) in

        let aux (label, (st: S.session_type), _) = 
            let events = stype_to_events ~k:(k+1) st in
            {
                T.vis=T.Public; 
                T.name= event_name_of_labels labels; 
                T.kind=T.Event; T.args=[(ct, Atom.fresh_builtin "value")]
            }::events
        in
        List.flatten (List.map aux xs)
    | S.STVar _ -> [] (*No need to signal the start of a new round*)
    | S.STRec (_,st) ->
        stype_to_events ~k:(k+1) st
    | S.STInline x -> 
        raise (Error.DeadbranchError "STInline should remains outside the codegen part, it should have been resolve during the partial evaluation pass.")

and finish_component_type place : S._component_type -> T.ctype = function
| S.CompTUid x -> T.TVar x 
and fcctype : S.component_type -> T.ctype = function t -> finish_component_type t.place t.value

and finish_mtype place : S._main_type -> T.ctype * T.event list = function
| S.CType ct -> fctype ct, [] 
| S.SType st -> encode_stype st, stype_to_events st
| S.CompType ct -> fcctype ct, []
| _ -> Core.Error.error place "Akka: Type translation is only supported for composed types and session types"
and fmtype : S.main_type ->  T.ctype * T.event list  = function mt -> finish_mtype mt.place mt.value

(************************************ Literals *****************************)

and finish_literal place : S._literal -> T.literal = function
    | S.EmptyLit -> T.EmptyLit
    | S.BoolLit b -> T.BoolLit b
    | S.FloatLit f -> T.FloatLit f
    | S.IntLit i -> T.IntLit i
    | S.LabelLit l -> Core.Error.error place "Label are not yet supported"
    | S.StringLit str -> T.StringLit str

    | S.ActivationInfo _ -> failwith "Activation info is not yet supported"

    | S.Place _ -> failwith "Place is not yet supported"
    | S.VPlace _ -> failwith "VPlace is not yet supported"

    | S.Bridge b -> T.StringLit (Atom.atom_to_str b.id)  (*TODO should be class *)
and fliteral : S.literal -> T.literal = function lit -> finish_literal lit.place lit.value

(************************************ Expr & Stmt *****************************)

and finish_expr place : S._expr -> T.expr = function
    | S.VarExpr x -> T.VarExpr x
    | S.AccessExpr (e1, e2) -> T.AccessExpr (fexpr e1, fexpr e2)
    | S.BinopExpr (t1, op, t2) -> T.BinopExpr (fexpr t1, op, fexpr t2)
    | S.LambdaExpr (x, stmt) -> T.LambdaExpr ([x], fstmt stmt) 
    | S.LitExpr lit -> T.LitExpr (fliteral lit)
    | S.UnopExpr (op, e) -> T.UnopExpr (op, fexpr e)

    | S.CallExpr (e1, es) -> begin 
        match e1.value with 
        | S.VarExpr x when Atom.is_builtin x -> begin
            (* TODO put this in separate fct *)
            (* TODO Remove string and used typed constructor *)
            match (Atom.value x) with 
            | "fire" -> begin 
                match es with
                | [ session; msg ] -> T.CallExpr(
                    T.AccessExpr (fexpr session, T.VarExpr x),
                    [ fexpr msg ]
                ) 
                | _ -> Error.error place "fire must take two arguments : place(session, message)"
                end
            | _ -> 
                logger#warning "Akka.Finish do not yet support builtin function %s" (Atom.value x);
                T.CallExpr (fexpr e1, List.map fexpr es)
        end
        | _ -> T.CallExpr (fexpr e1, List.map fexpr es)
    end
    | S.This -> T.This
    | S.Spawn {c; args; at=None} ->
        T.Spawn {context=T.CurrentContext;  actor_expr=
        T.CallExpr(
            T.VarExpr (Atom.fresh_builtin "spawn"),
            [
                T.CallExpr (
                    T.AccessExpr (
                        fcexpr c,
                        T.VarExpr (Atom.fresh_builtin "create")
                    ),
                    []
                )
            ] @ (List.map fexpr args)
        )}
    | S.Spawn {c; args; at=at} ->
        failwith "finish_expr spawn with place annotation not yet supported" 

    | S.BoxCExpr _ -> failwith "finish_expr BoxCexpr is not yet supported"
    
    | S.OptionExpr e_opt -> failwith "option not yet supported" 
    | S.ResultExpr (None, Some err) ->  T.CallExpr (
        T.VarExpr (Atom.fresh_builtin "err"),
        [fexpr err]
    ) 
    | S.ResultExpr (Some ok, None) -> T.CallExpr (
        T.VarExpr (Atom.fresh_builtin "ok"),
        [fexpr ok]
    ) 
    | S.ResultExpr (_,_) -> raise (Core.Error.DeadbranchError "finish_expr : a result expr can not be Ok and Err at the same time.")
    | S.BlockExpr (b, es) -> failwith "block not yet supported"
    | S.Block2Expr (b, xs) -> failwith "block not yet supported"
and fexpr : S.expr -> T.expr = function e -> finish_expr e.place e.value

and finish_stmt place : S._stmt -> T.stmt = function
    | S.EmptyStmt -> T.CommentsStmt (IR.LineComment "Empty Statement")

    (*S.* Binders *)
    | S.AssignExpr (x, e) -> T.AssignExpr (T.VarExpr x, fexpr e)
    | S.AssignThisExpr (x, e) -> T.AssignExpr (
                                    T.AccessExpr (T.This, T.VarExpr x),
                                   fexpr e)        
    | S.LetExpr (mt, x, e) ->  T.LetStmt (fst (fmtype mt), x, Some (fexpr e))                             

    (*S.* Comments *)
    | S.CommentsStmt comments -> T.CommentsStmt comments.value

    (*S.* Control flow *)
    | S.BreakStmt -> T.BreakStmt
    | S.ContinueStmt -> T.ContinueStmt
    | S.ExitStmt _ -> failwith "Exist is not yet supported"
    | S.ForStmt (_,_,_) -> failwith "For is not yet supported" 
    | S.IfStmt (e, s1, s2_opt) -> T.IfStmt (fexpr e, fstmt s1, Option.map fstmt s2_opt)
    | S.MatchStmt (_,_) -> Core.Error.error place "Match is not yet supported"
    | S.ReturnStmt e -> T.ReturnStmt (fexpr e) 

    (*S.*type name, type definition*)
    | S.ExpressionStmt e -> T.ExpressionStmt (fexpr e) 
    | S.BlockStmt stmts -> T.BlockStmt (List.map fstmt stmts)
    
    | S.GhostStmt _ -> raise (Core.Error.DeadbranchError "finish_stype : GhostStmt should have been remove by a previous compilation pass.")
and fstmt : S.stmt -> T.stmt = function stmt -> finish_stmt stmt.place stmt.value

(************************************ Component *****************************)
(* return type is T._expr for now, since we built only one state with all the variable inside FIXME *)
and finish_state place : S._state -> T.stmt = function 
    | S.StateDcl {ghost; kind; type0; name; body = S.InitExpr e} -> 
        T.LetStmt (fst (fmtype type0), name, Some (fexpr e))
    | S.StateDcl {ghost; kind; type0; name; body = S.InitBB bb_term} -> 
            failwith "InitBB not supported yet"
    (*use global x as y;*)
    | S.StateAlias {ghost; kind; type0; name} -> failwith "finish_state StateAlias is not yet supported" 
and fstate : S.state -> T.stmt = function state -> finish_state state.place state.value 


and finish_param place : S._param -> (T.ctype * T.variable) = function
| mt, x -> fst(fmtype mt), x
and fparam : S.param -> (T.ctype * T.variable) = function p -> finish_param p.place p.value


and finish_contract place (method0 : T.method0) (contract : S._contract) : T.method0 list =
    (* Inner logic of the method *)
    let inner_name = Atom.fresh ((Atom.hint contract.method_name)^"_inner") in
    let inner_method = { method0 with name = inner_name; vis = T.Private } in

    (* Pre binders *)
    let with_params = List.map (function (x,y,_) -> finish_param place (x,y)) contract.pre_binders in
    let with_stmts : S._stmt list = List.map (function (x,y,z) -> S.LetExpr (x,y,z)) contract.pre_binders in 
    let with_stmts : T.stmt list = List.map (finish_stmt place) with_stmts in
    (*let with_body : T.stmt = T.BlockStmt with_stmts in*)

    (* Pre-condition *)
    let ensures_methods, ensures_stmts = match contract.ensures with
    | None -> [], [] 
    | Some ensures_expr -> begin
        let ensures_name    = Atom.fresh ((Atom.hint contract.method_name)^"_ensures") in
        let ensures_params  = method0.args @ with_params in

        let ensures_method : T.method0 = {
            vis             = T.Private;
            ret_type        = T.Atomic "boolean";
            name            = ensures_name;
            body            = T.ExpressionStmt (fexpr ensures_expr);
            args            = ensures_params;
            is_constructor  = false
        } in

        [ensures_method], [ T.IfStmt (
            T.UnopExpr ( 
                IR.Not,
                T.CallExpr ( 
                    T.VarExpr ensures_name, 
                    List.map (function param -> T.VarExpr (snd param)) ensures_params 
                )
            ),
            T.ExpressionStmt (T.AssertExpr (T.LitExpr (T.BoolLit false))), (*TODO refine*)
            None
        ) ]
    end in 

    (* Post-condition *)
    let returns_methods, returns_stmts = match contract.returns with
    | None -> [], [
        T.ReturnStmt ( T.CallExpr (
            T.VarExpr inner_name,
            List.map (function param -> T.VarExpr (snd param)) method0.args
        ))
    ] 
    | Some returns_expr -> begin
        let returns_name    = Atom.fresh ((Atom.hint contract.method_name)^"_returns") in
        let ret_type_param  = (method0.ret_type, Atom.fresh "res") in 
        let returns_params  = ret_type_param :: method0.args @ with_params in

        let returns_method  : T.method0 = {
            vis             = T.Private;
            ret_type        = T.Atomic "boolean";
            name            = returns_name;
            body            = T.ExpressionStmt (T.CallExpr(
                fexpr returns_expr,
                [T.VarExpr (snd ret_type_param)]
            ));
            args            = returns_params;
            is_constructor  = false
        } in

        [returns_method], [ 
            T.LetStmt (
                method0.ret_type,
                (snd ret_type_param),
                Some (T.CallExpr (
                    T.VarExpr inner_name,
                    List.map (function param -> T.VarExpr (snd param)) method0.args
                ))
            );
            T.IfStmt (
                T.UnopExpr ( 
                    IR.Not,
                    T.CallExpr ( 
                        T.VarExpr returns_name, 
                        List.map (function param -> T.VarExpr (snd param)) returns_params 
                    )
                ),
                T.ExpressionStmt (T.AssertExpr (T.LitExpr (T.BoolLit false))), (*TODO refine*)
                None
            );
            T.ReturnStmt (T.VarExpr (snd ret_type_param))
        ]
    end in

    let main_stmts =
        with_stmts @
        ensures_stmts @
        returns_stmts
    in

    let main_method : T.method0 = {
        vis             = method0.vis;
        ret_type        = method0.ret_type;
        name            = method0.name;
        body            = T.BlockStmt main_stmts;
        args            = method0.args;
        is_constructor  = method0.is_constructor 
    } in 

    let methods =
        [inner_method] @
        ensures_methods @
        returns_methods @
        [main_method]
    in

    methods
and fcontract actor_name (method0 : T.method0) : S.contract -> T.method0 list = function m -> finish_contract m.place method0 m.value
    
and finish_method place actor_name ?is_constructor:(is_constructor=false) : S._method0 -> T.method0 list = function
    | S.CustomMethod m0 ->  
        let body = match m0.body with
            | AbstractImpl stmt -> fstmt stmt (* implem de reference -> generate code*)
            | BBImpl _ -> failwith "BBImpl not supported yet"
        in

        let new_method : T.method0 = { 
            vis             = T.Public; 
            ret_type        = fst (fmtype m0.ret_type);
            name            = if is_constructor then actor_name else m0.name;
            args            = (List.map fparam m0.args);
            body            = body; 
            is_constructor  = is_constructor 
        } in

        begin
            match m0.contract_opt with
            | None -> [new_method]
            | Some contract -> fcontract actor_name new_method contract 
        end

    | S.OnStartup m0 -> finish_method m0.place actor_name ~is_constructor:true m0.value   
    | S.OnDestroy _ -> failwith "onstart and ondestroy are not yet supported"
and fmethod actor_name : S.method0 -> T.method0 list = function m -> finish_method m.place actor_name m.value


and finish_component_dcl place : S._component_dcl -> T.actor list = function
| S.ComponentStructure {name; args; body} -> begin 
    print_string (Atom.hint name);
    print_newline ();
    assert( args == []); (* Not supported yet, maybe one day *)

    (* Group by kind the elmts : method, state, ...
        in order to display well stuctured code at the end (and since the order of definition do not matter)
    *)
    let grp_items = group_cdcl_by body in
    assert(grp_items.others == []);

    
    (* Building events *)
    (* Events that should be defined inside the Actor *)
    let is_stype = function
        | _, Some ({Core.AstUtils.place; Core.AstUtils.value = S.SType st}) -> false
        | _ -> true
    in
    let events = List.flatten (List.map (function x -> snd (fmtype x)) (List.filter_map (function x -> match snd x with |S.AbstractTypedef mt -> Some mt | _ -> None) grp_items.typedefs)) in
    

    (* Building states *)
    let states : T.state list = List.map (
        function state -> {T.persistent=false; stmts= [fstate state] } (* TODO handle persistency*)
    ) grp_items.states in 

    (* Building receiver *)
    let receiver_expr = T.CallExpr(T.VarExpr (Atom.fresh "newReceiveBuilder"), [T.LitExpr T.EmptyLit]) in

    let aux_receiver (expr: T.expr) (p: S.port) = 
        (* TODO also ensure this propertie about expecting after the partial evaluation pass by doing a 
        checking pass ??
        *)
        let expecting_msg_types = match p.value.expecting_st.value with 
        | S.SType st -> begin
            match st.value with
            | S.STRecv (msg_type,_) -> fst(fmtype msg_type)
            | S.STBranch xs -> 
                let labels = List.fold_left (fun acc (label, _,_) -> label::acc) [] xs in

                T.TVar (event_name_of_labels labels)
            | S.STEnd | S.STVar _ |S.STRecv _-> failwith "TOTO"
            | S.STSend _-> failwith "TATA"
            | S.STInline _ -> failwith "TITI"
            | _ -> Core.Error.error p.value.expecting_st.place "%s plugin: expecting type can only start by the reception of a message or of a label" plg_name  
        end 
        | _ -> Core.Error.error p.value.expecting_st.place "%s plugin do not support main type for port expecting" plg_name  
        in

        T.AccessExpr(
            expr, 
            T.CallExpr(T.VarExpr (Atom.fresh_builtin "onMessage"), [
                T.ClassOf expecting_msg_types;
                fexpr p.value.callback
            ]))
    in

    let receiver_expr = List.fold_left aux_receiver receiver_expr grp_items.ports in

    let receiver_expr = T.AccessExpr(
        receiver_expr, 
        T.CallExpr(T.VarExpr (Atom.fresh_builtin "build"), [T.LitExpr T.EmptyLit]))
    in

    let receiver : T.method0 = {
        args            = [];
        body            = T.ReturnStmt receiver_expr;
        name            = Atom.fresh_builtin "createReceive";
        ret_type        = T.Behavior "Command";
        vis             = T.Public;
        is_constructor  = false
    } in
    
    (* building methods *)
    let methods = List.flatten (List.map (fmethod name) grp_items.methods) in 
    

    (* Sumup *)
    [{   
        T.name;
        T.methods; 
        T.states;
        T.events;
        T.nested_items= (List.map (function x -> T.Actor x)) (List.flatten (List.map fcdcl grp_items.nested)); 
        T.receiver = Some receiver
    }]
end 
| S.ComponentAssign _ -> failwith "Component expr are not yet supported" 

and fcdcl  : S.component_dcl -> T.actor list = function cdcl -> finish_component_dcl cdcl.place cdcl.value 

(********************** Manipulating component structure *********************)
and finish_component_expr place : S._component_expr -> T.expr = function
    | S.VarCExpr x -> T.VarExpr x
    | _ -> failwith "Akka plg do not uspprt yet advance compoent expr" 
and fcexpr  : S.component_expr -> T.expr = function cexpr -> finish_component_expr cexpr.place cexpr.value 

(************************************ Program *****************************)

and finish_term place : S._term -> T.term list = function
| S.Comments c -> [T.Comments c.value]
| S.Component cdcl -> List.map (function a -> T.Actor a) (fcdcl cdcl)
| S.Stmt stmt -> [T.Stmt (fstmt stmt)]
| S.Typedef (v, S.AbstractTypedef body) -> (* TODO move this to the translation from akka to java, and add Typedef into akka ast *) 
    match body.value with
    | S.CType {value=S.TVar _; _} -> Error.error place "Type aliasing is not (natively) supported in Java"
    | CompType {value=CompTUid _; _} ->  Error.error place "Type aliasing is not (natively) supported in Java" 
    | _ -> 
        let mt, events = fmtype body in
        general_fenv := List.fold_left (fun env (event:T.event) -> bind_event env event.name event) !general_fenv events;
        (* FIXME | TODO do something with the mt *) 
        [T.Class v] @ (List.map (fun x -> T.Event x) events) 

and fterm : S.term -> T.term list = function t -> finish_term t.place t.value

let finish_program terms = 
    { 
        T.entrypoint = [];
        T.system = {
            name=Atom.fresh "AkkaSystem";
            methods= []; 
            states= []; 
            events=[]; 
            receiver = None;
            nested_items=[]};
        T.terms= List.flatten (List.map fterm terms)   
    }