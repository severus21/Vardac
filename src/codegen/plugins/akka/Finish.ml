open Core
open Utils
open AstUtils
open Easy_logging
open Fieldslib

(* Function composition, TODO put it in some Core file*)
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
    eventdefs: S.typedef list;
    states: S.state list; 
    nested: S.component_dcl list; 
    ports: S.port list;
    others: S.term list
}

let fresh_items_grp () = { 
    methods     = [];
    eventdefs    = [];
    states      = [];
    nested      = [];
    ports       = [];
    others      = [];
}

let group_cdcl_by (citems:  S.component_item list) : items_grps =
    let dispatch grp (citem: S.component_item) = match citem.value with
        | S.Contract _ -> raise (Core.Error.PlacedDeadbranchError  (citem.place, "Contract term should have been remove from AST by the cook pass and binded to a method"))
        | S.Include _ -> Core.Error.error citem.place "Include is not yet supported in Akka plg"
        | S.Method  m-> {grp with methods=m::grp.methods}
        | S.State f-> {grp with states=f::grp.states}
        | S.Port p -> {grp with ports=p::grp.ports}
        (* Shallow search of Typealias, FIXME do we need deep search ?*)
        | S.Term {place; value=S.Component cdcl} -> {grp with nested=cdcl::grp.nested}
        | S.Term {place; value=S.Typedef ({value=EventDef _;_} as edef)} -> {grp with eventdefs=edef::grp.eventdefs}
        | S.Term t -> {grp with others=t::grp.others}
    in

    let grp = (List.fold_left  dispatch (fresh_items_grp ()) citems) in 
        {
            methods=(List.rev grp.methods);
            eventdefs=(List.rev grp.eventdefs);
            states=(List.rev grp.states) ; 
            nested=(List.rev grp.nested) ; 
            ports=(List.rev grp.ports) ; 
            others=(List.rev grp.others)
        }

(************************************* Base types ****************************)

let rec finish_place finish_value ({ AstUtils.place ; AstUtils.value}: 'a AstUtils.placed) = 
    let value = finish_value place value in
    {AstUtils.place; AstUtils.value}

(************************************ Types **********************************)
let rec finish_ctype place : S._composed_type ->  T._ctype = function
    | S.TActivationInfo mt -> T.ActorRef (fmtype mt) 
    | S.TArrow (m1, m2) -> T.TFunction (
        fmtype m1,
        fmtype m2
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
    | S.TDict (m1, m2) -> T.TMap (fmtype m1, fmtype m2)
    | S.TList mt -> T.TList (fmtype mt)
    | S.TOption mt -> T.TOption (fmtype mt)
    | S.TResult (m1, m2) -> T.TResult (fmtype m1, fmtype m2)
    | S.TSet mt -> T.TSet (fmtype mt)
    | S.TTuple mts ->  T.TTuple (List.map (fun x -> (fmtype x)) mts)
    | S.TBridge b -> T.Atomic "lg4dc.protocol.Bridge" (*TODO*)
    | S.TRaw bbraw -> T.TRaw bbraw.value.body
and fctype ct :  T.ctype = finish_place finish_ctype ct

(* Represent an ST object in Java type *)
and finish_stype place : S._session_type -> T._ctype = function 
    | S.STEnd -> T.TVar (Atom.fresh_builtin "lg4dc.protocol.STEnd") 
    | (S.STSend (mt, st) as st0) | (S.STRecv (mt, st) as st0) -> begin 
        match fmtype mt with
        | ct ->
            T.TParam (
                {
                    place;
                    value =  T.TVar (Atom.fresh_builtin (match st0 with | S.STSend _ -> "lg4dc.protocol.STSend" | STRecv _ -> "lg4dc.protocol.STSend"))
                },
                [ct; fstype st]
            )
        | _ -> raise (Core.Error.PlacedDeadbranchError (mt.place, "finish_stype : STSend/STRecv type should not be a session type."))
    end
    | (S.STBranch xs as st0) | (S.STSelect xs as st0) ->
        let rec built_t_hlist = function
            | [] -> {place; value = T.TVar (Atom.fresh_builtin "lg4dc.protocol.HNil")}
            | st::sts -> { place; value = T.TParam( 
                { 
                    place = st.place; 
                    value = T.TVar (Atom.fresh_builtin "lg4dc.protocol.HCons")
                }, [
                    {place = place; value = T.TParam( 
                        { 
                            place = st.place;
                            value = T.TVar (Atom.fresh_builtin "lg4dc.protocol.STEntry")
                        },
                        [fstype st]
                    )}
                ] @ [(built_t_hlist sts)] 
            )}
        in

        let continuation_st = built_t_hlist (List.map (fun (_, st, _) -> st) xs) in

        T.TParam (
            { 
                place;
                value = T.TVar (Atom.fresh_builtin (match st0 with | S.STBranch _ -> "lg4dc.protocol.STBranch" | S.STSelect _ -> "lg4dc.protocol.STSelect"))
            },
            [continuation_st]
        )
    | S.STVar _ -> T.TVar (Atom.fresh_builtin ("lg4dc.protocol.STVar"))
    | S.STRec (_,st) ->
        T.TParam (
            { 
                place;
                value = T.TVar (Atom.fresh_builtin "lg4dc.protocol.STRec")
            },
            [ fstype st]
        )

    | S.STInline x -> 
        raise (Error.PlacedDeadbranchError (place, "STInline should remains outside the codegen part, it should have been resolve during the partial evaluation pass."))
and fstype st : T.ctype = finish_place finish_stype st

and finish_component_type place : S._component_type -> T._ctype = function
| S.CompTUid x -> T.TVar x 
and fcctype ct : T.ctype = finish_place finish_component_type ct

and finish_mtype place : S._main_type -> T.ctype = function
| S.CType ct -> fctype ct 
| S.SType st -> fstype st
| S.CompType ct -> fcctype ct
| _ -> Core.Error.error place "Akka: Type translation is only supported for composed types and session types"
and fmtype : S.main_type ->  T.ctype = function mt -> finish_mtype mt.place mt.value

(************************************ Literals *****************************)

and finish_literal place : S._literal -> T._literal = function
    | S.EmptyLit -> T.EmptyLit
    | S.VoidLit -> T.VoidLit
    | S.BoolLit b -> T.BoolLit b
    | S.FloatLit f -> T.FloatLit f
    | S.IntLit i -> T.IntLit i
    | S.LabelLit l -> Core.Error.error place "Label are not yet supported"
    | S.StringLit str -> T.StringLit str

    | S.ActivationInfo _ -> failwith "Activation info is not yet supported"

    | S.Place _ -> failwith "Place is not yet supported"
    | S.VPlace _ -> failwith "VPlace is not yet supported"

    | S.Bridge b -> T.StringLit (Atom.to_string b.id)  (*TODO should be class *)
and fliteral lit : T.literal = finish_place finish_literal lit

(************************************ Expr & Stmt *****************************)

and finish_expr place : S._expr -> T._expr = function
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
                    {
                        place = e1.place;
                        value = T.AccessExpr (fexpr session, {place = e1.place; value = T.VarExpr x})
                    },
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
        T.Spawn {
            context = {place; value = T.CurrentContext};  
            actor_expr= {place; value = T.CallExpr(
                { 
                    place;
                    value = T.VarExpr (Atom.fresh_builtin "spawn")
                },
                [{
                    place;
                    value = T.CallExpr ({
                        place;
                        value = T.AccessExpr (
                            fcexpr c,
                            { 
                                place;
                                value = T.VarExpr (Atom.fresh_builtin "create")
                            }
                        )},
                        []
                    )
                }] @ (List.map fexpr args)
            )}
        }
    | S.Spawn {c; args; at=at} ->
        failwith "finish_expr spawn with place annotation not yet supported" 

    | S.BoxCExpr _ -> failwith "finish_expr BoxCexpr is not yet supported"
    
    | S.OptionExpr e_opt -> failwith "option not yet supported" 
    | S.ResultExpr (None, Some err) ->  T.CallExpr (
        { place; value = T.VarExpr (Atom.fresh_builtin "err")},
        [fexpr err]
    ) 
    | S.ResultExpr (Some ok, None) -> T.CallExpr (
        { place; value = T.VarExpr (Atom.fresh_builtin "ok")},
        [fexpr ok]
    ) 
    | S.ResultExpr (_,_) -> raise (Core.Error.PlacedDeadbranchError (place, "finish_expr : a result expr can not be Ok and Err at the same time."))
    | S.BlockExpr (b, es) -> failwith "block not yet supported"
    | S.Block2Expr (b, xs) -> failwith "block not yet supported"
and fexpr e : T.expr = finish_place finish_expr e

and finish_stmt place : S._stmt -> T._stmt = function
    | S.EmptyStmt -> T.CommentsStmt (IR.LineComment "Empty Statement")

    (*S.* Binders *)
    | S.AssignExpr (x, e) -> T.AssignExpr ({place; value=T.VarExpr x}, fexpr e)
    | S.AssignThisExpr (x, e) -> 
        T.AssignExpr ( 
            {place; value= T.AccessExpr (
                {place; value=T.This},
                {place; value= T.VarExpr x})
            },
            fexpr e)        
    | S.LetExpr (mt, x, e) ->  T.LetStmt (fmtype mt, x, Some (fexpr e))                             

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
    
    | S.GhostStmt _ -> raise (Core.Error.PlacedDeadbranchError (place, "finish_stype : GhostStmt should have been remove by a previous compilation pass."))
and fstmt stmt : T.stmt = finish_place finish_stmt stmt

(************************************ Component *****************************)
(* return type is T._expr for now, since we built only one state with all the variable inside FIXME *)
and finish_state place : S._state -> T._stmt = function 
    | S.StateDcl {ghost; kind; type0; name; body = S.InitExpr e} -> 
        T.LetStmt (fmtype type0, name, Some (fexpr e))
    | S.StateDcl {ghost; kind; type0; name; body = S.InitBB bb_term} -> 
        let re = 
            if bb_term.value.template then 
                Error.error bb_term.place "template is not used for state"
            else
                bb_term.value.body
        in
        T.LetStmt (fmtype type0, name, Some ({place=bb_term.place; value = T.RawExpr re}))
    (*use global x as y;*)
    | S.StateAlias {ghost; kind; type0; name} -> failwith "finish_state StateAlias is not yet supported" 
and fstate s : T.stmt = finish_place finish_state s


and finish_param place : S._param -> (T.ctype * T.variable) = function
| mt, x -> fmtype mt, x
and fparam : S.param -> (T.ctype * T.variable) = function p -> finish_param p.place p.value


and finish_contract place (method0 : T.method0) (contract : S._contract) : T.method0 list =
    (* Inner logic of the method *)
    let inner_name = Atom.fresh ((Atom.hint contract.method_name)^"_inner") in
    let inner_method = {
        place = method0.place;
        value = { method0.value with 
            name = inner_name; 
            annotations = [T.Visibility T.Private] 
        }
    } in

    (* Pre binders *)
    let with_params = List.map (function (x,y,_) -> finish_param place (x,y)) contract.pre_binders in
    let with_stmts : S.stmt list = List.map (function (x,y,z) -> {place; value=S.LetExpr (x,y,z)}) contract.pre_binders in 
    let with_stmts : T.stmt list = List.map fstmt with_stmts in
    (*let with_body : T.stmt = T.BlockStmt with_stmts in*)

    (* Pre-condition *)
    let ensures_methods, ensures_stmts = match contract.ensures with
    | None -> [], [] 
    | Some ensures_expr -> begin
        let ensures_name    = Atom.fresh ((Atom.hint contract.method_name)^"_ensures") in
        let ensures_params  = method0.value.args @ with_params in

        let ensures_method : T.method0 = {
            place = ensures_expr.place;
            value = {
                annotations     = [ T.Visibility T.Private ];
                ret_type        = { place; value=T.Atomic "boolean"};
                name            = ensures_name;
                body            =  T.AbstractImpl ([{ 
                    place= ensures_expr.place;
                    value = T.ReturnStmt (fexpr ensures_expr)
                }]);
                args            = ensures_params;
                is_constructor  = false
            }
        } in

        [ensures_method], [ 
            { place = ensures_expr.place; value= T.IfStmt (
                {place = ensures_expr.place; value = T.UnopExpr ( 
                    IR.Not,
                    {place = ensures_expr.place; value = T.CallExpr ( 
                        {place = ensures_expr.place; value = T.VarExpr ensures_name}, 
                        List.map (function param -> {place = ensures_expr.place; value =T.VarExpr (snd param)}) ensures_params 
                    )}
                )},
                {place = ensures_expr.place; value = T.ExpressionStmt (
                    {place = ensures_expr.place; value = T.AssertExpr (
                        {place = ensures_expr.place; value = T.LitExpr (
                            {place = ensures_expr.place; value = T.BoolLit false})}
                    )}
                )}, (*TODO refine*)
                None
            )}
        ]
    end in 

    (* Post-condition *)
    let returns_methods, returns_stmts = match contract.returns with
    | None -> [], [
        {place; value = T.ReturnStmt ( 
            {place; value = T.CallExpr (
                {place; value=T.VarExpr inner_name},
                List.map (function param -> {place; value=T.VarExpr (snd param)}) method0.value.args
            )})
        }
    ] 
    | Some returns_expr -> begin
        let returns_name    = Atom.fresh ((Atom.hint contract.method_name)^"_returns") in
        let ret_type_param  = (method0.value.ret_type, Atom.fresh "res") in 
        let returns_params  = ret_type_param :: method0.value.args @ with_params in

        let returns_method  : T.method0 = {
            place = returns_expr.place;
            value = {
                annotations     = [ T.Visibility T.Private ];
                ret_type        = { place = returns_expr.place; value=T.Atomic "boolean"};
                name            = returns_name;
                body            = T.AbstractImpl ([
                    {place = returns_expr.place; value= T.ReturnStmt (
                        {place = returns_expr.place; value=T.CallExpr(
                            fexpr returns_expr,
                            [{place = returns_expr.place; value=T.VarExpr (snd ret_type_param)}]
                            )
                        })
                    }
                ]);
                args            = returns_params;
                is_constructor  = false
            }
        } in

        [returns_method], [ 
            {place; value= T.LetStmt (
                method0.value.ret_type,
                (snd ret_type_param),
                Some ({place; value=T.CallExpr (
                    {place; value=T.VarExpr inner_name},
                    List.map (function param -> {place; value=T.VarExpr (snd param)}) method0.value.args
                )})
            )};
            {place; value=T.IfStmt (
                {place; value=T.UnopExpr ( 
                    IR.Not,
                    {place; value=T.CallExpr ( 
                        {place; value=T.VarExpr returns_name}, 
                        List.map (function param -> {place; value=T.VarExpr (snd param)}) returns_params 
                    )}
                )},
                {place; value=T.ExpressionStmt (
                    {place; value=T.AssertExpr (
                        {place; value=T.LitExpr (
                            {place; value=T.BoolLit false}
                        )}
                    )}
                )}, (*TODO refine*)
                None
            )};
            {place; value=T.ReturnStmt (
                {place; value=T.VarExpr (snd ret_type_param)}
            )}
        ]
    end in

    let main_stmts =
        with_stmts @
        ensures_stmts @
        returns_stmts
    in

    let main_method : T.method0 = {
        place = method0.place@place;
        value = {
            annotations     = method0.value.annotations;
            ret_type        = method0.value.ret_type;
            name            = method0.value.name;
            body            = T.AbstractImpl main_stmts;
            args            = method0.value.args;
            is_constructor  = method0.value.is_constructor 
        }
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
            | S.AbstractImpl stmts -> T.AbstractImpl (List.map fstmt stmts)
            | S.BBImpl body -> T.BBImpl body
        in

        let new_method : T.method0 = {
            place;
            value = { 
                annotations     = [ T.Visibility T.Public ]; 
                ret_type        = fmtype m0.ret_type;
                name            = if is_constructor then actor_name else m0.name;
                args            = (List.map fparam m0.args);
                body            = body; 
                is_constructor  = is_constructor 
            }
        } in

        begin
            match m0.contract_opt with
            | None -> [new_method]
            | Some contract -> fcontract actor_name new_method contract 
        end

    | S.OnStartup m0 -> finish_method m0.place actor_name ~is_constructor:true m0.value   
    | S.OnDestroy _ -> failwith "ondestroy is not yet supported"
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
    let events = List.map ( function
        | {value=S.EventDef (name, mts, body); place} ->
            { 
                place = place; 
                value = {
                    T.vis=T.Public; 
                    T.name= name;
                    T.kind=T.Event; 
                    T.args=List.mapi ( fun i mt ->
                        fmtype mt, Atom.fresh_builtin ("value"^(string_of_int i))
                    ) mts
                }
            }
    ) grp_items.eventdefs in
       (* 
        List.flatten (List.map (function x -> snd (fmtype x)) (List.filter_map (function x -> match snd x with |S.AbstractTypealias mt -> Some mt | _ -> None) (List.map (function |{value=S.EventDef (x, mts,body); _} -> (x, mts, body)) grp_items.eventdefs))) in*)
    

    (* Building states *)
    let states : T.state list = List.map (
        function state -> {
            place = state.place; 
            value = {T.persistent=false; stmts= [fstate state] }
        } (* TODO handle persistency*)
    ) grp_items.states in 

    (* Building receiver *)
    let receiver_expr = {place; value=T.CallExpr(
        {place; value=T.VarExpr (
            Atom.fresh "newReceiveBuilder"
        )}, 
        [{place; value=T.LitExpr {place; value=T.EmptyLit}}]
    )} in

    let aux_receiver (expr: T.expr) (p: S.port) = 
        (* TODO also ensure this propertie about expecting after the partial evaluation pass by doing a 
        checking pass ??
        *)
        let expecting_msg_types = match p.value.expecting_st.value with 
        | S.SType st -> begin
            match st.value with
            | S.STRecv (msg_type,_) -> fmtype msg_type
            | S.STBranch xs -> 
                let labels = List.fold_left (fun acc (label, _,_) -> label::acc) [] xs in
                failwith "TRUC"
                (*{place=st.place; value=T.TVar (event_name_of_labels labels)}*)
            | S.STEnd | S.STVar _ |S.STRecv _-> failwith "TOTO"
            | S.STSend _-> failwith "TATA"
            | S.STInline _ -> failwith "TITI"
            | _ -> Core.Error.error p.value.expecting_st.place "%s plugin: expecting type can only start by the reception of a message or of a label" plg_name  
        end 
        | _ -> Core.Error.error p.value.expecting_st.place "%s plugin do not support main type for port expecting" plg_name  
        in
        
        {place; value=T.AccessExpr(
            expr, 
            {place; value=T.CallExpr(
                {place; value=T.VarExpr (Atom.fresh_builtin "onMessage")}, 
                [
                    {place; value=T.ClassOf expecting_msg_types};
                    fexpr p.value.callback
                ]
            )}
        )}
    in

    let receiver_expr = List.fold_left aux_receiver receiver_expr grp_items.ports in

    let receiver_expr = {place; value=T.AccessExpr(
        receiver_expr, 
        { place; value=T.CallExpr(
            {place; value=T.VarExpr (Atom.fresh_builtin "build")},
            [{place; value=T.LitExpr {place; value=T.EmptyLit}}])
        })}
    in

    let receiver : T.method0 = { 
        place;
        value = {
            args            = [];
            body            = T.AbstractImpl ([
                {place; value=T.ReturnStmt receiver_expr}
            ]);
            name            = Atom.fresh_builtin "createReceive";
            ret_type        = {place; value=T.Behavior "Command"};
            annotations     = [ T.Visibility T.Public ];
            is_constructor  = false
        }
    } in
    
    (* building methods *)
    let methods = List.flatten (List.map (fmethod name) grp_items.methods) in 
    

    (* Sumup *)
    [{
        place;
        value = {   
            T.name;
            T.methods; 
            T.states;
            T.events;
            T.nested_items= (List.map (function x -> { place = x.place; value=T.Actor x})) (List.flatten (List.map fcdcl grp_items.nested)); 
            T.receiver = Some receiver
        }
    }]
end 
| S.ComponentAssign _ -> failwith "Component expr are not yet supported" 

and fcdcl  : S.component_dcl -> T.actor list = function cdcl -> finish_component_dcl cdcl.place cdcl.value 

(********************** Manipulating component structure *********************)
and finish_component_expr place : S._component_expr -> T._expr = function
    | S.VarCExpr x -> T.VarExpr x
    | _ -> failwith "Akka plg do not support yet advance component expr" 
and fcexpr ce : T.expr = finish_place finish_component_expr ce

(************************************ Program *****************************)

and finish_term place : S._term -> T.term list = function
| S.Comments c -> [{
    place;
    value=T.Comments c.value
}]
| S.Component cdcl -> List.map (function a -> {
    place=a.place; 
    value=T.Actor a
}) (fcdcl cdcl)
| S.Stmt stmt -> [{
    place; 
    value = T.Stmt (fstmt stmt)
}]
| S.Typedef {value=S.ProtocolDef (name, {value=S.SType st; _});_} -> 
    let fplace = (Error.forge_place "Plg=Akka/finish_term/protocoldef" 0 0) in
    let auto_place smth = {place = fplace; value=smth} in
    (* TODO generalize the usage of auto place *)

    (* case protocol definition *)
    let rec extract_events place k = function
    | S.STEnd | S.STVar _ -> []
    | S.STInline _ -> raise (Error.PlacedDeadbranchError (place, "STInline should have been removed by the partial evaluation pass"))
    | S.STSend ({value=S.CType {value=S.TVar name;_};_}, st_next) | S.STRecv ({value=S.CType{value=S.TVar name;_};_}, st_next) -> 
        { 
            place = place; 
            (*
    goal public static final class Pong implements Event {}
            *)
            value = {
                T.vis=T.Public; 
                T.name= name;
                T.kind=T.Event; 
                T.args= []
            }
        }:: (extract_events st_next.place (k+1) st_next.value)
    | (S.STSend _ as t)| (STRecv _ as t)-> failwith "toto"
    | S.STBranch entries | STSelect entries -> begin
        let aux_entry (label, st, _) = 
            { 
                place; 
                value = {
                    T.vis = T.Public; 
                    T.name = label;
                    T.kind = T.Event; 
                    T.args = []
                }
            }
        in
        List.map aux_entry entries
    end
    | S.STRec (x, st_next) -> extract_events st_next.place k st_next.value
    | S.STTimeout (time_expr, st_next) ->        
        let next_events = (extract_events st.place (k+1) st_next.value) in
        let next_name = (List.hd next_events).value.name in

        { 
            place = place; 
            value = {
                T.vis=T.Public; 
                T.name= Atom.refresh_hint next_name ("Timeout"^(Atom.hint next_name));
                T.kind=T.Event; 
                T.args= []
            }
        }:: (extract_events st_next.place (k+1) st_next.value)
    in
    let sub_class_command = 
        T.ClassOrInterfaceDeclaration {
            isInterface = true;
            annotations = [T.Visibility T.Public];
            name = Atom.fresh_builtin "Command";
            extended_types = [auto_place (T.Atomic "CborSerializable")];
            implemented_types = [];
            body = [] 
        }
    in
    let sub_class_event = 
    begin
        let args = [ 
            auto_place (T.Atomic "String"), (Atom.fresh_builtin "bridge_id");
            auto_place (T.Atomic "int"), (Atom.fresh_builtin "session_id");
            auto_place (T.Atomic "Msg"), (Atom.fresh_builtin "msg")
        ] in
        let stmts = List.map (function (t, name) -> T.LetStmt (t, name, None)) args in
        let stmts = List.map (function stmt -> auto_place(T.Stmt (auto_place stmt))) stmts in

        let methods : T._method0 list = [
            { T.annotations = [T.Visibility T.Public];
                ret_type = auto_place T.TVoid;
                name = Atom.fresh_builtin "Event";
                body = AbstractImpl (List.map (function (_, name) -> (auto_place (T.AssignExpr (
                        auto_place (T.AccessExpr (
                            auto_place T.This, 
                            auto_place (T.VarExpr name)
                            )
                        ),
                        auto_place (T.VarExpr name)
                    )
                ))) args);
                args;
                is_constructor = true;
            }
        ] in
        let methods = List.map (function m -> auto_place (T.MethodDeclaration (auto_place m))) methods in

        T.ClassOrInterfaceDeclaration {
            isInterface = false;
            annotations = [T.Visibility T.Public];
            name = Atom.fresh_builtin "Event<Msg>";
            extended_types = [];
            implemented_types = [auto_place (T.Atomic "Command")];
            body = stmts @ methods 
        }
    end
    in
    let sub_classes = List.map (function cl -> auto_place cl) [sub_class_command; sub_class_event] in

    let stmts = [
        T.LetStmt ({place=fplace; value=(Atomic "String")}, (Atom.fresh_builtin "bridge_id"), None);
        T.LetStmt ({place=fplace; value=(Atomic "int")}, (Atom.fresh_builtin "session_id"), None);
        T.LetStmt ({place=fplace; value=(Atomic "ActorRef<TODO>")}, (Atom.fresh_builtin "other"), None)
    ] in
    let stmts = List.map (function stmt -> auto_place(T.Stmt {place=fplace; value=stmt})) stmts in
    let events = extract_events st.place 0 st.value in
    let events = List.map (function e -> {place=e.place@fplace; value=T.Event e}) events in
    [{place; value=T.ClassOrInterfaceDeclaration {
        isInterface = false;
        annotations = [T.Visibility T.Public];
        extended_types = [];
        implemented_types = [];
        name = name;
        body = sub_classes @ events @ stmts 
    }}]

    (* TODO generate the core of the protocol *)

| S.Typealias (v, S.AbstractTypealias body) -> raise (Error.PlacedDeadbranchError (place, "partial evaluation should have removed type alias exept those from impl"))
| S.Typealias (v, S.BBTypealias body) as term -> raise (Error.PlacedDeadbranchError (place, "should have been removed (and replaced) by clean_terms"))
|Typedef {value= EventDef (name, mts, None)as tdef; place = inner_place} ->
    [{
        place;
        value = T.Event { 
            place = inner_place; 
            value = {
                T.vis=T.Public; 
                T.name= name;
                T.kind=T.Event; 
                T.args=List.mapi ( fun i mt ->
                    fmtype mt, Atom.fresh_builtin ("value"^(string_of_int i))
                ) mts
            }
        }
    }]
| S.Typedef  {value= ClassicalDef (name, args, None) as tdef; place} -> (* implicit constructor should translate to akka *)
    let args = List.map (function (arg:T.ctype) -> (arg, Atom.fresh "arg")) (List.map fmtype args) in
    let constructor_body = 
        let place = (Error.forge_place "Plg=Akka/finish_term/typedef/implicit_constructor" 0 0) in
        let aux (_, arg_name) = 
            { place; value = T.AssignExpr (
                { place; value = T.AccessExpr (
                    { place; value = T.This }, 
                    { place; value = T.VarExpr arg_name }
                )},
                { place; value = T.VarExpr arg_name }
            )}
        in
        T.BlockStmt (List.map aux args)
    in
    let fields = 
        let place = (Error.forge_place "Plg=Akka/finish_term/typedef/implicit_constructor" 0 0) in
        let aux (arg_ctype, arg_name) = 
            { place; value = T.Stmt (
                { place; value = T.LetStmt (
                    arg_ctype, 
                    name,
                    None 
                )}
            )}
        in
        List.map aux args
    in

    let constructor = { place = place @ (Error.forge_place "Plg=Akka/finish_term/typedef/implicit_constructor" 0 0); value= 
        T.MethodDeclaration { place; value = {
            annotations = [T.Visibility T.Public];
            ret_type = {place; value = T.TVoid}; (* removed by the is_constructor *)
            name;
            body = T.AbstractImpl [{place; value  = constructor_body}];
            args = args;
            is_constructor = true
        }}
    }
    in

    [{place; value =
        T.ClassOrInterfaceDeclaration {
            isInterface = false;
            annotations = [T.Visibility T.Public];
            extended_types = [];
            implemented_types = [];
            name;
            body = fields @ [constructor] 
        }
    }]
| S.Typedef {value = ClassicalDef (v, _, Some body); _} ->
    if not body.value.template then (
        [{place; value = T.RawClass (v, {place = body.place; value = body.value.body})}] 
    ) else (
        [{place; value = T.TemplateClass {place=body.place; value = body.value.body}}]
    )
| S.Typedef {value = EventDef (v, _, Some body); _} -> Error.error place "eventdef with body is not yet supported by the akka.finish"

and fterm : S.term -> T.term list = function t -> finish_term t.place t.value

(* Remove type alias for java *)
and clean_terms : S.term list -> S.term list = function
| [] -> [] 
| {value= S.Typealias (x, S.BBTypealias body); _} :: terms ->
    let terms = List.map (S.type_replace_term (TVar x) (TRaw body)) terms in
    (clean_terms terms)
| term :: terms -> term::(clean_terms terms)

let finish_program terms = 
    let terms = clean_terms terms in
    let terms = List.flatten (List.map fterm terms) in

    { 
        T.entrypoint = [];
        T.system = {
            place = Error.forge_place "Plg=Akka/finish_program" 0 0;
            value = {
            name=Atom.fresh "AkkaSystem";
            methods= []; 
            states= []; 
            events=[]; 
            receiver = None;
            nested_items=[]}
        };
        T.terms = terms   
    }