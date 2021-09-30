open Core
open Utils
open AstUtils
open Easy_logging
open Fieldslib
open Misc

(* Function composition, TODO put it in some Core file*)
let plg_name = "Akka"
let logger = Logging.make_logger ("_1_ compspec.plg."^plg_name) Debug [];;

(* The source calculus. *)
module S = IRI 
(* The target calculus. *)
module T = Ast 

let to_capitalize_variables = Hashtbl.create 64
let make_capitalize_renaming = function x ->
    match Hashtbl.find_opt to_capitalize_variables x with 
    | None -> x
    | Some _ -> Atom.refresh_hint x (String.capitalize_ascii (Atom.hint x))

(*** Global state *)
type collected_state = {
    event2receptionists : (Atom.t, Atom.t list) Hashtbl.t; 
}
let empty_cstate () : collected_state = {
    event2receptionists = Hashtbl.create 0
}
(*
    event -> list of components that can receive it 
*)
let event2receptionists : (Atom.t, Atom.t list) Hashtbl.t= Hashtbl.create 64
let add_event_e2rs event component : unit = 
    let vs = 
        try
            Hashtbl.find event2receptionists event
        with Not_found -> []
    in
    Hashtbl.add event2receptionists event (component::vs)

(*****)

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
    | S.TBridge b -> T.Atomic "Protocol.Bridge"
    | S.TRaw bbraw -> T.TRaw bbraw.value.body
and fctype ct :  T.ctype = finish_place finish_ctype ct

(* Represent an ST object in Java type *)
and finish_stype place : S._session_type -> T._ctype = function 
    | S.STEnd -> T.TVar (Atom.fresh_builtin "Protocol.STEnd") 
    | (S.STSend (mt, st) as st0) | (S.STRecv (mt, st) as st0) -> begin 
        match fmtype mt with
        | ct ->
            T.TParam (
                {
                    place;
                    value =  T.TVar (Atom.fresh_builtin (match st0 with | S.STSend _ -> "Protocol.STSend" | STRecv _ -> "Protcol.STRecv"))
                },
                [ct; fstype st]
            )
        | _ -> raise (Core.Error.PlacedDeadbranchError (mt.place, "finish_stype : STSend/STRecv type should not be a session type."))
    end
    | (S.STBranch xs as st0) | (S.STSelect xs as st0) ->
        let rec built_t_hlist = function
            | [] -> {place; value = T.TVar (Atom.fresh_builtin "Protocol.HNil")}
            | st::sts -> { place; value = T.TParam( 
                { 
                    place = st.place; 
                    value = T.TVar (Atom.fresh_builtin "Protocol.HCons")
                }, [
                    {place = place; value = T.TParam( 
                        { 
                            place = st.place;
                            value = T.TVar (Atom.fresh_builtin "Protocol.STEntry")
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
                value = T.TVar (Atom.fresh_builtin (match st0 with | S.STBranch _ -> "Protocol.STBranch" | S.STSelect _ -> "Protocol.STSelect"))
            },
            [continuation_st]
        )
    | S.STVar _ -> T.TVar (Atom.fresh_builtin ("Protocol.STVar"))
    | S.STRec (_,st) ->
        T.TParam (
            { 
                place;
                value = T.TVar (Atom.fresh_builtin "Protocol.STRec")
            },
            [ fstype st]
        )

    | S.STInline x -> 
        raise (Error.PlacedDeadbranchError (place, "STInline should remains outside the codegen part, it should have been resolve during the partial evaluation pass."))
and fstype st : T.ctype = finish_place finish_stype st

(* Represent an ST object in Java value *)
and finishv_stype place : S._session_type -> T._expr = 
(****** Helpers *****)
let fplace = place@(Error.forge_place "Plg=Akka/finishv_stype" 0 0) in
let auto_place smth = {place = fplace; value=smth} in
let  encodectype = function
| {value=T.TVar x; place} -> {value=T.VarExpr x; place} 
in
function 
    | S.STEnd -> T.VarExpr (a_ASTStype_of "End")
    | (S.STSend (mt, st) as st0) | (S.STRecv (mt, st) as st0) -> begin 
        match fmtype mt with
        | ct ->
            let constructor = auto_place (match st0 with 
            | S.STSend _ -> T.VarExpr (a_ASTStype_of "Send")
            | STRecv _ -> T.VarExpr (a_ASTStype_of "Receive")
            ) in

            T.CallExpr (
                constructor,
                [
                    encodectype ct;
                    fvstype st
                ]
            )
        | _ -> raise (Core.Error.PlacedDeadbranchError (mt.place, "finish_stype : STSend/STRecv type should not be a session type."))
    end
    | (S.STBranch xs as st0) | (S.STSelect xs as st0) ->
        let constructor = auto_place (match st0 with 
        | S.STSend _ -> T.VarExpr (a_ASTStype_of "Branch")
        | STRecv _ -> T.VarExpr (a_ASTStype_of "Select")
        ) in

        T.CallExpr (
            constructor,
            [
                auto_place (T.BlockExpr (
                    IR.List,
                    List.map (function (label, st, _) -> auto_place (T.BlockExpr (
                        IR.Tuple, 
                        [ auto_place (T.VarExpr label); fvstype st ]
                    ))) xs
                ))
            ]
        )
    | S.STVar _ -> failwith "Not yet supported" 
    | S.STRec (_,st) -> failwith "Not yet supported"
    | S.STInline x -> 
        raise (Error.PlacedDeadbranchError (place, "STInline should remains outside the codegen part, it should have been resolve during the partial evaluation pass."))
and fvstype st : T.expr = finish_place finishv_stype st

and finish_component_type place : S._component_type -> T._ctype = function
| S.CompTUid x -> T.TVar x 
and fcctype ct : T.ctype = finish_place finish_component_type ct

and finish_mtype place : S._main_type -> T.ctype = function
| S.CType ct -> fctype ct 
| S.SType st -> {
    place; 
    value = T.TParam (
        { 
            place;
            value = T.TVar (a_ASTStype_of "")
        },
        [ fstype st]
    )
}
| S.CompType ct -> fcctype ct
| _ -> Core.Error.error place "Akka: Type translation is only supported for composed types and session types"
and fmtype : S.main_type ->  T.ctype = function mt -> finish_mtype mt.place mt.value

(************************************ Literals *****************************)

and finish_literal place : S._literal -> T._literal = function
    | S.VoidLit -> T.VoidLit
    | S.BoolLit b -> T.BoolLit b
    | S.FloatLit f -> T.FloatLit f
    | S.IntLit i -> T.IntLit i
    | S.LabelLit l -> Core.Error.error place "Label are not yet supported"
    | S.StringLit str -> T.StringLit str

    | S.ActivationInfo _ -> failwith "Activation info is not yet supported"

    | S.Place _ -> failwith "Place is not yet supported"
    | S.VPlace _ -> failwith "VPlace is not yet supported"
    | S.Bridge _ -> raise (Error.DeadbranchError "Bridge should have been process by the finish_expr (returns an expr)")
and fliteral lit : T.literal = finish_place finish_literal lit

(************************************ Expr & Stmt *****************************)

and finish_expr place : S._expr -> T._expr =
let fplace = place@(Error.forge_place "Plg=Akka/finish_expr" 0 0) in
let auto_place smth = {place = fplace; value=smth} in
function
    | S.VarExpr x -> T.VarExpr x
    | S.AccessExpr (e1, e2) -> T.AccessExpr (fexpr e1, fexpr e2)
    | S.BinopExpr (t1, op, t2) -> T.BinopExpr (fexpr t1, op, fexpr t2)
    | S.LambdaExpr (x, stmt) -> T.LambdaExpr ([x], fstmt stmt) 
    | S.LitExpr {value=S.Bridge b; place=lit_place} -> T.NewExpr(
        auto_place (T.VarExpr (a_ASTStype_of "Bridge")),
        [
            e_session_of_protocol fplace (auto_place (T.VarExpr b.protocol_name))
        ]
    )
    | S.LitExpr lit -> T.LitExpr (fliteral lit)
    | S.UnopExpr (op, e) -> T.UnopExpr (op, fexpr e)

    | S.CallExpr (e1, es) -> begin 
        match e1.value with 
        | S.VarExpr x when Atom.is_builtin x ->
            Encode.encode_builtin_fct e1.place (Atom.value x) (List.map fexpr es)
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
                        List.map fexpr args
                    )
                }] @ [ auto_place (T.LitExpr (auto_place (T.StringLit (Atom.to_string (Atom.fresh "actor_name")))))]
            )}
        }
    | S.Spawn {c; args; at=at} ->
        failwith "finish_expr spawn with place annotation not yet supported" 

    | S.BoxCExpr _ -> failwith "finish_expr BoxCexpr is not yet supported"
    
    | S.OptionExpr e_opt -> failwith "option not yet supported" 
    | S.ResultExpr (None, Some err) ->  T.CallExpr (
        { place; value = T.VarExpr (Atom.fresh_builtin "Either.left")},
        [fexpr err]
    ) 
    | S.ResultExpr (Some ok, None) -> T.CallExpr (
        { place; value = T.VarExpr (Atom.fresh_builtin "Either.right")},
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

and finish_eventdef (inner_place:Error.place) (name, mts, body) : T.event =
match body with  
| None -> { 
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

and finish_function place : S._function_dcl -> T.method0 list = function
    | f ->
        let body = match f.body with
            | S.AbstractImpl stmts -> T.AbstractImpl (List.map fstmt stmts)
            | S.BBImpl body -> T.BBImpl body
        in

        let new_function : T.method0 = {
            place;
            value = { 
                decorators      = [];
                annotations     = [ T.Visibility T.Public ]; 
                ret_type        = fmtype f.ret_type;
                name            = f.name;
                args            = (List.map fparam f.args);
                body            = body; 
                is_constructor  = false 
            }
        } in

        [new_function]
and ffunction : S.function_dcl -> T.method0 list = function m -> finish_function m.place m.value
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
                decorators       = [];
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
                decorators      = [];
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
            decorators      = method0.value.decorators;
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
                decorators      = []; 
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
    assert( args == []); (* Not supported yet, maybe one day *)

    (* Registration *)
    Hashtbl.add to_capitalize_variables name ();

    (****** Helpers *****)
    let fplace = (Error.forge_place "Plg=Akka/finish_term/protocoldef" 0 0) in
    let auto_place smth = {place = fplace; value=smth} in
    let expr2stmt e : T.stmt = auto_place (T.ExpressionStmt e) in
    let exprs2stmts es : T.stmt list = List.map expr2stmt es in

    (***** Processing *****)

    (* Group by kind the elmts : method, state, ...
        in order to display well stuctured code at the end (and since the order of definition do not matter)
    *)
    let grp_items = group_cdcl_by body in
    assert(grp_items.others == []);

    
    (*** Building events ***)
    (* Events that should be defined inside the Actor *)
    let is_stype = function
        | _, Some ({Core.AstUtils.place; Core.AstUtils.value = S.SType st}) -> false
        | _ -> true
    in
    let events = List.map ( function
        | {value=S.EventDef (name, mts, body); place} ->
            finish_eventdef place (name, mts, body)
    ) grp_items.eventdefs in
       (* 
        List.flatten (List.map (function x -> snd (fmtype x)) (List.filter_map (function x -> match snd x with |S.AbstractTypealias mt -> Some mt | _ -> None) (List.map (function |{value=S.EventDef (x, mts,body); _} -> (x, mts, body)) grp_items.eventdefs))) in*)
    

    (*** Building states ***)
    let states : T.state list = List.map (
        function state -> {
            place = state.place; 
            value = {T.persistent=false; stmts= [fstate state] }
        } (* TODO handle persistency*)
    ) grp_items.states in 

    (*** Building receiver ***)
    (* Step0 - name of receiver param (event) *)
    let l_event_name : Atom.atom = (Atom.fresh_builtin "e") in
    let l_event : T.expr = auto_place (T.VarExpr l_event_name) in

    (* Step1 - create {event_name: {(bridge_expr, remaining_step i.e st) ->  callbak}} *)
    let env : (Atom.atom, (T.expr * S.session_type, T.expr) Hashtbl.t) Hashtbl.t = Hashtbl.create 16 in
    let hydrate_env (p: S.port) = 
        let msg_type, remaining_st = match p.value.expecting_st.value with 
        | S.SType st -> begin
            match st.value with
            | S.STRecv ({value=S.CType {value=S.TVar event_name;};}, st) -> event_name, st
            | S.STRecv _ -> Core.Error.error p.place "only event type supported"
            | S.STBranch xs -> failwith "TODO"
            | S.STEnd | S.STVar _ |S.STRecv _-> failwith "TOTO"
            | S.STInline _ -> failwith "TITI"
            | _ -> Core.Error.error p.value.expecting_st.place "%s plugin: expecting type can only start by the reception of a message or of a label" plg_name  
        end 
        | _ -> Core.Error.error p.value.expecting_st.place "%s plugin do not support main type for port expecting" plg_name  
        in
       
        let inner_env = begin 
            try 
                Hashtbl.find env msg_type 
            with Not_found -> let _inner_env = Hashtbl.create 8 in Hashtbl.add env msg_type _inner_env; _inner_env
        end in

        let key = (fexpr p.value.input, remaining_st) in

        (* check that key are not duplicated for the current event *)
        try
            ignore (Hashtbl.find inner_env key);
            Error.error (place@p.place) "Tuple (bridge, st) is not unique for the component %s" (Atom.hint name)
        with Not_found -> Hashtbl.add inner_env key (fexpr p.value.callback)
    in

    List.iter hydrate_env grp_items.ports;

    (* Step 2 - Generate a receiver per event *)
    let generate_event_receiver (event_name:Atom.atom) (inner_env:(T.expr * S.session_type, T.expr) Hashtbl.t) : T.stmt list =
        (* Helpers *)
        let bridgeid (bridge: T.expr) = auto_place (T.AccessExpr (
            bridge,
            auto_place (T.VarExpr (Atom.fresh_builtin "id"))
        )) in
        let e_bridgeid e = auto_place (T.AccessExpr (
            e,
            auto_place (T.VarExpr (Atom.fresh_builtin "bridge_id"))
        )) in
        let remaining_stepof st = fvstype st in
        let e_remaining_step e = auto_place (T.AccessExpr (
            e,
            auto_place (T.VarExpr (Atom.fresh_builtin "current_set"))
        )) in

        (* Creating the statement*)
        (* TODO do it with a switch ??? *)
        (*
            if(e.bridge_id == port_bridge.id && e.current_set == 
            current_aststype){
                this.handle_ping46(e);
            }else{
                ...
            }
        *)
        let add_case (bridge, st) (callback:T.expr) acc : T.stmt =
            auto_place (T.IfStmt (
                auto_place (T.BinopExpr(
                    auto_place (T.BinopExpr (e_bridgeid l_event, S.Equal, bridgeid bridge)),
                    S.And,
                    auto_place (T.BinopExpr (e_remaining_step l_event, S.Equal, remaining_stepof st))
                )),
                auto_place (T.ExpressionStmt (auto_place (
                    T.CallExpr(
                        callback,
                        [ l_event ]
                    )
                ))),
                Some acc
            ))
        in

        (* return Behaviors.same(); *)
        let ret_stmt = T.ReturnStmt (auto_place (T.CallExpr ( 
            auto_place (T.AccessExpr (
                auto_place (T.VarExpr (Atom.fresh_builtin "Behaviors")),
                auto_place (T.VarExpr (Atom.fresh_builtin "same"))
            )),
            []
        ))) in

        [ Hashtbl.fold add_case inner_env (auto_place (T.EmptyStmt)) ] @ [auto_place ret_stmt]
    in

    (* Step3 - Generate the component receiver *)
    let generate_component_receiver () = 
        let init_receiver_expr = {place; value=T.CallExpr(
            {place; value=T.VarExpr (
                Atom.fresh_builtin "newReceiveBuilder"
            )}, 
            []
        )} in
        let add_case event_name inner_env (acc, acc_methods) =
            let _m_name = Atom.fresh "event_dispatcher" in
            let _m : T.method0 = auto_place {
                T.decorators = [];
                annotations = [T.Visibility T.Public];
                ret_type = t_behavior_of_actor fplace name;
                name = _m_name;
                body = AbstractImpl (generate_event_receiver event_name inner_env);
                args = [(auto_place (T.TVar event_name), l_event_name)];
                is_constructor = false;
            } in

            ({place; value=T.AccessExpr(
                acc, 
                {place; value=T.CallExpr(
                    {place; value=T.VarExpr (Atom.fresh_builtin "onMessage")}, 
                    [
                        {place; value=T.ClassOf (auto_place (T.TVar event_name))};
                        auto_place (T.AccessMethod (
                            auto_place T.This,
                            _m_name
                        ))
                    ]
                )}
            )}, _m::acc_methods)
        in
        Hashtbl.fold add_case env (init_receiver_expr, [])
    in

    let (receiver_body, receiver_methods) = generate_component_receiver () in

    let receiver_expr = {place; value=T.AccessExpr(
        receiver_body, 
        { place; value=T.CallExpr(
            {place; value=T.VarExpr (Atom.fresh_builtin "build")},
            []
        )})}
    in

    let receiver : T.method0 = { 
        place;
        value = {
            decorators      = [Override];
            args            = [];
            body            = T.AbstractImpl ([
                {place; value=T.ReturnStmt receiver_expr}
            ]);
            name            = Atom.fresh_builtin "createReceive";
            ret_type        = t_receive_of_actor place name;
            annotations     = [ T.Visibility T.Public ];
            is_constructor  = false
        }
    } in

    (* Step 4 - Prepare parent_env for updating event definition in order to
        event Pong implements C.Command for all C that can receive a Pong event
    *)
    Hashtbl.iter (fun event _ -> add_event_e2rs event name) env;
    
    (***** Building methods *****)
    let methods = receiver_methods @ (List.flatten (List.map (fmethod name) grp_items.methods)) in 
    

    (***** Sumup *****)
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
| S.Function f -> List.map (function m -> {
    place; 
    value = T.MethodDeclaration m
}) (ffunction f)
| S.Typedef {value=S.ProtocolDef (name, {value=S.SType st; _});_} -> 
    (*** Helpers ***)
    let fplace = (Error.forge_place "Plg=Akka/finish_term/protocoldef" 0 0) in
    let auto_place smth = {place = fplace; value=smth} in
    let expr2stmt e : T.stmt = auto_place (T.ExpressionStmt e) in
    let exprs2stmts es : T.stmt list = List.map expr2stmt es in

    (* Registration *)
    Hashtbl.add to_capitalize_variables name ();


    (* TODO generalize the usage of auto place *)

    (*** Processing ***)
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


    (*** Inner class: Event<Msg> ***)
    let cl_event_name = Atom.fresh_builtin "Event" in
    let sub_class_event = 
    begin
        let l_st = Atom.fresh_builtin "st" in
        let this_st = T.AccessExpr (
                            auto_place T.This, 
                            auto_place (T.VarExpr l_st)
                            ) in

        (*** Constructor ***)
        let constructor_args = [ 
            auto_place (T.Atomic "String"), (Atom.fresh_builtin "bridge_id");
            auto_place (T.Atomic "int"), (Atom.fresh_builtin "session_id");
            (auto_place (T.Atomic "ASTStype")), (Atom.fresh_builtin "st");
            auto_place (T.Atomic "Msg"), (Atom.fresh_builtin "msg");
        ] in
        let m_constructor = { 
            T.decorators      = [];
            annotations = [T.Visibility T.Public];
                ret_type = auto_place T.TVoid;
                name = cl_event_name;
                body = AbstractImpl (List.map (function (_, name) -> (auto_place (T.AssignExpr (
                        auto_place (T.AccessExpr (
                            auto_place T.This, 
                            auto_place (T.VarExpr name)
                            )
                        ),
                        auto_place (T.VarExpr name)
                    )
                ))) constructor_args);
                args = constructor_args;
                is_constructor = true;
        } in

        let methods : T.method0 list = List.map auto_place [ m_constructor ] in
        let methods = List.map (function m -> auto_place (T.MethodDeclaration m)) methods in

        (*** Attributes ***)
        let stmts = List.map (function (t, name) -> T.LetStmt (t, name, None)) constructor_args in
        let stmts = List.map (function stmt -> auto_place(T.Stmt (auto_place stmt))) stmts in


        (*** CL Def ***)
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

    (*** Inner class: Session ***)
    let cl_session_name = Atom.fresh_builtin "Session" in
    let sub_class_session = 
    begin
        (*** Helpers ***)
        let l_bridge_id = Atom.fresh_builtin "bridge_id" in
        let l_session_id = Atom.fresh_builtin "session_id" in
        let l_right = Atom.fresh_builtin "right" in
        let l_st = Atom.fresh_builtin "st" in
        let this_bridge_id = T.AccessExpr (
                            auto_place T.This, 
                            auto_place (T.VarExpr l_bridge_id)
                            ) in
        let this_session_id = T.AccessExpr (
                            auto_place T.This, 
                            auto_place (T.VarExpr l_session_id)
                            ) in
        let this_right = T.AccessExpr (
                            auto_place T.This, 
                            auto_place (T.VarExpr l_right)
                            ) in
        let this_st = T.AccessExpr (
                            auto_place T.This, 
                            auto_place (T.VarExpr l_st)
                            ) in

        (*** Constructor ***)
        let constructor_args = [ 
            auto_place (T.Atomic "String"), l_bridge_id;
            auto_place (T.ActorRef (auto_place(T.Atomic "?"))), l_right;
            auto_place (T.Atomic "int"), l_session_id;
            (auto_place (T.TVar (a_ASTStype_of ""))), l_st;
        ] in
        let m_constructor = { 
            T.decorators      = [];
            annotations = [T.Visibility T.Public];
                ret_type = auto_place T.TVoid;
                name = cl_session_name;
                body = AbstractImpl (List.map (function (_, name) -> (auto_place (T.AssignExpr (
                        auto_place (T.AccessExpr (
                            auto_place T.This, 
                            auto_place (T.VarExpr name)
                            )
                        ),
                        auto_place (T.VarExpr name)
                    )
                ))) constructor_args);
                args = constructor_args;
                is_constructor = true;
        } in


        (*** Method: fire ***)
        let m_fire_name = Atom.fresh_builtin "fire" in
        let fire_args = [
            auto_place (T.Atomic "Msg"), (Atom.fresh_builtin "msg");
            auto_place (T.Atomic "ActorContext"), (Atom.fresh_builtin "context");
        ] in
        let m_fire = 
            let l_msg = T.VarExpr (Atom.fresh_builtin "msg") in  
            let l_context = T.VarExpr (Atom.fresh_builtin "context") in  
            let cl_msg_name = Atom.fresh_builtin "Msg" in  
            let l_e = Atom.fresh_builtin "e" in
            { 

                T.decorators      = [];
                annotations = [T.Visibility T.Public];
                ret_type = auto_place (T.TVar cl_session_name);
                name = m_fire_name;
                body = AbstractImpl ([
                    (* Event<Msg> e = Event(this.bridge_id, this.session_id, msg); *)
                    (auto_place (T.LetStmt (
                        auto_place (T.TParam 
                        (auto_place (T.TVar cl_event_name),
                        [ (auto_place (T.TVar cl_msg_name)) ]
                        )),
                        l_e,
                        Some (auto_place (T.CallExpr (
                            auto_place (T.VarExpr cl_event_name),
                            [
                                auto_place this_bridge_id;
                                auto_place this_session_id;
                                auto_place this_st;
                                auto_place l_msg;
                            ]
                        ))
                    ))));
                    
                    (* this.right.tell(e,  getContext().getSelf()); *)
                    expr2stmt (auto_place (T.CallExpr (
                        auto_place (T.AccessExpr (
                            auto_place this_right,
                            auto_place (T.VarExpr (Atom.fresh_builtin "tell"))
                            )
                        ),
                        [
                            auto_place (T.VarExpr l_e);
                            e_get_self fplace (auto_place l_context)
                        ]
                    )));

                    (* this.st = st.continuation *)
                    (auto_place (T.AssignExpr (
                        auto_place this_st,
                        auto_place (T.AccessExpr (
                            auto_place this_st,
                            auto_place (T.VarExpr (Atom.fresh_builtin "continuation"))
                        ))
                    )));
                    (* return e.msg; *)
                    (auto_place (T.ReturnStmt (
                        auto_place (T.This)
                    )))
                ]);
                args = fire_args;
                is_constructor = false;
            } 
        in

        (*** Method: receive ***)
        let m_receive_name = Atom.fresh_builtin "receive" in
        let receive_args = [
            auto_place (T.Atomic "ActorContext"), (Atom.fresh_builtin "context");
        ] in
        let m_receive = 
            let l_msg = T.VarExpr (Atom.fresh_builtin "msg") in  
            let l_context = T.VarExpr (Atom.fresh_builtin "context") in  
            let l_timeout = (Atom.fresh_builtin "timeout") in (* TODO deals with user defined timeout + load timeout from config *) 
            let l_future = (Atom.fresh_builtin "future") in 
            let cl_msg_name = Atom.fresh_builtin "Msg" in  
            let l_e = Atom.fresh_builtin "e" in

            { 
                T.decorators = [];
                annotations = [T.Visibility T.Public];
                ret_type = auto_place (T.TTuple [ 
                    auto_place (T.TVar (Atom.fresh_builtin "Object"));
                    auto_place (T.TVar cl_session_name)
                ]);
                name = m_receive_name;
                body = AbstractImpl ([
                    (* final Duration timeout = Duration.ofSeconds(3); *)
                    (auto_place (T.LetStmt (
                        (auto_place (T.TVar (Atom.fresh_builtin "Duration"))),
                        l_timeout,
                        Some (auto_place (T.CallExpr (
                            auto_place (T.VarExpr (Atom.fresh_builtin "Duration.ofSeconds")),
                            [
                                auto_place (T.LitExpr (auto_place(T.IntLit 3)));
                            ]
                        ))
                    ))));

                    (* CompletableFuture<Event<Object>> future = ask(actorB, msg, timeout).toCompletableFuture(); *)
                    (auto_place (T.LetStmt (
                        (auto_place (T.TParam(
                            auto_place (T.TVar (Atom.fresh_builtin "CompletableFuture")),
                            [ 
                                auto_place (T.TVar (Atom.fresh_builtin "Event<Object>"))
                            ]
                        ))),
                        l_future,
                        Some (auto_place (T.AccessExpr ( 
                            auto_place (T.CallExpr (
                                auto_place (T.VarExpr (Atom.fresh_builtin "ask")),
                                [
                                    auto_place this_right;
                                    auto_place l_msg;
                                    auto_place (T.VarExpr l_timeout);
                                ]
                            )),
                            auto_place (T.CallExpr (
                                auto_place (T.VarExpr (Atom.fresh_builtin "toCompletableFuture")),
                                []
                            ))
                        ))
                    ))));

                    (* Event<Object> msg = (Event<Object>) future.join() *)
                    (auto_place (T.LetStmt (
                        (auto_place (T.TParam(
                            auto_place (T.TVar (Atom.fresh_builtin "Event")),
                            [
                                auto_place (T.TVar (Atom.fresh_builtin "Object"))
                            ]
                        ))),
                        l_e,
                        Some (auto_place (T.CallExpr (
                            auto_place (T.AccessExpr(
                                auto_place (T.VarExpr l_future),
                                auto_place (T.VarExpr (Atom.fresh_builtin "join"))
                            )),
                            [
                                auto_place (T.LitExpr (auto_place(T.IntLit 3)));
                            ]
                        ))
                    ))));

                    (* this.st = st.continuation *)
                    (auto_place (T.AssignExpr (
                        auto_place this_st,
                        auto_place (T.AccessExpr (
                            auto_place this_st,
                            auto_place (T.VarExpr (Atom.fresh_builtin "continuation"))
                        ))
                    )));

                (* return Tuple2(e.msg, this); *)
                    (auto_place (T.ReturnStmt (
                        auto_place ( T.NewExpr (
                            auto_place ( T.VarExpr (Atom.fresh_builtin "Tuple2")),
                            [
                                auto_place ( T.AccessExpr(
                                    auto_place (T.VarExpr l_e),
                                    auto_place (T.VarExpr (Atom.fresh_builtin "msg"))
                                ));
                                auto_place ( T.This)
                            ]
                        ))
                    )))
                ]);
                args = receive_args;
                is_constructor = false;
            } 
        in
        
        (*** Other communication methods ***)

        let methods : T.method0 list = List.map auto_place [ m_constructor; m_fire; m_receive ] in
        let methods = List.map (function m -> auto_place (T.MethodDeclaration m)) methods in

        (*** Attributes ***)
        let stmts = List.map (function (t, name) -> auto_place(T.LetStmt (t, name, None))) constructor_args in
        let stmts = List.map (function stmt -> auto_place(T.Stmt (stmt))) stmts in


        (*** CL Def ***)
        T.ClassOrInterfaceDeclaration {
            isInterface = false;
            annotations = [T.Visibility T.Public];
            name = Atom.fresh_builtin "Session";
            extended_types = [];
            implemented_types = [auto_place (T.Atomic "Command")];
            body = stmts @ methods 
        }
    end
    in

    
    (*let events = extract_events st.place 0 st.value in
    let events = List.map (function e -> {place=e.place@fplace; value=T.Event e}) events in*)

    (*** Helpers ***)
    let l_st = Atom.fresh_builtin "st" in
    let this_st = T.AccessExpr (
        auto_place T.This, 
        auto_place (T.VarExpr l_st)
    ) in

    (*** Constructor ***)
    let event_constructor_args = [ 
        auto_place (T.Atomic "String"), (Atom.fresh_builtin "bridge_id");
        auto_place (T.Atomic "int"), (Atom.fresh_builtin "session_id");
        auto_place (T.ActorRef (auto_place(T.Atomic "?"))), (Atom.fresh_builtin "right")
    ] in
(*    let m_event_constructor = { 
        T.annotations = [T.Visibility T.Public];
            ret_type = auto_place T.TVoid;
            name = name;
            body = AbstractImpl (
                (List.map (function (_, name) -> (auto_place (T.AssignExpr (
                        auto_place (T.AccessExpr (
                            auto_place T.This, 
                            auto_place (T.VarExpr name)
                            )
                        ),
                        auto_place (T.VarExpr name)
                    )
                ))) event_constructor_args)
                @
                [
                    (* this.st = encoding *)
                    auto_place (T.AssignExpr (
                        auto_place this_st,
                        fvstype st
                    ))
                ]
            );
            args = event_constructor_args;
            is_constructor = true;
    } in
*)
    (* TODO with ports *)

    (*** Body assembly ***)
    let sub_classes = List.map (function cl -> auto_place cl) [sub_class_command; sub_class_event; sub_class_session] in

    let methods : T.method0 list = List.map auto_place [] in
    let methods = List.map (function m -> auto_place (T.MethodDeclaration m)) methods in

    let stmts = List.map (function stmt -> auto_place (T.Stmt stmt)) [] in

    [{place; value=T.ClassOrInterfaceDeclaration {
        isInterface = false;
        annotations = [T.Visibility T.Public];
        extended_types = [auto_place (T.Atomic "Protocol")];
        implemented_types = [];
        name = name;
        body = stmts @ sub_classes @ methods (*@ events*)
    }}]

    (* TODO generate the dynamic checking of protocol order if needed *)

| S.Typealias (v, S.AbstractTypealias body) -> raise (Error.PlacedDeadbranchError (place, "partial evaluation should have removed type alias exept those from impl"))
| S.Typealias (v, S.BBTypealias body) as term -> raise (Error.PlacedDeadbranchError (place, "should have been removed (and replaced) by clean_terms"))
|Typedef {value= EventDef (name, mts, None) as tdef; place = inner_place} ->
    (* Registration *)
    Hashtbl.add to_capitalize_variables name ();

    [{
        place;
        value = T.Event (finish_eventdef inner_place (name, mts, None)) 
    }]
| S.Typedef  {value= ClassicalDef (name, args, None) as tdef; place} -> (* implicit constructor should translate to akka *)
    (* Registration *)
    Hashtbl.add to_capitalize_variables name ();

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
            decorators = [];
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
    (* Registration *)
    Hashtbl.add to_capitalize_variables v ();

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
    let terms = List.flatten (List.rev(List.map fterm terms)) in
    let terms = List.map (T.apply_rename_term (make_capitalize_renaming)) terms in 

    {
        event2receptionists;
    }, { 
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