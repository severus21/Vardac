open Core
open Easy_logging
open Fieldslib

let plg_name = "Akka"
let logger = Logging.make_logger ("_1_ compspec.plg."^plg_name) Debug [];;

(* The source calculus. *)
module S = IR 
(* The target calculus. *)
module T = Ast 

(* The translation of a complete program. *)

let fst3 (x,y,z) = x

(* Environments map strings to atoms. *)
module LabelsEnv = Atom.AtomsMap 

type finish_env = {
    labels:     T.variable LabelsEnv.t; } [@@deriving fields] 

let fresh_fenv () = {
    labels    = LabelsEnv.empty; }

(* FIXME: do we need to make fenv depend of the scope, if yes follow the cook architecture *)
let general_fenv = ref (fresh_fenv())

(* XXXX *)

type items_grps = { methods: S.method0 list; 
                    states: S.state list; 
                    nested: S.component_dcl list; 
                    ports: S.port list;
                    typedefs: (S.variable * S.main_type option) list;
                    others: S.term list}

let fresh_items_grp () = { 
    methods     = [];
    states      = [];
    nested      = [];
    ports       = [];
    typedefs    = [];
    others      = [];}

let group_cdcl_by (citems:  S.component_item list) : items_grps =
    let dispatch grp (citem: S.component_item) = match citem.value with
        | S.Contract _ -> Core.Error.error citem.place "Contract is not yet supported in Akka plg"
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
let getfst = function x -> Option.get (fst x )
let rec finish_ctype place : S._composed_type ->  T.ctype = function
    | S.TArrow (m1, m2) -> T.TFunction (
        getfst(fmtype m1), 
        getfst(fmtype m2)
    )

    | S.TVar x -> T.TVar x 
    | S.TFlatType ft -> begin match ft with  
        | S.TBool -> T.Atomic "Bool"
        | S.TInt -> T.Atomic "Int"
        | S.TFloat -> T.Atomic "Float"
        | S.TStr -> T.Atomic "Str"
        | S.TVoid -> TVoid
        | _ -> Core.Error.error place "TActivationInfo/Place/VPlace/Label type not yey supported."
    end
    | S.TDict (m1, m2) -> T.TMap (getfst(fmtype m1), getfst(fmtype m2))
    | S.TList mt -> T.TList (getfst(fmtype mt))
    | S.TOption mt -> T.TOption (getfst(fmtype mt))
    | S.TResult (m1, m2) -> T.TResult (getfst(fmtype m1), getfst(fmtype m2))
    | S.TSet mt -> T.TSet (getfst(fmtype mt))
    | S.TTuple mts ->  T.TTuple (List.map (fun x -> (getfst(fmtype x))) mts)
and fctype  : S.composed_type ->  T.ctype = function ct -> finish_ctype ct.place ct.value 

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

and event_name_of_mtype : S.main_type -> T.variable = function
(* TODO we should traverse all the recurisve type to ensure that we do not send/receive unserializable things -> maybe we should do this verification after the partial evaluation pass *)
| {place; value=S.CType ct} -> Atom.fresh (Printf.sprintf "%sEvent" (event_name_of_ctype ct.place ct.value))
| {place; _} -> Core.Error.error place "Session types, component/channel types can not be translated to a serializable Akka event."

(*
take the list of labels of a STBranch/STSelect and generate a related EventName. This Event will have a value of type string with the label value inside.
*)
and event_name_of_labels (labels: S.variable list) : T.variable = 
    try
        LabelsEnv.find labels !general_fenv.labels
    with Not_found -> begin
        let name = Atom.fresh "LabelEnv" in

        (* Update *)
        let labels_env = LabelsEnv.add labels name !general_fenv.labels in 
        general_fenv := {!general_fenv with labels = labels_env };

        name
    end

(* @param k order of the mtype in the protocol *)
and finish_stype k place : S._session_type ->  T.ctype option * T.event list  = function
    | S.STEnd -> None, [] (* Optimization: generation of a mock EndEvent is not needed *)
    | S.STSend (mt, st) | S.STRecv (mt, st) -> begin 
        let name =  event_name_of_mtype mt in
        let events = match finish_stype (k+1) st.place st.value with
        | None, events -> events
        | _,_ -> raise (Core.Error.DeadbranchError "finish_stype : STSend/STRecv a session type should not produce a ctype.")
        in

        match fmtype mt with
        | Some ct, [] ->
            None, {
                T.vis=T.Public; 
                T.name= name;
                T.kind=T.Event; 
                T.args=[(ct, Atom.fresh_builtin "value")]
            }::events
        | _,_ -> raise (Core.Error.DeadbranchError "finish_stype : STSend/STRecv type should not be a session type.")
    end
    | S.STBranch xs | S.STSelect xs ->
        let labels = List.fold_left (fun acc (label, _,_) -> label::acc) [] xs in

        let mock_place : Error.place = Error.forge_place "plugins/akka/Finish.ml" 0 0 in        let ct = Option.get (fst(finish_mtype mock_place (S.CType ({place=mock_place; value=S.TFlatType S.TStr})))) in

        let aux = function (label, (st: S.session_type), _) ->
            let events = match finish_stype (k+1) st.place st.value with
                | None, events -> events
                | _,_ -> raise (Core.Error.DeadbranchError "finish_stype : STSend/STRecv a session type should not produce a ctype.")
            in
            {
                T.vis=T.Public; 
                T.name= event_name_of_labels labels; 
                T.kind=T.Event; T.args=[(ct, Atom.fresh_builtin "value")]
            }::events
        in
        None, List.flatten (List.map aux xs)
    | S.STVar _ -> None, [] (*No need to signal the start of a new round*)
    | S.STRec (_,st) ->
        let events = match finish_stype (k+1) st.place st.value with
            | None, events -> events
            | _,_ -> raise (Core.Error.DeadbranchError "finish_stype : STSend/STRecv a session type should not produce a ctype.")
        in None, events 
and fstype : S.session_type -> T.ctype option * T.event list = function st -> (finish_stype 0) st.place st.value 

and finish_component_type place : S._component_type -> T.ctype = function
| S.CompTUid x -> T.TVar x 
and fcctype : S.component_type -> T.ctype = function t -> finish_component_type t.place t.value

and finish_mtype place : S._main_type -> T.ctype option * T.event list = function
| S.CType ct -> Some (fctype ct), [] 
| S.SType st -> fstype st
| S.CompType ct -> Some (fcctype ct), []
| _ -> Core.Error.error place "Akka: Type translation is only supported for composed types and session types"
and fmtype : S.main_type ->  T.ctype option * T.event list  = function mt -> finish_mtype mt.place mt.value

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
and fliteral : S.literal -> T.literal = function lit -> finish_literal lit.place lit.value

(************************************ Expr & Stmt *****************************)

and finish_expr place : S._expr -> T.expr = function
    | S.VarExpr x -> T.VarExpr x
    | S.AccessExpr (e1, e2) -> T.AccessExpr (fexpr e1, fexpr e2)
    | S.BinopExpr (t1, op, t2) -> T.BinopExpr (fexpr t1, op, fexpr t2)
    | S.LambdaExpr (x, stmt) -> failwith "Lambda is not yet supported" 
    | S.LitExpr lit -> T.LitExpr (fliteral lit)
    | S.UnopExpr (op, e) -> T.UnopExpr (op, fexpr e)

    | S.CallExpr (e1, es)-> T.CallExpr (fexpr e1, List.map fexpr es)

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
    | S.LetExpr (mt, x, e) ->  T.LetStmt (getfst (fmtype mt), x, Some (fexpr e))                             

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

(************************************ Channels *****************************)

and finish_channel_dcl place : S._channel_dcl -> T.term list = function 
| _ -> failwith "channel is not yet supported"
and fchdcl : S.channel_dcl -> T.term list = function chdcl -> finish_channel_dcl chdcl.place chdcl.value


(************************************ Component *****************************)
(* return type is T._expr for now, since we built only one state with all the variable inside FIXME *)
and finish_state place : S._state -> T.stmt = function 
    | S.StateDcl {ghost; kind; type0; name; init_opt} -> begin
        T.LetStmt (Option.get (fst (fmtype type0)), name, Option.map fexpr init_opt)
    end
    (*use global x as y;*)
    | S.StateAlias {ghost; kind; type0; name} -> failwith "finish_state StateAlias is not yet supported" 
and fstate : S.state -> T.stmt = function state -> finish_state state.place state.value 


and finish_param place : S._param -> (T.ctype * T.variable) = function
| mt, x -> Option.get (fst(fmtype mt)), x
and fparam : S.param -> (T.ctype * T.variable) = function p -> finish_param p.place p.value

and finish_method place actor_name : S._method0 -> T.method0 = function
    | S.CustomMethod m0 ->  
        assert( m0.contract_opt = None);

        let body = match m0.abstract_impl with
            | None -> T.ContinueStmt (*TODO TODO failwith "loading method from external sources is not yet supported"*) (* abstract method, implementation should be loaded from external sources*)
            | Some stmt -> fstmt stmt (* implem de reference -> generate code*)
        in

        { 
            vis             = T.Public; 
            ret_type        = Option.get (fst (fmtype m0.ret_type));
            name            = m0.name;
            args            = (List.map fparam m0.args);
            body            = body; 
            is_constructor  = false
        }
    | S.OnStartup m0 -> let m = fmethod actor_name m0 in  
        { m with name = actor_name ; is_constructor = true }
    | S.OnDestroy _ -> failwith "onstart and ondestroy are not yet supported"
and fmethod actor_name : S.method0 -> T.method0 = function m -> finish_method m.place actor_name m.value


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
    let events = List.flatten (List.map (function x -> snd (fmtype x)) (List.map (function x -> Option.get (snd x)) grp_items.typedefs)) in
    

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
            | S.STRecv (msg_type,_) -> Option.get (fst(fmtype msg_type))
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

    let receiver : T.method0= {
        args            = [];
        body            = T.ReturnStmt receiver_expr;
        name            = Atom.fresh_builtin "createReceive";
        ret_type        = T.Behavior "Command";
        vis             = T.Public;
        is_constructor  = false
    } in
    
    (* building methods *)
    let methods = List.map (fmethod name) grp_items.methods in 

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
| S.Channel chdcl -> fchdcl chdcl
| S.Comments c -> [T.Comments c.value]
| S.Component cdcl -> List.map (function a -> T.Actor a) (fcdcl cdcl)
| S.Stmt stmt -> [T.Stmt (fstmt stmt)]
| S.Typedef (v, mt_opt) -> [T.Class v] (* FIXME *) 

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