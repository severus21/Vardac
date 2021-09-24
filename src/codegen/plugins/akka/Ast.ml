(** Underlying programming language primitives *)
open Core
open AstUtils

(************************************* Base types ****************************)
open Label

type variable = Atom.atom

and comments = Core.IR._comments

(************************************ Types **********************************)
and _ctype = 
    | Atomic of string (*int, float, ?, ..*)           
    | Behavior of string 
    | ActorRef of ctype
    | TFunction of ctype * ctype
    | TList of ctype 
    | TMap of ctype * ctype (*Map<String, String> map = new HashMap<String, String>();*)
    | TOption of ctype (*https://www.touilleur-express.fr/2014/11/07/optional-en-java-8/*)
    | TResult of ctype * ctype
    | TSet of ctype (*Set<String> set =  new HashSet<String>() ;*)
    | TTuple of ctype list 
    | TVar of variable
    | TVoid

    | TParam of ctype * ctype list (* Type parametric (ct, [arg_1; .. arg_n]) -> ct<arg_1, .., arg_n>*)
    | TRaw of string
and ctype =_ctype placed

(************************************ Expr & Stmt *****************************)

and unop = IR.unop 
and binop = IR.binop 

and _literal = 
    | VoidLit
    | BoolLit of bool
    | FloatLit of float 
    | IntLit of int
    | StringLit of string
and literal = _literal placed

and parameter = ctype * variable

and _expr =                  
    | AccessExpr of expr * expr
    | AssertExpr of expr 
    | BinopExpr of expr * binop * expr 
    | CallExpr of expr * expr list
    | LambdaExpr of variable list * stmt 
    | LitExpr of literal
    | This
    | UnopExpr of unop * expr         
    | VarExpr of variable                    
    | NewExpr of expr * expr list                   

    | ClassOf of ctype 

    | BlockExpr of IR.block * expr list

    (** Akka scheduler *)
    | Spawn of { context: expr; actor_expr: expr }
    | CurrentContext
    | CurrentSystem
    | RawExpr of string
and expr = _expr placed

and _stmt = 
    | AssignExpr of expr * expr (*expression should be access or variable*)
    | BlockStmt of stmt list
    | BreakStmt
    | EmptyStmt
    | LetStmt of ctype * variable * expr option(*int x = blabl*)
    | CommentsStmt of comments
    | ContinueStmt
    | ExpressionStmt of expr
    | IfStmt of expr * stmt * stmt option               
    | ReturnStmt of expr
and stmt = _stmt placed

and visibility = 
    | Private
    | Protected
    | Public
and annotation =
    | Visibility of visibility
    | Static 
    | Final
and method0_body = 
| AbstractImpl of stmt list
| BBImpl of Impl_common.blackbox_term

and _method0 = {
    annotations: annotation list;
    ret_type: ctype;
    name: variable;
    body: method0_body;
    args: (ctype * variable) list;
    is_constructor: bool
}
and method0 = _method0 placed
(************************************ Akka Actors **********************************)

(** Akka specific primitives *)
and event_kind = 
    | Command 
    | Event

and _event = {
    vis: visibility;
    name: variable;
    kind: event_kind; 
    args: (ctype * variable) list
}
and event = _event placed

and _state = {
    persistent: bool;
    (** list of let*)
    stmts: stmt list
}
and state = _state placed

and _actor = {
    name: variable;
    methods: method0 list;
    (*
    @Override
    public Receive<Command> createReceive() {
        return newReceiveBuilder()
                .onMessage(PutEvent.class, this::onPut)
                .onMessage(GetEvent.class, this::onGet)
                .onMessage(CommitEvent.class, this::onCommit)
                .onMessage(AbortEvent.class, this::onAbort)
                .build();
    }
    *)
    receiver: method0 option;
    states: state list;
    events: event list; 
    nested_items: term list;
}
and actor = _actor placed
(************************************ Akka streams **********************************)
(* TODO *)

(************************************ Akka program **********************************)
and raw = string placed

and _term = 
| Comments of comments
(** Preprocessor *)
| Import of string

(** Other *)
| Class of variable 
| ClassOrInterfaceDeclaration of {
    isInterface:bool; 
    annotations: annotation list;
    name: variable;
    extended_types: ctype list;
    implemented_types: ctype list;
    body: term list} 
| MethodDeclaration of method0
| RawClass of variable *raw 
| TemplateClass of raw 
| Stmt of stmt 

(** Akka structure *)
| Actor of actor
| Event of event
and term = _term placed

(**  Main function*)
and entrypoint = expr list
(** Akka actor system *)
and system =  actor  
and program = {
    entrypoint: entrypoint;
    system: system;
    terms: term list}

[@@deriving show { with_path = false }]

(* No issues with binders since atom are uniques !!  No clash
Binders can be rename*)

let rec apply_rename_place (apply_rename : Core.Error.place -> 'a -> 'a) ({ Core.AstUtils.place ; Core.AstUtils.value}: 'a Core.AstUtils.placed) = 
    let value = apply_rename place value in
    {Core.AstUtils.place; Core.AstUtils.value}
let rec _apply_rename_ctype (renaming : Atom.atom -> Atom.atom) place : _ctype -> _ctype = function
    | Atomic s -> Atomic s
    | Behavior s -> Behavior s
    | ActorRef ct -> ActorRef (apply_rename_ctype renaming ct)
    | TFunction (ct1, ct2) -> TFunction (
        apply_rename_ctype renaming ct1,
        apply_rename_ctype renaming ct2
    )
    | TList ct -> TList (
        apply_rename_ctype renaming ct
    )
    | TMap (ct1, ct2) -> TMap (
        apply_rename_ctype renaming ct1,
        apply_rename_ctype renaming ct2
    )
    | TOption ct -> TOption (
        apply_rename_ctype renaming ct
    )
    | TResult (ct1, ct2) -> TResult (
        apply_rename_ctype renaming ct1,
        apply_rename_ctype renaming ct2
    )
    | TSet ct -> TSet (
        apply_rename_ctype renaming ct
    )
    | TTuple  cts -> TTuple (
        List.map (apply_rename_ctype renaming) cts
    ) 
    | TVar x -> TVar (renaming x) 
    | TVoid -> TVoid
    | TParam (ct, cts) -> TParam (
        apply_rename_ctype renaming ct,
        List.map (apply_rename_ctype renaming) cts
    )
    | TRaw s -> TRaw s
and apply_rename_ctype renaming (ct:ctype) = apply_rename_place (_apply_rename_ctype renaming) ct

and _apply_rename_expr (renaming : Atom.atom -> Atom.atom) place = function
    | AccessExpr (e1, e2) -> AccessExpr (
        apply_rename_expr renaming e1,
        apply_rename_expr renaming e2
    )
    | AssertExpr e -> AssertExpr (
        apply_rename_expr renaming e
    ) 
    | BinopExpr (e1, binop, e2) -> BinopExpr (
        apply_rename_expr renaming e1,
        binop,
        apply_rename_expr renaming e1
    ) 
    | CallExpr (e,es) -> CallExpr (
        apply_rename_expr renaming e,
        List.map (apply_rename_expr renaming) es
    )
    | LambdaExpr (xs, stmt) -> LambdaExpr (
        List.map renaming xs, (* binders can be renamed, see comments at the begining *)
        apply_rename_stmt renaming stmt
    ) 
    | LitExpr l -> LitExpr l
    | This -> This
    | UnopExpr (unop, e) -> UnopExpr (
        unop, 
        apply_rename_expr renaming e
    )         
    | VarExpr x -> VarExpr (renaming  x)                   
    | NewExpr (e, es) -> NewExpr (apply_rename_expr renaming e, List.map (apply_rename_expr renaming) es)                   
    | ClassOf ct -> ClassOf (apply_rename_ctype renaming ct)

    | BlockExpr (b, es) -> BlockExpr (
        b,
        List.map (apply_rename_expr renaming) es
    ) 
    | Spawn s -> Spawn {
        context = apply_rename_expr renaming s.context;
        actor_expr = apply_rename_expr renaming s.actor_expr }
    | CurrentContext -> CurrentContext
    | CurrentSystem -> CurrentSystem
    | RawExpr s -> RawExpr s 
and apply_rename_expr renaming e = apply_rename_place (_apply_rename_expr renaming) e

and _apply_rename_stmt (renaming : Atom.atom -> Atom.atom) place = function
    | AssignExpr (e1, e2) -> AssignExpr (apply_rename_expr renaming e1, apply_rename_expr renaming e2)
    | BlockStmt stmts -> BlockStmt (List.map (apply_rename_stmt renaming) stmts)
    | BreakStmt -> BreakStmt
    | EmptyStmt -> EmptyStmt
    | LetStmt (ct, x, e_opt) -> LetStmt (
        apply_rename_ctype renaming ct,
        renaming x, (* binder can be rename see comments at the begining*)
        Option.map (apply_rename_expr renaming) e_opt
    )
    | CommentsStmt c -> CommentsStmt c
    | ContinueStmt -> ContinueStmt
    | ExpressionStmt e -> ExpressionStmt (apply_rename_expr renaming e)
    | IfStmt (e, stmt1, stmt2_opt) -> IfStmt (
        apply_rename_expr renaming e,
        apply_rename_stmt renaming stmt1,
        Option.map (apply_rename_stmt renaming) stmt2_opt    
    )
    | ReturnStmt e -> ReturnStmt (apply_rename_expr renaming e)
and apply_rename_stmt renaming stmt = apply_rename_place (_apply_rename_stmt renaming) stmt 

and apply_rename_method0_body (renaming : Atom.atom -> Atom.atom) = function
| AbstractImpl stmts -> AbstractImpl (List.map (apply_rename_stmt renaming) stmts) 
| BBImpl bbterm -> BBImpl bbterm

and _apply_rename_method0 (renaming : Atom.atom -> Atom.atom) place m0 =
    {
        annotations = m0.annotations;
        ret_type =  apply_rename_ctype renaming m0.ret_type;
        name = renaming m0.name;
        body = apply_rename_method0_body renaming m0.body;
        args = List.map (function (ct, x) -> (
            apply_rename_ctype renaming ct,
            renaming x
        )) m0.args;
        is_constructor = m0.is_constructor
}
and apply_rename_method0 renaming m0 = apply_rename_place (_apply_rename_method0 renaming) m0 

and _apply_rename_event (renaming : Atom.atom -> Atom.atom) place e =
{
    vis = e.vis;
    name = renaming e.name;
    kind = e.kind; 
    args = List.map (function (ct, x) ->
        apply_rename_ctype renaming ct,
        renaming x
    ) e.args
}
and apply_rename_event renaming e = apply_rename_place (_apply_rename_event renaming) e 

and _apply_rename_state (renaming : Atom.atom -> Atom.atom) place s =
{
    persistent = s.persistent;
    stmts = List.map (apply_rename_stmt renaming) s.stmts
}
and apply_rename_state renaming s = apply_rename_place (_apply_rename_state renaming) s 

and _apply_rename_actor (renaming : Atom.atom -> Atom.atom) place (a:_actor) =
{
    name = renaming a.name;
    methods = List.map (apply_rename_method0 renaming) a.methods;
    receiver = Option.map (apply_rename_method0 renaming) a.receiver;
    states = List.map (apply_rename_state renaming) a.states;
    events = List.map (apply_rename_event renaming) a.events; 
    nested_items = List.map (apply_rename_term renaming) a.nested_items;
}
and apply_rename_actor renaming a = apply_rename_place (_apply_rename_actor renaming) a 

and _apply_rename_term (renaming : Atom.atom -> Atom.atom ) place = function
| Actor a -> Actor (apply_rename_actor renaming a)
| Class x -> Class (renaming x) 
| ClassOrInterfaceDeclaration cdcl -> ClassOrInterfaceDeclaration
    {
        isInterface = cdcl.isInterface; 
        annotations = cdcl.annotations;
        name = renaming cdcl.name;
        extended_types = List.map (apply_rename_ctype renaming) cdcl.extended_types;
        implemented_types = List.map (apply_rename_ctype renaming) cdcl.implemented_types;
        body = List.map (apply_rename_term renaming) cdcl.body; 
    }
| Comments c -> Comments c 
| Event e -> Event (apply_rename_event renaming e)
| Import s -> Import s 
| MethodDeclaration m0 -> MethodDeclaration (apply_rename_method0 renaming m0)
| RawClass (x,raw) -> RawClass (renaming x, raw)
| Stmt stmt -> Stmt (apply_rename_stmt renaming stmt)
| TemplateClass raw -> TemplateClass raw 
and apply_rename_term renaming t = apply_rename_place (_apply_rename_term renaming) t 