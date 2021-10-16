(** Underlying programming language primitives *)
open Core
open AstUtils

(************************************* Base types ****************************)
open Label

type variable = Atom.atom

and comments = Core.AstUtils._comments

(************************************ Types **********************************)
and _ctype = 
    | Atomic of string (*void, Void, int, float, ?, ..*)           
    | ActorRef of ctype
    | TFunction of ctype * ctype
    | TArray of ctype 
    | TList of ctype 
    | TMap of ctype * ctype (*Map<String, String> map = new HashMap<String, String>();*)
    | TOption of ctype (*https://www.touilleur-express.fr/2014/11/07/optional-en-java-8/*)
    | TResult of ctype * ctype
    | TSet of ctype (*Set<String> set =  new HashSet<String>() ;*)
    | TTuple of ctype list 
    | TVar of variable

    | TAccess of ctype * ctype (* t1.t2 *)
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
    | AccessMethod of expr * variable
    | AssertExpr of expr 
    | BinopExpr of expr * binop * expr 
    | CallExpr of expr * expr list
    | CastExpr of ctype * expr
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
    | TryStmt of stmt * (ctype * variable * stmt) list
and stmt = _stmt placed

and visibility = 
    | Private
    | Protected
    | Public
and annotation =
    | Visibility of visibility
    | Static 
    | Final
and decorator = 
    | Override
and 'a annotated = {
    annotations: annotation list;
    decorators: decorator list;
    v: 'a;
}
and method0_body = 
| AbstractImpl of stmt list
| BBImpl of Impl_common.blackbox_term

and _method0 = {
    ret_type: ctype;
    name: variable;
    body: method0_body;
    args: (ctype * variable) list;
    is_constructor: bool
}
and method0 = (_method0 annotated) placed
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
    is_guardian: bool;
    extended_types: ctype list;
    implemented_types: ctype list;
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
    receiver: method0;
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
and term = (_term annotated) placed

(**  Main function*)
and entrypoint = expr list
(** Akka actor system *)
and system =  unit  
and program = {
    entrypoint: entrypoint;
    system: system;
    terms: term list}

[@@deriving show { with_path = false }]

let rec apply_rename_place (apply_rename : Core.Error.place -> 'a -> 'a) ({ Core.AstUtils.place ; Core.AstUtils.value}: 'a Core.AstUtils.placed) = 
    let value = apply_rename place value in
    {Core.AstUtils.place; Core.AstUtils.value}
let rec _apply_rename_ctype (renaming : Atom.atom -> Atom.atom) place : _ctype -> _ctype = function
    | Atomic s -> Atomic s
    | ActorRef ct -> ActorRef (apply_rename_ctype renaming ct)
    | TFunction (ct1, ct2) -> TFunction (
        apply_rename_ctype renaming ct1,
        apply_rename_ctype renaming ct2
    )
    | TArray ct -> TArray (
        apply_rename_ctype renaming ct
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
    | TAccess (ct1, ct2) -> TAccess (
        apply_rename_ctype renaming ct1,
        apply_rename_ctype renaming ct2
    )
    | TParam (ct, cts) -> TParam (
        apply_rename_ctype renaming ct,
        List.map (apply_rename_ctype renaming) cts
    )
    | TRaw s -> TRaw s
and apply_rename_ctype renaming (ct:ctype) = apply_rename_place (_apply_rename_ctype renaming) ct

and _apply_rename_expr rename_binders (renaming : Atom.atom -> Atom.atom) place = function
    | AccessExpr (e1, e2) -> AccessExpr (
        apply_rename_expr rename_binders renaming e1,
        apply_rename_expr rename_binders renaming e2
    )
    | AccessMethod (e1, x) -> AccessMethod (
        apply_rename_expr rename_binders renaming e1,
        renaming x 
    )
    | AssertExpr e -> AssertExpr (
        apply_rename_expr rename_binders renaming e
    ) 
    | BinopExpr (e1, binop, e2) -> BinopExpr (
        apply_rename_expr rename_binders renaming e1,
        binop,
        apply_rename_expr rename_binders renaming e2
    ) 
    | CallExpr (e,es) -> CallExpr (
        apply_rename_expr rename_binders renaming e,
        List.map (apply_rename_expr rename_binders renaming) es
    )
    | CastExpr (ct, e) -> CastExpr (
        apply_rename_ctype renaming ct,
        apply_rename_expr rename_binders renaming e
    )
    | LambdaExpr (xs, stmt) -> LambdaExpr (
        (if rename_binders then List.map renaming xs else xs),
        apply_rename_stmt rename_binders renaming stmt
    ) 
    | LitExpr l -> LitExpr l
    | This -> This
    | UnopExpr (unop, e) -> UnopExpr (
        unop, 
        apply_rename_expr rename_binders renaming e
    )         
    | VarExpr x -> VarExpr (renaming  x)                   
    | NewExpr (e, es) -> NewExpr (apply_rename_expr rename_binders renaming e, List.map (apply_rename_expr rename_binders renaming) es)                   
    | ClassOf ct -> ClassOf (apply_rename_ctype renaming ct)

    | BlockExpr (b, es) -> BlockExpr (
        b,
        List.map (apply_rename_expr rename_binders renaming) es
    ) 
    | Spawn s -> Spawn {
        context = apply_rename_expr rename_binders renaming s.context;
        actor_expr = apply_rename_expr rename_binders renaming s.actor_expr }
    | CurrentContext -> CurrentContext
    | CurrentSystem -> CurrentSystem
    | RawExpr s -> RawExpr s 
and apply_rename_expr rename_binders renaming e = apply_rename_place (_apply_rename_expr rename_binders renaming) e

and _apply_rename_stmt rename_binders (renaming : Atom.atom -> Atom.atom) place = function
    | AssignExpr (e1, e2) -> AssignExpr (apply_rename_expr rename_binders renaming e1, apply_rename_expr rename_binders renaming e2)
    | BlockStmt stmts -> BlockStmt (List.map (apply_rename_stmt rename_binders renaming) stmts)
    | BreakStmt -> BreakStmt
    | EmptyStmt -> EmptyStmt
    | LetStmt (ct, x, e_opt) -> LetStmt (
        apply_rename_ctype renaming ct,
        (if rename_binders then renaming x else x),
        Option.map (apply_rename_expr rename_binders renaming) e_opt
    )
    | CommentsStmt c -> CommentsStmt c
    | ContinueStmt -> ContinueStmt
    | ExpressionStmt e -> ExpressionStmt (apply_rename_expr rename_binders renaming e)
    | IfStmt (e, stmt1, stmt2_opt) -> IfStmt (
        apply_rename_expr rename_binders renaming e,
        apply_rename_stmt rename_binders renaming stmt1,
        Option.map (apply_rename_stmt rename_binders renaming) stmt2_opt    
    )
    | ReturnStmt e -> ReturnStmt (apply_rename_expr rename_binders renaming e)
    | TryStmt (stmt, branches) -> TryStmt (
        apply_rename_stmt rename_binders renaming stmt,
        List.map (function (ct, x, stmt)->
            apply_rename_ctype renaming ct,
            renaming x,
            apply_rename_stmt rename_binders renaming stmt
        ) branches
    )
and apply_rename_stmt rename_binders renaming stmt = apply_rename_place (_apply_rename_stmt rename_binders renaming) stmt 

and apply_rename_method0_body rename_binders (renaming : Atom.atom -> Atom.atom) = function
| AbstractImpl stmts -> AbstractImpl (List.map (apply_rename_stmt rename_binders renaming) stmts) 
| BBImpl bbterm -> BBImpl bbterm

and _apply_rename_method0 rename_binders (renaming : Atom.atom -> Atom.atom) place m0 =
    {
        decorators = m0.decorators;
        annotations = m0.annotations;
        v = {
            ret_type =  apply_rename_ctype renaming m0.v.ret_type;
            name = (if rename_binders then renaming m0.v.name else m0.v.name);
            body = apply_rename_method0_body rename_binders renaming m0.v.body;
            args = List.map (function (ct, x) -> (
                apply_rename_ctype renaming ct,
                renaming x
            )) m0.v.args;
            is_constructor = m0.v.is_constructor
        }
}
and apply_rename_method0 rename_binders renaming m0 = apply_rename_place (_apply_rename_method0 rename_binders renaming) m0 

and _apply_rename_event rename_binders (renaming : Atom.atom -> Atom.atom) place e =
{
    vis = e.vis;
    name = if rename_binders then renaming e.name else e.name;
    kind = e.kind; 
    args = List.map (function (ct, x) ->
        apply_rename_ctype renaming ct,
        renaming x
    ) e.args
}
and apply_rename_event rename_binders renaming e = apply_rename_place (_apply_rename_event rename_binders renaming) e 

and _apply_rename_state rename_binders (renaming : Atom.atom -> Atom.atom) place s =
{
    persistent = s.persistent;
    stmts = List.map (apply_rename_stmt rename_binders renaming) s.stmts
}
and apply_rename_state rename_binders renaming s = apply_rename_place (_apply_rename_state rename_binders renaming) s 

and _apply_rename_actor rename_binders (renaming : Atom.atom -> Atom.atom) place (a:_actor) =
{
    is_guardian = a.is_guardian;
    extended_types = List.map (apply_rename_ctype renaming) a.extended_types;
    implemented_types = List.map (apply_rename_ctype renaming) a.implemented_types;
    name = if rename_binders then renaming a.name else a.name;
    methods = List.map (apply_rename_method0 rename_binders renaming) a.methods;
    receiver = (apply_rename_method0 rename_binders renaming) a.receiver;
    states = List.map (apply_rename_state rename_binders renaming) a.states;
    events = List.map (apply_rename_event rename_binders renaming) a.events; 
    nested_items = List.map (apply_rename_term rename_binders renaming) a.nested_items;
}
and apply_rename_actor rename_binders renaming a = apply_rename_place (_apply_rename_actor rename_binders renaming) a 

and _apply_rename_term rename_binders (renaming : Atom.atom -> Atom.atom ) place {annotations; decorators;v} = 
{
    annotations = annotations;
    decorators = decorators;
    v = match v with
        | Actor a -> Actor (apply_rename_actor rename_binders renaming a)
        | Class x -> Class (renaming x) 
        | ClassOrInterfaceDeclaration cdcl -> ClassOrInterfaceDeclaration
            {
                isInterface = cdcl.isInterface; 
                name = if rename_binders then renaming cdcl.name else cdcl.name;
                extended_types = List.map (apply_rename_ctype renaming) cdcl.extended_types;
                implemented_types = List.map (apply_rename_ctype renaming) cdcl.implemented_types;
                body = List.map (apply_rename_term rename_binders renaming) cdcl.body; 
            }
        | Comments c -> Comments c 
        | Event e -> Event (apply_rename_event rename_binders renaming e)
        | Import s -> Import s 
        | MethodDeclaration m0 -> MethodDeclaration (apply_rename_method0 rename_binders renaming m0)
        | RawClass (x,raw) -> RawClass (renaming x, raw)
        | Stmt stmt -> Stmt (apply_rename_stmt rename_binders renaming stmt)
        | TemplateClass raw -> TemplateClass raw 
}
and apply_rename_term rename_binders renaming t = apply_rename_place (_apply_rename_term rename_binders renaming) t 

let rec _replaceterm_term (rename_binders:bool) selector replace place : _term annotated -> _term annotated = function
| t when selector t -> replace t 
| {annotations; decorators; v=Actor a} -> 
    {    
        annotations;
        decorators;
        v = Actor {
            place = a.place; 
            value = {a.value with
                nested_items = List.map (replaceterm_term rename_binders selector replace) a.value.nested_items;
            }
        } 
    }
| {annotations; decorators; v = ClassOrInterfaceDeclaration cdcl} -> 
    {    
        annotations;
        decorators;
        v = ClassOrInterfaceDeclaration {cdcl with 
        body = List.map (replaceterm_term rename_binders selector replace) cdcl.body; 
        }
    }
| t -> t
and replaceterm_term (rename_binders:bool) (selector : _term annotated -> bool) (replace : _term annotated-> _term annotated) t = apply_rename_place (_replaceterm_term rename_binders selector replace) t


let rec apply_rewrite_place (apply_rewrite : Core.Error.place -> 'a -> 'a) ({ Core.AstUtils.place ; Core.AstUtils.value}: 'a Core.AstUtils.placed) = 
    let value = apply_rewrite place value in
    {Core.AstUtils.place; Core.AstUtils.value}

let rec _rewriteexpr_expr selector rewriter place : _expr -> _expr = function 
| e when selector e -> rewriter e
| AccessExpr (e1, e2) -> AccessExpr(
    rewriteexpr_expr selector rewriter e1,
    rewriteexpr_expr selector rewriter e2
)
| AccessMethod (e, x) -> AccessMethod (
    rewriteexpr_expr selector rewriter e,
    x
) 
| AssertExpr e -> AssertExpr (
    rewriteexpr_expr selector rewriter e
) 
| BinopExpr (e1, op, e2) -> BinopExpr (
    rewriteexpr_expr selector rewriter e1,
    op,
    rewriteexpr_expr selector rewriter e2
)
| CallExpr (e, es) -> CallExpr (
    rewriteexpr_expr selector rewriter e,
    List.map (rewriteexpr_expr selector rewriter) es
)
| LambdaExpr (xs, stmt) -> LambdaExpr (
    xs,
    rewriteexpr_stmt selector rewriter stmt
)
| LitExpr _ as e -> e
| This -> This
| UnopExpr (op, e) -> UnopExpr (
    op,
    rewriteexpr_expr selector rewriter e
)
| VarExpr _ as e -> e
| NewExpr (e, es) ->  NewExpr (
    rewriteexpr_expr selector rewriter e,
    List.map (rewriteexpr_expr selector rewriter) es
)
| ClassOf _ as e -> e
| BlockExpr (b, es) -> BlockExpr (
    b,
    List.map (rewriteexpr_expr selector rewriter) es
) 
| Spawn s -> Spawn {
    context = rewriteexpr_expr selector rewriter s.context;
    actor_expr = rewriteexpr_expr selector rewriter s.actor_expr;
}
| CurrentContext -> CurrentContext
| CurrentSystem -> CurrentSystem
| RawExpr _ as e -> e
and rewriteexpr_expr selector rewriter = apply_rewrite_place (_rewriteexpr_expr selector rewriter)
and _rewriteexpr_stmt selector rewriter place : _stmt -> _stmt = function
| AssignExpr (e1, e2) -> AssignExpr (
    rewriteexpr_expr selector rewriter e1,
    rewriteexpr_expr selector rewriter e2
    )
| BlockStmt stmts -> BlockStmt (List.map (rewriteexpr_stmt selector rewriter) stmts)
| BreakStmt as stmt -> stmt
| EmptyStmt as stmt -> stmt
| LetStmt (ct, x, e_opt) -> LetStmt (
    ct,
    x, 
    Option.map (rewriteexpr_expr selector rewriter) e_opt
)
| CommentsStmt _ as stmt -> stmt
| ContinueStmt as stmt -> stmt
| ExpressionStmt e -> ExpressionStmt (rewriteexpr_expr selector rewriter e)
| IfStmt (e, stmt1, stmt2_opt) -> IfStmt (
    rewriteexpr_expr selector rewriter e,
    rewriteexpr_stmt selector rewriter stmt1,
    Option.map (rewriteexpr_stmt selector rewriter) stmt2_opt
)
| ReturnStmt e -> ReturnStmt (rewriteexpr_expr selector rewriter e)
and rewriteexpr_stmt selector rewriter = apply_rewrite_place (_rewriteexpr_stmt selector rewriter)


