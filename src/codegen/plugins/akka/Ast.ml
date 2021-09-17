(** Underlying programming language primitives *)
open Core
open AstUtils

(************************************* Base types ****************************)
open Label

type variable = Atom.atom

and comments = Core.IR._comments

(************************************ Types **********************************)
and _ctype = 
    | Atomic of string (*int, float, ..*)           
    | Behavior of string 
    | ActorRef of ctype
    | TFunction of ctype * ctype
    | TList of ctype 
    | TMap of ctype * ctype (*Map<String, String> map = new HashMap<String, String>();*)
    | TOption of ctype (*https://www.touilleur-express.fr/2014/11/07/optional-en-java-8/*)
    | TResult of ctype * ctype (*https://github.com/hdevalke/result4j*)
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
    | EmptyLit 
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

    | ClassOf of ctype 

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
