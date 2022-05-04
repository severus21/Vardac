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
    | TActivationRef of ctype
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
    | TBB of blackbox_term
    | TUnknown 
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
    | ActivationRef of {schema: expr; actor_ref: expr;}
    | InterceptedActivationRef of {actor_ref: expr; intercepted_actor_ref: expr option}
    | AssertExpr of expr 
    | BinopExpr of expr * binop * expr 
    | CallExpr of expr * expr list
    | CastExpr of ctype * expr
    | LambdaExpr of (ctype * variable) list * stmt 
    | LitExpr of literal
    | This
    | UnopExpr of unop * expr         
    | VarExpr of variable                    
    | NewExpr of expr * expr list                   

    | ClassOf of ctype 

    | BlockExpr of IR.block * expr list
    | Block2Expr of IR.block2 * (expr * expr) list

    | TernaryExpr of expr * expr * expr

    (** Akka scheduler *)
    | Spawn of { context: expr; actor_expr: expr }
    | CurrentContext
    | CurrentSystem
    | RawExpr of string
    | BBExpr of blackbox_term
and expr = (_expr * ctype) placed

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
    | ForStmt of ctype * variable * expr * stmt
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

and blackbox_body = 
| Text of string
| Varda of expr 

and _blackbox_term = {
    language: string option;
    body: blackbox_body list;
} 

and blackbox_term = _blackbox_term placed

and method0_body = 
| AbstractImpl of stmt list
| BBImpl of blackbox_term

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
    static_items: term list; (* class (not actor), typedef *)
    imports: string list (*specific imports*)
}
and actor = _actor placed
(************************************ Akka streams **********************************)
(* TODO *)

(************************************ Akka program **********************************)
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
    body: term list;
    imports: string list (*specific imports*)
} 
| MethodDeclaration of method0
| RawClass of variable * blackbox_term 
| RawTerm of blackbox_term
| Stmt of stmt 

(** Akka structure *)
| Actor of actor
| Event of event
and term = (_term annotated) placed

(**  Main function*)
and entrypoint = expr list
(** Akka actor system *)
and system =  unit  
and program = term list

[@@deriving show { with_path = false }]
