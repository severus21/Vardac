(* Docs: https://github.com/javaparser/javaparser/tree/master/javaparser-core/src/main/java/com/github/javaparser/ast*)

open Core
type unop = IR.unop 
and binop = IR.binop 
and assign_operator = 
    | AssignOp (* = *)
    (* TODO += etc if needed*)
and literal = 
    | EmptyLit
    | BoolLit of bool
    | FloatLit of float
    | IntLit of int
    | StringLit of string
and visibility = 
    | Private
    | Protected
    | Public
and annotation =
    | Visibility of visibility
    | Static 
    | Final
[@@deriving show { with_path = false }]

type variable =
    Atom.atom
and parameter = jtype * variable
and type_parameter = jtype
and body =
    | ClassOrInterfaceDeclaration of {
        isInterface:bool; 
        annotations: annotation list;
        name: variable;
        parameters: type_parameter list;
        extended_types:jtype list;
        implemented_types: jtype list;
        body: str_items list} (*parameters extends .... implements interfaces*)(* TODO GADT should guarantee that jtype is ClassOrInterfaceDeclaration*)
    | MethodDeclaration of { 
        annotations: annotation list;
        ret_type: jtype option; (* None for constructor, Some _ otherwise *)
        name: variable;
        parameters: parameter list;
        body: stmt 
    } (*GADT stmt of type blockstatemtn*)
and comments = Core.IR._comments
and expr = 
    | AccessExpr of expr * expr (*e1.e2*)
    | AppExpr of expr * expr list (*e1 e2*)
    | AssertExpr of expr 
    | AssignExpr of expr * assign_operator * expr (* target value*)
    | BinaryExpr of expr * binop * expr
    | LiteralExpr of literal
    | LambdaExpr of parameter list * expr 
    | ThisExpr
    | UnaryExpr of unop * expr
    | VarExpr of variable
and jmodule = 
    | ImportDirective of string
and stmt = 
    | BlockStmt of stmt list
    | BreakStmt 
    | CommentsStmt of comments
    | ContinueStmt
    | ExpressionStmt of expr
    | IfStmt of expr * stmt * stmt option              
    | NamedExpr of jtype * variable * expr 
    | ReturnStmt of expr
and jtype =
    | ClassOrInterfaceType of variable * jtype list (* name * args *) 
    | TAtomic of string
    | TVar of variable
and str_items =
    | Body of body
    | Comments of comments
    | JModule of jmodule
    | Stmt of stmt
    | JType of jtype
and program = str_items list


[@@deriving show { with_path = false }]
