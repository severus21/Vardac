(* Docs: https://github.com/javaparser/javaparser/tree/master/javaparser-core/src/main/java/com/github/javaparser/ast*)

open Core
open AstUtils

type unop = IR.unop 
and binop = IR.binop 
and assign_operator = 
    | AssignOp (* = *)
    (* TODO += etc if needed*)
and _literal = 
    | BoolLit of bool
    | FloatLit of float
    | IntLit of int
    | StringLit of string
    | VoidLit
and literal = _literal placed
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
[@@deriving show { with_path = false }]

type variable =
    Atom.atom
and parameter = jtype * variable
and type_parameter = jtype
and _body =
    | ClassOrInterfaceDeclaration of {
        isInterface:bool; 
        name: variable;
        parameters: type_parameter list;
        extended_types:jtype list;
        implemented_types: jtype list;
        body: str_items list} (*parameters extends .... implements interfaces*)(* TODO GADT should guarantee that jtype is ClassOrInterfaceDeclaration*)
    | MethodDeclaration of { 
        ret_type: jtype option; (* None for constructor, Some _ otherwise *)
        name: variable;
        parameters: parameter list;
        body: stmt list
    } (*GADT stmt of type blockstatemtn*)
    | FieldDeclaration of { 
        type0: jtype;
        name: variable;
        body: expr option 
    }
and body = (_body annotated) placed
and comments = AstUtils._comments

and _expr = 
    | AccessExpr of expr * expr (*e1.e2*)
    | AccessMethod of expr * variable (*e1::m*)
    | AppExpr of expr * expr list (*e1 e2*)
    | AssertExpr of expr 
    | AssignExpr of expr * assign_operator * expr (* target value*)
    | BinaryExpr of expr * binop * expr
    | CastExpr of jtype * expr
    | LiteralExpr of literal
    | LambdaExpr of variable list * stmt 
    | NewExpr of expr * expr list
    | ThisExpr
    | UnaryExpr of unop * expr
    | VarExpr of variable
    | RawExpr of string
and expr = (_expr * jtype) placed

and _jmodule = 
    | ImportDirective of string
    | PackageDeclaration of string
and jmodule = _jmodule placed

and _stmt = 
    | BlockStmt of stmt list
    | BreakStmt 
    | CommentsStmt of comments
    | ContinueStmt
    | ExpressionStmt of expr
    | EmptyStmt 
    | IfStmt of expr * stmt * stmt option              
    | ForStmt of jtype * variable * expr * stmt
    | NamedExpr of jtype * variable * expr option 
    | ReturnStmt of expr
    | RawStmt of string
    | TryStmt of stmt * (jtype * variable * stmt) list
and stmt = _stmt placed

and _jtype =
    | ClassOrInterfaceType of jtype * jtype list (* name * args *) 
    | TAtomic of string
    | TVar of variable
    | TAccess of jtype * jtype
    | TArray of jtype (* string[] *)
    | TUnknown
and jtype = _jtype placed

and _str_items =
    | Body of body
    | Comments of comments
    | JModule of jmodule
    | Stmt of stmt
    | JType of jtype
    | Raw of string
and str_items = _str_items placed

and program = str_items list
[@@deriving show { with_path = false }]
