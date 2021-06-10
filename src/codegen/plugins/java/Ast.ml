(* Docs: https://github.com/javaparser/javaparser/tree/master/javaparser-core/src/main/java/com/github/javaparser/ast*)

open Core
open AstUtils

type unop = IR.unop 
and binop = IR.binop 
and assign_operator = 
    | AssignOp (* = *)
    (* TODO += etc if needed*)
and _literal = 
    | EmptyLit
    | BoolLit of bool
    | FloatLit of float
    | IntLit of int
    | StringLit of string
and literal = _literal placed
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
and _body =
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
        body: stmt list
    } (*GADT stmt of type blockstatemtn*)
    | FieldDeclaration of { 
        annotations: annotation list;
        type0: jtype;
        name: variable;
        body: expr option 
    }
and body = _body placed
and comments = Core.IR._comments

and _expr = 
    | AccessExpr of expr * expr (*e1.e2*)
    | AppExpr of expr * expr list (*e1 e2*)
    | AssertExpr of expr 
    | AssignExpr of expr * assign_operator * expr (* target value*)
    | BinaryExpr of expr * binop * expr
    | LiteralExpr of literal
    | LambdaExpr of variable list * stmt 
    | ThisExpr
    | UnaryExpr of unop * expr
    | VarExpr of variable
    | RawExpr of string
and expr = _expr placed

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
    | IfStmt of expr * stmt * stmt option              
    | NamedExpr of jtype * variable * expr 
    | ReturnStmt of expr
    | RawStmt of string
and stmt = _stmt placed

and _jtype =
    | ClassOrInterfaceType of variable * jtype list (* name * args *) 
    | TAtomic of string
    | TVar of variable
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
