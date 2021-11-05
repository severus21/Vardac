open Ast
open Core
open Core.AstUtils
open Easy_logging

let logger = Logging.make_logger ("_1_ compspec.plg.Java") Debug [];;

module AnnotationOrder = struct
  type t = annotation
  let compare = compare
end
module DecoratorOrder = struct
  type t = decorator
  let compare = compare
end
module SAnnotation = Set.Make(AnnotationOrder)
module SDecorator = Set.Make(DecoratorOrder)

let deduplicate_annotations annots = 
    SAnnotation.elements (SAnnotation.of_list annots)

let deduplicate_decorators dcs = 
SDecorator.elements (SDecorator.of_list dcs)

let clean_return_type stmts ret_type = 
    match ret_type.value with
    | TAtomic "Void" ->
        let rec search_return flag stmt = 
            match stmt.value with
            | BlockStmt stmts -> List.fold_left search_return flag stmts
            | IfStmt (_, stmt1, stmt2_opt) ->
                (search_return flag stmt1) ||
                (match Option.map (search_return flag) stmt2_opt with | Some f -> f | None -> flag)
            | ReturnStmt _ -> true
            | _ -> flag
        in

        let flag = List.fold_left search_return false stmts in
        if flag then ret_type else {ret_type with value = TAtomic "void"}
    | _ -> ret_type


let rec clean_expr place : _expr -> _expr = function
| AccessExpr (e1, e2) -> AccessExpr (cexpr e1, cexpr e2)
| AccessMethod (e, x) -> AccessMethod (cexpr e, x)
| AppExpr (e, es) -> AppExpr (cexpr e, List.map cexpr es)
| AssertExpr e -> AssertExpr (cexpr e)
| AssignExpr (e1, op, e2) -> AssignExpr (cexpr e1, op, cexpr e2)
| BinaryExpr (e1, AstUtils.StructuralEqual, e2) -> 
    AppExpr ( {place; value = AccessExpr (cexpr e1, {place; value = VarExpr (Atom.fresh_builtin "equals")})}, [cexpr e2]) 
| BinaryExpr (e1, op, e2) -> BinaryExpr (cexpr e1, op, cexpr e2)
| CastExpr (ct, e) -> CastExpr (ct, cexpr e)
| LiteralExpr l -> LiteralExpr l
| LambdaExpr (xs, stmt) -> LambdaExpr (xs, cstmt stmt) 
| NewExpr (e, es) -> NewExpr (cexpr e, List.map cexpr es)
| ThisExpr -> ThisExpr 
| UnaryExpr (op, e) -> UnaryExpr (op, cexpr e)
| VarExpr x -> VarExpr x
| RawExpr s -> RawExpr s
and cexpr e = map_place clean_expr e 

and clean_stmt place : _stmt -> _stmt = function
| BlockStmt stmts -> BlockStmt (List.map cstmt stmts)
| BreakStmt -> BreakStmt
| CommentsStmt c -> CommentsStmt c 
| ContinueStmt -> ContinueStmt
| ExpressionStmt e -> ExpressionStmt (cexpr e)
| EmptyStmt -> EmptyStmt
| IfStmt (e, stmt1, stmt2_opt) -> IfStmt (
    cexpr e,
    cstmt stmt1,
    Option.map cstmt stmt2_opt
)              
| ForStmt(ct, x, e, stmt) -> ForStmt (
    ct,
    x, 
    cexpr e,
    cstmt stmt
)
| NamedExpr (t, x, e_opt) -> NamedExpr (t, x, Option.map cexpr e_opt)
| ReturnStmt e -> ReturnStmt (cexpr e) 
| RawStmt s -> RawStmt s 
| TryStmt  (stmt, branches) -> TryStmt (
    cstmt stmt,
    List.map (function (ct, x, stmt)->
        ct,
        x,
        cstmt stmt
    ) branches
)
and cstmt e = map_place clean_stmt e 

and clean_body_v place : _body -> _body = function 
| ClassOrInterfaceDeclaration cid ->
    ClassOrInterfaceDeclaration { cid with 
        body = List.map citem cid.body
    }
| MethodDeclaration m0 ->
    MethodDeclaration { m0 with
        ret_type = Option.map (clean_return_type m0.body) m0.ret_type;
        body = List.map cstmt m0.body;
    }
| FieldDeclaration fdcl -> 
    FieldDeclaration { fdcl with
        body = Option.map cexpr fdcl.body;
    } 
and clean_body place {annotations; decorators; v} = { 
    annotations = deduplicate_annotations annotations;
    decorators = deduplicate_decorators decorators;
    v = clean_body_v place v;
}
and cbody b = map_place clean_body b 
    
and clean_item place = function
| Body b -> Body (cbody b) 
| Stmt stmt -> Stmt (cstmt stmt)
| item -> item 
and citem item =  map_place clean_item item 

let clean_program program = List.map citem program 