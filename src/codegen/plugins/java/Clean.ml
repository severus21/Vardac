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

let rec clean_expr place (e, jtype): _expr * jtype = 
let fplace = place@(Error.forge_place "Plg=Akka/clean_expr" 0 0) in
let auto_place smth = {place = fplace; value=smth} in
(match e with 
    | AccessExpr (e1, e2) -> AccessExpr (cexpr e1, cexpr e2)
    | AccessMethod (e, x) -> AccessMethod (cexpr e, x)
    | AppExpr (e, es) as e0 -> begin 
        match (snd e.value).value with
            | ClassOrInterfaceType  ({value=TAtomic "Function"}, targs) -> begin
                assert(List.length targs > 0);
                let t_ret = List.nth targs (List.length targs - 1) in

                match fst e.value with
                | LambdaExpr ([(t1, _)], _) ->
                    AppExpr(
                        auto_place(AccessExpr(
                            auto_place(CastExpr(
                                auto_place(ClassOrInterfaceType  ( auto_place (TAtomic "Function"), [t1; t_ret])),
                                e
                            ), snd (e.value)),
                            auto_place (RawExpr "apply", auto_place TUnknown)
                        ), auto_place TUnknown),
                        es
                    )
                | LambdaExpr ([(t1, _); (t2,_)], _) ->

                    (* t_ret must be rewritten to erase t2 since we group in BiFunction 
                    because, in Java, 
                        type a -> b -> c is not the same than a-> (b -> c)
                        and Varda is working with curryfied types for now
                        however we can not curryfied appexpr since there is no partial function/method in Java .....

                        Our transformation encoding works for lambda with one or two args only
                    *)

                    let t_ret = match t_ret.value with
                        | ClassOrInterfaceType  ({value=TAtomic "Function"}, _::[t]) -> t
                        | ClassOrInterfaceType  ({place; value=TAtomic "Function"}, _::targs) -> 
                            {
                                place= t_ret.place; value=ClassOrInterfaceType  ({place; value=TAtomic "Function"}, targs)
                            }
                    in

                    AppExpr(
                        auto_place(AccessExpr(
                            auto_place(CastExpr(
                                auto_place(ClassOrInterfaceType  ( auto_place (TAtomic "BiFunction"), [t1; t2; t_ret])),
                                e
                            ), snd (e.value)),
                            auto_place (RawExpr "apply", auto_place TUnknown)
                        ), auto_place TUnknown),
                        es
                    )
            | LambdaExpr _ -> failwith "LambdaExpr with more than two args are not yet supported (Akka Plg)"
            | _ -> 
                AppExpr (cexpr e, List.map cexpr es)
            end
            | _ -> AppExpr (cexpr e, List.map cexpr es)
    end
    | AssertExpr e -> AssertExpr (cexpr e)
    | AssignExpr (e1, op, e2) -> AssignExpr (cexpr e1, op, cexpr e2)
    | BinaryExpr (e1, AstUtils.StructuralEqual, e2) -> 
        AppExpr ( 
            auto_place (AccessExpr (
                cexpr e1, 
                auto_place (VarExpr (Atom.builtin "equals"), auto_place TUnknown)
            ), auto_place TUnknown),
            [cexpr e2]
        ) 
    | BinaryExpr (e1, op, e2) -> BinaryExpr (cexpr e1, op, cexpr e2)
    | CastExpr (ct, e) -> CastExpr (ct, cexpr e)
    | LiteralExpr l -> LiteralExpr l
    | LambdaExpr (xs, stmt) -> LambdaExpr (xs, cstmt stmt) 
    | NewExpr (e, es) -> NewExpr (cexpr e, List.map cexpr es)
    | ThisExpr -> ThisExpr 
    | UnaryExpr (op, e) -> UnaryExpr (op, cexpr e)
    | VarExpr x -> VarExpr x
    | RawExpr s -> RawExpr s
    | TernaryExpr (e1, e2, e3) -> TernaryExpr(cexpr e1, cexpr e2, cexpr e3)
), jtype
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
| BBStmt bb -> BBStmt bb
| TryStmt  (stmt, branches) -> TryStmt (
    cstmt stmt,
    List.map (function (ct, x, stmt)->
        ct,
        x,
        cstmt stmt
    ) branches
)
and cstmt e = map_place clean_stmt e 

and clean_body_v place : _body -> _body = 
    let fplace = place@(Error.forge_place "Plg=Akka/clean_body_v" 0 0) in
    let auto_fplace smth = {place = fplace; value=smth} in
function 
| ClassOrInterfaceDeclaration cid ->
    ClassOrInterfaceDeclaration { cid with 
        body = List.map citem cid.body
    }
| MethodDeclaration m0 -> begin
    let rec search_return flag stmt = 
        flag || match stmt.value with
        | BreakStmt | CommentsStmt _ | ContinueStmt | ExpressionStmt _ | EmptyStmt _ |NamedExpr _ | RawStmt _ -> false 
        | BlockStmt stmts -> List.fold_left search_return flag stmts
        | IfStmt (_, stmt1, stmt2_opt) ->
            (search_return flag stmt1) ||
            (match Option.map (search_return flag) stmt2_opt with | Some f -> f | None -> flag)
        | ReturnStmt _ -> true
        | ForStmt (_,_,_,stmt) -> search_return flag stmt 
        | TryStmt (stmt, branches) ->
            search_return flag stmt || (
                List.fold_left (fun flag (_,_,stmt) -> flag || search_return flag stmt) flag branches
            )
    in

    let body = match m0.ret_type with
        | Some {value=TAtomic "Void"} ->
            if (List.fold_left search_return false m0.body) then
                m0.body
            else ( 
                (* Add a return since Void needs a return *)
                m0.body @ [
                    auto_fplace (ReturnStmt (auto_fplace(RawExpr "null", auto_fplace (TAtomic "null"))))
                ]
            )
        | _ -> m0.body
    in
    let body = List.map cstmt body in

    MethodDeclaration { m0 with body; }
end
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

(*****************************************************)
module Make(Arg: sig val filename:string end) = struct
    let displayed_pass_shortdescription = Printf.sprintf "Cleaned Lg AST for file %s" Arg.filename
    let displayed_ast_name = "IR recvelim"
    let show_ast = true
    let precondition program = program
    let postcondition program = program
    let apply_program = clean_program
end