open Ast
open Core
open Core.AstUtils
open Easy_logging


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


(*****************************************************)
module Make(Arg: sig val filename:string val toplevel_functions:Atom.Set.t end) = struct
    let logger = Core.Utils.make_log_of "Java.Clean"

    let name = "Java.Clean"

    (*****************************************************)

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
            match fst e.value with
            | LambdaExpr ([(t1, _)] as params, stmt_lambda) ->
                let t_ret = match (snd e.value).value with
                    | ClassOrInterfaceType  ({value=TAtomic "Function"}, targs) ->
                        List.nth targs (List.length targs - 1)
                    | TUnknown -> begin
                        (* TODO Infer it here (hack), we should port the TypeInference path both on IR and IRI and run it after prepare and future elim in Akka plg *)
                        let rec find_return_ place = function
                        | ReturnStmt e -> [e]
                        | BlockStmt stmts -> List.flatten(List.map find_return stmts)
                        and find_return stmt = map0_place find_return_ stmt in
                        let e_lambda = List.hd (find_return stmt_lambda) in

                        match (JavaUtils.jtype_of_lambda params e_lambda).value with 
                        | ClassOrInterfaceType  ({value=TAtomic "Function"}, targs) ->
                        List.nth targs (List.length targs - 1)
                    end

                in

                AppExpr(
                    auto_place(AccessExpr(
                        auto_place(CastExpr(
                            auto_place(ClassOrInterfaceType  ( auto_place (TAtomic "Function"), [t1; t_ret])),
                            cexpr e
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

                let t_ret = match (snd e.value).value with
                    | ClassOrInterfaceType  ({value=TAtomic "Function"}, targs) ->
                        List.nth targs (List.length targs - 1)
                in
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
                            cexpr e
                        ), snd (e.value)),
                        auto_place (RawExpr "apply", auto_place TUnknown)
                    ), auto_place TUnknown),
                    es
                )
            | LambdaExpr _ -> failwith "LambdaExpr with more than two args are not yet supported (Akka Plg)"
            | VarExpr x when Atom.Set.find_opt x Arg.toplevel_functions = None -> begin 
                match (snd e.value).value with
                | ClassOrInterfaceType  ({value=TAtomic "Function"}, targs) -> begin
                    assert(List.length targs > 0);
                    let t_ret = List.nth targs (List.length targs - 1) in

                    match fst e.value with
                    | VarExpr x when Atom.Set.find_opt x Arg.toplevel_functions = None -> 
                        AppExpr(
                            auto_place(AccessExpr(
                                cexpr e,
                                auto_place (RawExpr "apply", auto_place TUnknown)
                            ), auto_place TUnknown),
                            es
                        )
                    | _ -> AppExpr (cexpr e, List.map cexpr es)
                end
                | _ -> AppExpr (cexpr e, List.map cexpr es)
            end
            | _ -> AppExpr (cexpr e, List.map cexpr es)
        end
        | AssertExpr e -> AssertExpr (cexpr e)
        | AssignExpr (e1, op, e2) -> AssignExpr (cexpr e1, op, cexpr e2)
        | BinaryExpr (e1, StructuralEqual, e2) -> 
        begin
            match (snd e1.value).value, (snd e2.value).value with
            | TAtomic "Integer", _ | TAtomic "Void", _ | TAtomic "void", _ 
            | _, TAtomic "Integer" | _, TAtomic "Void" | _, TAtomic "void"
             -> 
                (*  Equal == StructuralEqual for those types in Java
                    Moreover, .equals() works only on boxed types (e.g. Integer) and not on atomic (e.g. int)    
                    Therefore, we use Equal
                *)
                BinaryExpr (e1, Equal, e2)
            | _-> 
                AppExpr ( 
                    auto_place (AccessExpr (
                        cexpr e1, 
                        auto_place (VarExpr (Atom.builtin "equals"), auto_place TUnknown)
                    ), auto_place TUnknown),
                    [cexpr e2]
                ) 
        end
        | BinaryExpr (e1, NotStructuralEqual, e2) -> 
            fst (cexpr (auto_place( UnaryExpr( 
                AstUtils.Not, 
                {
                    place = place@fplace; 
                    value = BinaryExpr (e1, StructuralEqual, e2), jtype}), auto_place TUnknown))).value
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
        | BBExpr bbterm -> BBExpr bbterm
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
    | ForeachStmt(ct, x, e, stmt) -> ForeachStmt (
        ct,
        x, 
        cexpr e,
        cstmt stmt
    )
    | LetStmt (t, x, e_opt) -> LetStmt (t, x, Option.map cexpr e_opt)
    | ReturnStmt e -> ReturnStmt (cexpr e) 
    | RawStmt s -> RawStmt s 
    | TemplateStmt (str, models) -> TemplateStmt (str, models)
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
        (** stmt list -> bool
            Return should be the last stmt of method,
            comments excepted
        *)
        let rec ends_by_return = function 
            | [] -> false
            | stmts -> begin
                let rec aux = function
                    | [] -> false
                    | {value=BBStmt _} ::_ -> true (* External to Varda reach *)
                    | {value=ReturnStmt _} :: _ -> true
                    | {value=CommentsStmt _} :: stmts -> aux stmts 
                    | {value=IfStmt (_, stmt1, None)}::_ -> false 
                    | {value=IfStmt (_, stmt1, Some stmt2)}::_ ->  (ends_by_return [stmt1]) && (ends_by_return [stmt2])
                    | {value=BlockStmt stmts} :: _ -> ends_by_return stmts
                    | _ -> false
                in
                aux (List.rev stmts)
            end
        in
        let body = match m0.ret_type with
            | None -> m0.body (* constructor *)
            | Some {value=TAtomic "void"} -> m0.body
            | Some {value=TAtomic "Void"} ->
                if ends_by_return m0.body then
                    m0.body
                else ( 
                    (* Add a return since Void needs a return *)
                    m0.body @ [
                        auto_fplace (ReturnStmt (auto_fplace(RawExpr "null", auto_fplace (TAtomic "null"))))
                    ]
                )
            | Some {value=ClassOrInterfaceType({value=TAtomic "Either"}, [t_left; {value=TAtomic "UUID"}])} ->
                if ends_by_return m0.body then
                    m0.body
                else ( 
                    m0.body @ [
                        auto_fplace (ReturnStmt (auto_fplace(RawExpr "null", auto_fplace (TAtomic "null"))))
                    ]
                )
            | Some {value=ClassOrInterfaceType({value=TAtomic "Either"}, [t_left; {value=TAtomic "Void"}])} ->
                if ends_by_return m0.body then
                    m0.body
                else ( 
                    m0.body @ [
                        auto_fplace (ReturnStmt (
                            auto_fplace (RawExpr "Either.right(null)", auto_fplace TUnknown)
                        ))
                    ]
                )
            | Some {value=ClassOrInterfaceType({value=TAtomic "Either"}, [_; {value=ClassOrInterfaceType({value=TAtomic "CompletableFuture"}, [{value=_}])}])}->
                if ends_by_return m0.body then
                    m0.body
                else ( 
                    let f = Atom.fresh "ret_ffuture" in

                    m0.body @ [
                        (* create the future *)
                        auto_fplace(LetStmt(
                            auto_fplace (TAtomic "CompletableFuture<Either<com.varda.Error, Void>>"),
                            f,
                            Some (auto_fplace( RawExpr "new CompletableFuture()", auto_fplace TUnknown))
                        ));
                        (* Complete future *)
                        auto_fplace (ExpressionStmt(
                            auto_fplace(AppExpr(
                                auto_fplace(AccessExpr(
                                    auto_fplace(VarExpr f, auto_fplace TUnknown),
                                    auto_fplace (RawExpr "complete", auto_fplace TUnknown)
                                ), auto_fplace TUnknown),
                                [
                                    auto_fplace (RawExpr "null", auto_fplace TUnknown) 
                                ]
                            ), auto_fplace TUnknown)
                        ));
                        auto_fplace (ReturnStmt (
                            auto_fplace (AppExpr(
                                auto_fplace (RawExpr "Either.right", auto_fplace TUnknown),
                                [ auto_fplace(VarExpr f, auto_fplace TUnknown) ]
                            ), auto_fplace TUnknown)
                        ))
                    ]
                )
            | Some ret_type when Bool.not (ends_by_return m0.body) -> 
                raise (Error.PlacedDeadbranchError (place, Printf.sprintf "Method has no return : %s \n %s" (Atom.to_string m0.name) (show_jtype ret_type)))
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

    let displayed_pass_shortdescription = Printf.sprintf "Cleaned Lg AST for file %s" Arg.filename
    let displayed_ast_name = "cleaned Java AST"
    let show_ast = true
    let global_at_most_once_apply = false

    let precondition program = program
    let postcondition program = program
    let apply_program = clean_program
end