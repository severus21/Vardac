open Ast
open Core
open Core.AstUtils
open Easy_logging

let logger = Logging.make_logger ("_1_ compspec.plg.Java") Debug [];;


module Make(Arg: sig val filename:string end) = struct
    let cl_filename = (Filename.basename Arg.filename)

    module Env = Set.Make(Atom)
    type ctx = Env.t 
    let fresh_ctx () : ctx = Env.empty


    (* hr_XX 
    a) rename binder x:atom to builtin except when they found x in ctx and add it to ctx
    b) rename x:atom not in binder to builtin if x in ctx*)

    let hr_atom_no_binder ctx x = 
        if Atom.is_builtin x then ctx, x
        else
            match Env.find_opt x ctx with
            | None -> ctx, x
            | Some _ -> ctx, Atom.builtin (Atom.value x)

    let hr_atom_binder ctx x = 
        if Atom.is_builtin x then ctx, x
        else
            match Env.find_opt x ctx with
            | None -> Env.add x ctx, Atom.builtin (Atom.value x)
            | Some _ -> ctx, x

    (* jtype has no binders, only case b) *)
    let rec hr_jt_ ?(is_binder=false) ctx place item =
        match item with 
        | TAtomic _ | TUnknown | TBB _ -> ctx, item
        | TVar x -> 
            let ctx, x = 
                if is_binder then hr_atom_binder ctx x
                else hr_atom_no_binder ctx x 
            in
            ctx, TVar x
        | TAccess (jt1, jt2) ->
            let ctx1, jt1 = hr_jt ctx jt1 in 
            let ctx2, jt2 = hr_jt ctx1 jt2 in 
            ctx2, TAccess (jt1, jt2)
        | TArray jt -> 
            let ctx, jt = hr_jt ctx jt in
            ctx, TArray jt
        | ClassOrInterfaceType (jt_cl, jt_args) ->
            let ctx0, jt_cl = hr_jt ctx jt_cl in 
            let ctx1, jt_args = List.fold_left_map hr_jt ctx0 jt_args in
            ctx1, ClassOrInterfaceType (jt_cl, jt_args)
    and hr_jt ?(is_binder=false) ctx = map2_place (hr_jt_ ~is_binder:is_binder ctx)

    and hr_expr_ ctx place (item, jt) =
        let ctx, jt = hr_jt ctx jt in
        let ctx, e  = (match item  with
        | AccessExpr (e1, e2) ->
            let ctx, e1 = hr_expr ctx e1 in
            let ctx, e2 = hr_expr ctx e2 in
            ctx, AccessExpr (e1, e2)
        | AccessMethod (e1, x)  ->
            let ctx, e1 = hr_expr ctx e1 in
            let ctx, x = hr_atom_no_binder ctx x in
            ctx, AccessMethod (e1, x)
        | AppExpr (e1, es) -> 
            let ctx, e1 = hr_expr ctx e1 in
            let ctx, es = List.fold_left_map hr_expr ctx es in
            ctx, AppExpr(e1, es)
        | NewExpr (e1, es) ->
            let ctx, e1 = hr_expr ctx e1 in
            let ctx, es = List.fold_left_map hr_expr ctx es in
            ctx, AppExpr(e1, es)
        | AssertExpr e ->
            let ctx, e = hr_expr ctx e in
            ctx, AssertExpr e
        | AssignExpr (e1, op, e2) ->
            let ctx, e1 = hr_expr ctx e1 in
            let ctx, e2 = hr_expr ctx e2 in
            ctx, AssignExpr (e1, op, e2)
        | BinaryExpr (e1, binop, e2) ->
            let ctx, e1 = hr_expr ctx e1 in
            let ctx, e2 = hr_expr ctx e2 in
            ctx, BinaryExpr (e1, binop, e2)
        | CastExpr (jt, e) -> 
            let ctx, jt = hr_jt ctx jt in 
            let ctx, e = hr_expr ctx e in
            ctx, CastExpr (jt, e)
        | LiteralExpr _ | ThisExpr | RawExpr _ | BBExpr _ -> ctx, item 
        | LambdaExpr (args, stmt) -> 
            let ctx, args = List.fold_left_map (
                fun ctx (jt, x) ->
                    let ctx, jt = hr_jt ctx jt in
                    let ctx, x = hr_atom_binder ctx x in
                    ctx, (jt, x)
            ) ctx args in
            let ctx, stmt = hr_stmt ctx stmt in
            ctx, LambdaExpr (args, stmt)
        | UnaryExpr (op, e) -> 
            let ctx, e = hr_expr ctx e in
            ctx, UnaryExpr (op, e)
        | VarExpr x -> 
            let ctx, x = hr_atom_no_binder ctx x in
            ctx, VarExpr x
        | TernaryExpr (e1, e2, e3) ->
            let ctx, e1 = hr_expr ctx e1 in
            let ctx, e2 = hr_expr ctx e2 in
            let ctx, e3 = hr_expr ctx e3 in
            ctx, TernaryExpr (e1, e2, e3)
        ) in
        ctx, (e, jt)
    and hr_expr ctx  = map2_place (hr_expr_ ctx)

    and hr_stmt_ ctx place item =
        match item with 
        | BlockStmt stmts ->
            let ctx, stmts = List.fold_left_map hr_stmt ctx stmts in
            ctx, BlockStmt stmts
        | BreakStmt | CommentsStmt _ | ContinueStmt | EmptyStmt | RawStmt _ | BBStmt _ -> ctx, item
        | ExpressionStmt e -> 
            let ctx, e = hr_expr ctx e in
            ctx, ExpressionStmt e
        | IfStmt (e, stmt1, stmt2_opt) ->
            let ctx, e = hr_expr ctx e in
            let ctx, stmt1 = hr_stmt ctx stmt1 in
            let ctx, stmt2_opt = match stmt2_opt with
                | None -> ctx, None
                | Some stmt2 -> let ctx, stmt2 = hr_stmt ctx stmt2 in ctx, Some stmt2
            in
            ctx, IfStmt (e, stmt1, stmt2_opt)
        | ForStmt (jt, x, e, stmt) ->
            let ctx, jt     = hr_jt ctx jt in
            let ctx, x      = hr_atom_binder ctx x in
            let ctx, e      = hr_expr ctx e in
            let ctx, stmt   = hr_stmt ctx stmt in 
            ctx, ForStmt (jt, x, e, stmt)
        | NamedExpr (jt, x, e_opt) ->
            let ctx, jt     = hr_jt ctx jt in
            let ctx, x      = hr_atom_binder ctx x in
            let ctx, e_opt  = match e_opt with 
                | None -> ctx, None
                | Some e -> let ctx, e      = hr_expr ctx e in ctx, Some e 
            in
            ctx, NamedExpr (jt, x, e_opt)
        | ReturnStmt e -> 
            let ctx, e = hr_expr ctx e in
            ctx, ReturnStmt e
        | TryStmt (stmt, branches) ->
            let ctx, stmt = hr_stmt ctx stmt in
            let hr_try_branch ctx (jt, x, stmt) = 
                let ctx, jt     = hr_jt ctx jt in
                let ctx, x      = hr_atom_no_binder ctx x in
                let ctx, stmt   = hr_stmt ctx stmt in 
                ctx, (jt, x, stmt)
            in
            let ctx, branches = List.fold_left_map hr_try_branch ctx branches in
            ctx, TryStmt (stmt, branches)
    and hr_stmt ctx  = map2_place (hr_stmt_ ctx)

    let map_annotated (fct: 'a -> 'b * 'c) ({annotations; decorators; v}:'a annotated) : 'b * ('c annotated)= 
        let env, v = fct v in
        env, { annotations; decorators; v}

    let rec hr_body_ parent_opt ctx place = function
    | ClassOrInterfaceDeclaration cl ->

        let ctx, name = 
            (* Parent class are not renamed *)
            if Atom.to_string cl.name = cl_filename then ctx, cl.name
            else hr_atom_binder ctx cl.name 
        in
        (* Type parameters are binders for generics *)
        let ctx, parameters = List.fold_left_map (hr_jt ~is_binder:true) ctx cl.parameters in
        let ctx, extended_types = List.fold_left_map hr_jt ctx cl.extended_types in 
        let ctx, implemented_types = List.fold_left_map hr_jt ctx cl.implemented_types in 
        let ctx, body = List.fold_left_map (hr_str_item  (Some (cl.name, name))) ctx cl.body in 

        ctx, ClassOrInterfaceDeclaration {
            isInterface = cl.isInterface;
            name;
            parameters;
            extended_types;
            implemented_types;
            body;
        }
    | MethodDeclaration m ->
        let ctx, name = 
            match parent_opt with
            | Some (x, hr_x) when x = m.name -> ctx, hr_x (* should have the same name as the parent constructor *)
            | _ -> hr_atom_binder ctx m.name
        in
        let ctx, ret_type = match m.ret_type with
            | None -> ctx, None
            | Some jt -> let ctx, jt = hr_jt ctx jt in ctx, Some jt
        in
        (* Type parameters bind generics*)
        let ctx, parameters = List.fold_left_map (fun inner_ctx -> 
            function (decorators, jt, x) -> 
                let ctx, jt = hr_jt ctx jt in 
                let ctx, x  = hr_atom_binder ctx x in 
                ctx, (decorators, jt, x)
        ) ctx m.parameters in 
        let ctx, body = List.fold_left_map hr_stmt ctx m.body in
        let ctx, throws = List.fold_left_map hr_atom_no_binder ctx m.throws in
        ctx,  MethodDeclaration {
            ret_type;
            name;
            parameters;
            body;
            throws;
        }
    | FieldDeclaration f -> 
        let ctx, name = hr_atom_binder ctx f.name in
        let ctx, type0 = hr_jt ctx f.type0 in
        let ctx, body = match f.body with
            | None -> ctx, None
            | Some e -> let ctx, e = hr_expr ctx e in ctx, Some e in 

        ctx, FieldDeclaration {
            name;
            type0;
            body;
        }

    and hr_body parent_opt ctx : body -> ctx * body = map2_place (function place -> map_annotated(hr_body_ parent_opt ctx place))

    and hr_str_item_ parent_opt ctx place item =
        match item with
        | Comments _ | JModule _ | Raw _ | BBItem _ -> ctx, item
        | Body body ->
            let ctx, body = hr_body parent_opt ctx body in
            ctx, Body body
        | JType jt -> 
            let ctx, jt = hr_jt ctx jt in
            ctx, JType jt
        | Stmt stmt -> 
            let ctx, stmt = hr_stmt ctx stmt in
            ctx, Stmt stmt
    and hr_str_item parent_opt ctx = map2_place (hr_str_item_ parent_opt ctx)
    let hr_program program = 
        snd (List.fold_left_map (hr_str_item None) (fresh_ctx ()) program)
(*****************************************************)
    let name = "Java.HumanReadable"
    let displayed_pass_shortdescription = Printf.sprintf "To HumanReadable Lg AST for file %s" Arg.filename
    let displayed_ast_name = "Java HR"
    let show_ast = true
    let global_at_most_once_apply = false

    let precondition program = program
    let postcondition program = program
    let apply_program = hr_program
end