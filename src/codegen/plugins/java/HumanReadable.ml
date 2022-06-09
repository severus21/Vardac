open Ast
open Core
open Core.AstUtils
open Easy_logging

let logger = Logging.make_logger ("_1_ compspec.plg.Java") Debug [];;
let fplace = (Error.forge_place "Plg=Java.HumanReadable" 0 0)

(* Atom.value -> already binded in the scope *)
module StrSet = Set.Make(String)
type ctx = {
    local: StrSet.t;
}

(* exisiting atom.id -> new Atom*)
let renaming : (int, Atom.atom) Hashtbl.t = Hashtbl.create 256
let filename2ctx = Hashtbl.create 16
let get_ctx filename = 
    let fresh_ctx () : ctx = {
        local       = StrSet.empty;
    } in    
    match Hashtbl.find_opt filename2ctx filename with
    | None -> fresh_ctx ()
    | Some ctx -> ctx

let store_ctx filename ctx = 
    match Hashtbl.find_opt filename2ctx filename with
    | None -> Hashtbl.add filename2ctx filename ctx
    | _ ->  ()
    (* debug failwith (Printf.sprintf "ctx already exists for filename [%s]" filename) *)

module Make(Arg: sig val filename:string end) = struct
    let cl_filename = (Filename.basename Arg.filename)



    (* hr_XX 
    a) rename binder x:atom to builtin except when they found x in ctx and add it to ctx
    b) rename x:atom not in binder to builtin if x in ctx*)

    let hr_atom_no_binder ctx x = 
        if Atom.is_builtin x then ctx, x
        else 
            let tokens = String.split_on_char '.' (Atom.to_string x) in

            (* if x = author.project_name.XXX.YY *)
            if List.length tokens > 1 then
                let rec pre_tokens = function 
                    | [] -> []
                    | x::y::[] -> [x]
                    | x::xs -> x::(pre_tokens xs)
                in
                (* package not renamed and cls renamed using an external context *)
                let package = String.concat "." (pre_tokens tokens) in
                let external_filename = List.nth tokens (List.length tokens -2) in
                let cls = List.nth tokens (List.length tokens -1) in

                let cls_id, cls_hint = 
                    try
                        let re = Str.regexp {|\([a-zA-Z0-9_]*[a-zA-Z_]\)\([0-9]+\)|} in
                        let _ = Str.search_forward re cls 0 in
                        int_of_string (Str.matched_group 2 cls), Str.matched_group 1 cls
                    with Not_found -> failwith cls 
                in

                if cl_filename = cls || external_filename = (Config.project_name ()) then ctx, x
                else
                    let external_ctx = get_ctx external_filename in
                    let z = (Atom.craft cls_id cls_hint cls_hint false) in
                    match Hashtbl.find_opt renaming (Atom.identity z) with
                    | None -> 
                        (* Not in stage order therefore we can not rename *)
                        logger#debug "[external %s] Not in stage order (HR) %s %s" external_filename (Atom.show z) (Atom.to_string x);
                        Hashtbl.add renaming (Atom.identity z) z;
                        let external_ctx = {
                            local = StrSet.add (Atom.hint z) external_ctx.local;
                        } in
                        store_ctx external_filename external_ctx;
                        ctx, x
                    | Some cls -> 
                        logger#debug "In stage order (HR) %s %s" (Atom.to_string x) (Atom.to_string cls);
                        ctx, Atom.builtin (package^"."^(Atom.to_string cls))
            else
                if cl_filename = Atom.to_string x then ctx, x
                else
                    match Hashtbl.find_opt renaming (Atom.identity x) with
                    | None -> ctx, x
                        (* debug - can not be used in prod since GRPC code is defined outisde the reach of Java plugin 
                        raise (Error.PlacedDeadbranchError (fplace, Printf.sprintf "[%s] [%s] not found in renaming" cl_filename (Atom.to_string x))) *)
                    | Some y -> 
                        logger#error "set %s -> %s" (Atom.to_string x) (Atom.to_string y);
                        ctx, y 
    let hr_atom_binder ctx x = 
        if Atom.is_builtin x then ctx, x
        else if Atom.hint x = "main" then( (* reserved *)
            Hashtbl.add renaming (Atom.identity x) x;
            ctx, x
        )else 
            match Hashtbl.find_opt renaming (Atom.identity x) with
            | Some y -> (*already binded e.g. namedexpr seen during shallow scan *)
                ctx, y
            | None -> begin
                match StrSet.find_opt (Atom.hint x) ctx.local with
                | None -> 
                    let y = Atom.builtin (Atom.hint x) in 
                    logger#error "[%s] let %s %s" cl_filename (Atom.to_string x) (Atom.to_string y);
                    Hashtbl.add renaming (Atom.identity x) y;
                    let ctx = {
                        local = StrSet.add (Atom.hint x) ctx.local;
                    } in
                    ctx, y 
                | Some _ -> 
                    Hashtbl.add renaming (Atom.identity x) x;
                    ctx, x 
            end

    (* jtype has no binders, only case b) *)
    let rec hr_jt_ ?(is_binder=false) ctx place item =
        match item with 
        | TAtomic _ | TUnknown -> ctx, item
        | TBB bbterm -> 
            let ctx, bbterm = hr_bbterm ctx bbterm in
            ctx, TBB bbterm
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

    and hr_bb_ ctx = function  
    | Text str -> ctx, Text str
    | Varda e -> 
        let ctx, e = (hr_expr ctx e) in
        ctx, Varda e
    | Template (str, models) -> ctx, Template (str, models)
    and hr_bbterm ctx = map2_place (fun place bbs -> List.fold_left_map hr_bb_ ctx bbs)

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
            ctx, NewExpr(e1, es)
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
        | LiteralExpr _ | ThisExpr | RawExpr _ -> ctx, item 
        | BBExpr bbterm -> 
            let ctx, bbterm = hr_bbterm ctx bbterm in
            ctx, BBExpr bbterm
        | LambdaExpr (args, stmt) -> 
            let inner_ctx, args = List.fold_left_map (
                fun ctx (jt, x) ->
                    let ctx, jt = hr_jt ctx jt in
                    let ctx, x = hr_atom_binder ctx x in
                    ctx, (jt, x)
            ) ctx args in
            let inner_ctx, stmt = hr_stmt inner_ctx stmt in
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
        | BreakStmt | CommentsStmt _ | ContinueStmt | EmptyStmt | RawStmt _ | TemplateStmt _ -> ctx, item
        | BBStmt bbterm -> 
            let ctx, bbterm = hr_bbterm ctx bbterm in
            ctx, BBStmt bbterm
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
    let map0_annotated (fct: 'a -> 'b) ({annotations; decorators; v}:'a annotated) : 'b = 
        fct v

    let rec hr_body_ parent_opt ctx place = function
    | ClassOrInterfaceDeclaration cl ->
        let ctx, name = 
            (* Parent class are not renamed *)
            if Atom.to_string cl.name = cl_filename then ctx, cl.name
            else 
                (* Already binded in shallow scan of parent class*)
                hr_atom_binder ctx cl.name 
        in
        (* Type parameters are binders for generics *)
        let ctx, parameters = List.fold_left_map (hr_jt ~is_binder:true) ctx cl.parameters in
        let ctx, extended_types = List.fold_left_map hr_jt ctx cl.extended_types in 
        let ctx, implemented_types = List.fold_left_map hr_jt ctx cl.implemented_types in 

        (* Shallow scan of methods and fields to binded them before doing the depth first renaming *)
        let ctx = List.fold_left (function ctx -> map0_place (function place -> (function 
            | Body{ value={v=ClassOrInterfaceDeclaration cl}} ->
                let ctx, name = 
                    (* Parent class are not renamed *)
                    if Atom.to_string cl.name = cl_filename then ctx, cl.name
                    else hr_atom_binder ctx cl.name 
                in
                ctx
            | Body{ value={v=MethodDeclaration m}} ->
                let ctx, name = 
                    match parent_opt with
                    | Some (x, hr_x) when x = m.name -> ctx, hr_x (* should have the same name as the parent constructor *)
                    | _ -> hr_atom_binder ctx m.name
                in
                ctx
            | Body{ value={v=FieldDeclaration f}} -> 
                logger#debug "sscan %s" (Atom.to_string f.name);
                let ctx, name = hr_atom_binder ctx f.name in
                ctx
            | Stmt{ value=NamedExpr (_, name, _)} -> (* attribute definition *)
                let ctx, name = hr_atom_binder ctx name in
                ctx
            | _-> ctx
        ))) ctx cl.body in

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
        (* Already binded in shallow scan *)
        let ctx, name = hr_atom_no_binder ctx m.name in
        let ctx, ret_type = match m.ret_type with
            | None -> ctx, None
            | Some jt -> let ctx, jt = hr_jt ctx jt in ctx, Some jt
        in
        logger#debug "<<<HR method [%s]" (Atom.to_string name);
        let inner_ctx, parameters = List.fold_left_map (fun ctx -> 
            function (decorators, jt, x) -> 
                let ctx, jt = hr_jt ctx jt in 
                let ctx, x  = hr_atom_binder ctx x in 
                ctx, (decorators, jt, x)
        ) ctx m.parameters in 
        logger#debug ">>>";
        let inner_ctx, body = List.fold_left_map hr_stmt inner_ctx m.body in
        let ctx, throws = List.fold_left_map hr_atom_no_binder ctx m.throws in
        ctx,  MethodDeclaration {
            ret_type;
            name;
            parameters;
            body;
            throws;
        }
    | FieldDeclaration f -> 
        logger#debug "hrf %s" (Atom.to_string f.name);
        (* Already binded in shallow scan *)
        let ctx, name = hr_atom_no_binder ctx f.name in
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
        | Comments _ | JModule _ | Raw _ -> ctx, item
        | BBItem bbterm -> 
            let ctx, bbterm = hr_bbterm ctx bbterm in
            ctx, BBItem bbterm
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
        (* DEBUG
        let tmp = Hashtbl.fold (fun k v acc -> 
            Printf.sprintf "%s- %d: %s \n" acc k (Atom.to_string v)
        ) renaming "" in
        logger#debug "renaming: \n%s\n\n" tmp;*)

        (* Can not be renamed since it is tightly linked with proto buf generated classes *)
        let re_service_impl = Str.regexp {|[A-Za-z0-9_]*ServiceImpl[0-9]+|} in
        if (*cl_filename = "MaingRPCServer" || cl_filename = "MaingRPCClient" ||*) Str.string_match re_service_impl cl_filename 0 then
            program
        else
            let ctx, program = List.fold_left_map (hr_str_item None) (get_ctx cl_filename) program in
            store_ctx cl_filename ctx;
            program
(*****************************************************)
    let name = "Java.HumanReadable"
    let displayed_pass_shortdescription = Printf.sprintf "HumanReadable Java AST for file %s" Arg.filename
    let displayed_ast_name = "HR Java"
    let show_ast = true
    let global_at_most_once_apply = false

    let precondition program = program
    let postcondition program = program
    let apply_program = hr_program
end