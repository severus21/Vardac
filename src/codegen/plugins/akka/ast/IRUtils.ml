open Core.AstUtils
module Atom = Core.Atom
open AAst

let fplace = (Core.Error.forge_place "Plg=AkkaJava" 0 0)
let auto_fplace smth = {place = fplace; value=smth}
open AstUtils2.Make(struct let fplace = fplace end)

let encodectype = function
    | {value=TVar x; place} -> e2var x
    | {value=Atomic x; place} -> e2var (Atom.builtin x)
    | {value=TTuple xs} -> e2var (Atom.builtin (Printf.sprintf "io.vavr.Tuple%d" (List.length xs)))
    | {value=TArray {value=Atomic x}} -> e2var (Atom.builtin (Printf.sprintf "%s[]" x))
    | _ -> failwith "TODO encodectype_Finish"

open Core

let rec map_constructor f : method0 list -> method0 list = function
| [] -> []
| m::ms when m.value.v.is_constructor -> (f m)::ms
| m::ms -> m::(map_constructor f ms)

let rec apply_rename_place (apply_rename : Core.Error.place -> 'a -> 'a) ({ Core.AstUtils.place ; Core.AstUtils.value}: 'a Core.AstUtils.placed) = 
    let value = apply_rename place value in
    {Core.AstUtils.place; Core.AstUtils.value}
let rec _apply_rename_ctype (renaming : Atom.atom -> Atom.atom) place : _ctype -> _ctype = function
    | Atomic s -> Atomic s
    | ActorRef ct -> ActorRef (apply_rename_ctype renaming ct)
    | TActivationRef ct -> TActivationRef (apply_rename_ctype renaming ct)
    | TFunction (ct1, ct2) -> TFunction (
        apply_rename_ctype renaming ct1,
        apply_rename_ctype renaming ct2
    )
    | TArray ct -> TArray (
        apply_rename_ctype renaming ct
    )
    | TList ct -> TList (
        apply_rename_ctype renaming ct
    )
    | TMap (ct1, ct2) -> TMap (
        apply_rename_ctype renaming ct1,
        apply_rename_ctype renaming ct2
    )
    | TOption ct -> TOption (
        apply_rename_ctype renaming ct
    )
    | TResult (ct1, ct2) -> TResult (
        apply_rename_ctype renaming ct1,
        apply_rename_ctype renaming ct2
    )
    | TSet ct -> TSet (
        apply_rename_ctype renaming ct
    )
    | TTuple  cts -> TTuple (
        List.map (apply_rename_ctype renaming) cts
    ) 
    | TVar x -> TVar (renaming x) 
    | TAccess (ct1, ct2) -> TAccess (
        apply_rename_ctype renaming ct1,
        apply_rename_ctype renaming ct2
    )
    | TParam (ct, cts) -> TParam (
        apply_rename_ctype renaming ct,
        List.map (apply_rename_ctype renaming) cts
    )
    | TRaw s -> TRaw s
    | TUnknown -> TUnknown
    | TBB tbb -> TBB tbb
and apply_rename_ctype renaming (ct:ctype) = apply_rename_place (_apply_rename_ctype renaming) ct

and _apply_rename_expr rename_binders (renaming : Atom.atom -> Atom.atom) place (e,mt) = 

(match e with
    | AccessExpr (e1, e2) -> AccessExpr (
        apply_rename_expr rename_binders renaming e1,
        apply_rename_expr rename_binders renaming e2
    )
    | AccessMethod (e1, x) -> AccessMethod (
        apply_rename_expr rename_binders renaming e1,
        renaming x 
    )
    | ActivationRef {schema; actor_ref} -> ActivationRef { 
        schema = apply_rename_expr rename_binders renaming schema;
        actor_ref = apply_rename_expr rename_binders renaming actor_ref
    }
    | InterceptedActivationRef {actor_ref; intercepted_actor_ref} -> InterceptedActivationRef { 
        actor_ref = apply_rename_expr rename_binders renaming actor_ref;
        intercepted_actor_ref = Option.map (apply_rename_expr rename_binders renaming) intercepted_actor_ref
    }
    | AssertExpr e -> AssertExpr (
        apply_rename_expr rename_binders renaming e
    ) 
    | BinopExpr (e1, binop, e2) -> BinopExpr (
        apply_rename_expr rename_binders renaming e1,
        binop,
        apply_rename_expr rename_binders renaming e2
    ) 
    | CallExpr (e,es) -> CallExpr (
        apply_rename_expr rename_binders renaming e,
        List.map (apply_rename_expr rename_binders renaming) es
    )
    | CastExpr (ct, e) -> CastExpr (
        apply_rename_ctype renaming ct,
        apply_rename_expr rename_binders renaming e
    )
    | LambdaExpr (params, stmt) -> LambdaExpr (
        List.map (function (mt, x) -> 
            apply_rename_ctype renaming mt, 
            if rename_binders then renaming x else x
        ) params,
        apply_rename_stmt rename_binders renaming stmt
    ) 
    | LitExpr l -> LitExpr l
    | This -> This
    | UnopExpr (unop, e) -> UnopExpr (
        unop, 
        apply_rename_expr rename_binders renaming e
    )         
    | VarExpr x -> VarExpr (renaming  x)                   
    | NewExpr (e, es) -> NewExpr (apply_rename_expr rename_binders renaming e, List.map (apply_rename_expr rename_binders renaming) es)                   
    | ClassOf ct -> ClassOf (apply_rename_ctype renaming ct)

    | BlockExpr (b, es) -> BlockExpr (
        b,
        List.map (apply_rename_expr rename_binders renaming) es
    ) 
    | Block2Expr (b, ees) -> Block2Expr (
        b,
        List.map (function (e1, e2) -> 
            apply_rename_expr rename_binders renaming e1,
            apply_rename_expr rename_binders renaming e2
        ) ees
    ) 
    | Spawn s -> Spawn {
        context = apply_rename_expr rename_binders renaming s.context;
        actor_expr = apply_rename_expr rename_binders renaming s.actor_expr }
    | CurrentContext -> CurrentContext
    | CurrentSystem -> CurrentSystem
    | RawExpr s -> RawExpr s 
    | TernaryExpr (e1, e2, e3) -> TernaryExpr (
            apply_rename_expr rename_binders renaming e1,
            apply_rename_expr rename_binders renaming e2,
            apply_rename_expr rename_binders renaming e3
    )
), apply_rename_ctype renaming mt
and apply_rename_expr rename_binders renaming e = apply_rename_place (_apply_rename_expr rename_binders renaming) e

and _apply_rename_stmt rename_binders (renaming : Atom.atom -> Atom.atom) place = function
    | AssignExpr (e1, e2) -> AssignExpr (apply_rename_expr rename_binders renaming e1, apply_rename_expr rename_binders renaming e2)
    | BlockStmt stmts -> BlockStmt (List.map (apply_rename_stmt rename_binders renaming) stmts)
    | BreakStmt -> BreakStmt
    | EmptyStmt -> EmptyStmt
    | LetStmt (ct, x, e_opt) -> LetStmt (
        apply_rename_ctype renaming ct,
        (if rename_binders then renaming x else x),
        Option.map (apply_rename_expr rename_binders renaming) e_opt
    )
    | CommentsStmt c -> CommentsStmt c
    | ContinueStmt -> ContinueStmt
    | ExpressionStmt e -> ExpressionStmt (apply_rename_expr rename_binders renaming e)
    | IfStmt (e, stmt1, stmt2_opt) -> IfStmt (
        apply_rename_expr rename_binders renaming e,
        apply_rename_stmt rename_binders renaming stmt1,
        Option.map (apply_rename_stmt rename_binders renaming) stmt2_opt    
    )
    | ForeachStmt(ct, x, e, stmt) -> ForeachStmt (
        apply_rename_ctype renaming ct,
        (if rename_binders then renaming x else x),
        apply_rename_expr rename_binders renaming e,
        apply_rename_stmt rename_binders renaming stmt
    )
    | ReturnStmt e -> ReturnStmt (apply_rename_expr rename_binders renaming e)
    | TryStmt (stmt, branches) -> TryStmt (
        apply_rename_stmt rename_binders renaming stmt,
        List.map (function (ct, x, stmt)->
            apply_rename_ctype renaming ct,
            renaming x,
            apply_rename_stmt rename_binders renaming stmt
        ) branches
    )
    | RawStmt str -> RawStmt str
    | TemplateStmt (str, models) -> TemplateStmt (str, models)
and apply_rename_stmt rename_binders renaming stmt = apply_rename_place (_apply_rename_stmt rename_binders renaming) stmt 

and apply_rename_method0_body rename_binders (renaming : Atom.atom -> Atom.atom) = function
| AbstractImpl stmts -> AbstractImpl (List.map (apply_rename_stmt rename_binders renaming) stmts) 
| BBImpl bbterm -> BBImpl bbterm

and _apply_rename_method0 rename_binders (renaming : Atom.atom -> Atom.atom) place m0 =
    {
        decorators = m0.decorators;
        annotations = m0.annotations;
        v = {
            ret_type =  apply_rename_ctype renaming m0.v.ret_type;
            name = (if rename_binders then renaming m0.v.name else m0.v.name);
            body = apply_rename_method0_body rename_binders renaming m0.v.body;
            args = List.map (function (ct, x) -> (
                apply_rename_ctype renaming ct,
                renaming x
            )) m0.v.args;
            is_constructor = m0.v.is_constructor;
            throws = List.map renaming m0.v.throws;
        }
}
and apply_rename_method0 rename_binders renaming m0 = apply_rename_place (_apply_rename_method0 rename_binders renaming) m0 

and _apply_rename_event rename_binders (renaming : Atom.atom -> Atom.atom) place e =
{
    vis = e.vis;
    name = if rename_binders then renaming e.name else e.name;
    args = List.map (function (ct, x) ->
        apply_rename_ctype renaming ct,
        renaming x
    ) e.args;
    headers = e.headers;
}
and apply_rename_event rename_binders renaming e = apply_rename_place (_apply_rename_event rename_binders renaming) e 

and _apply_rename_state rename_binders (renaming : Atom.atom -> Atom.atom) place s =
{
    persistent = s.persistent;
    stmts = List.map (apply_rename_stmt rename_binders renaming) s.stmts
}
and apply_rename_state rename_binders renaming s = apply_rename_place (_apply_rename_state rename_binders renaming) s 

and _apply_rename_actor rename_binders (renaming : Atom.atom -> Atom.atom) place (a:_actor) =
{
    is_guardian = a.is_guardian;
    extends = Option.map (apply_rename_ctype renaming) a.extends;
    implemented_types = List.map (apply_rename_ctype renaming) a.implemented_types;
    name = if rename_binders then renaming a.name else a.name;
    methods = List.map (apply_rename_method0 rename_binders renaming) a.methods;
    receiver = (apply_rename_method0 rename_binders renaming) a.receiver;
    states = List.map (apply_rename_state rename_binders renaming) a.states;
    events = List.map (apply_rename_event rename_binders renaming) a.events; 
    nested_items = List.map (apply_rename_term rename_binders renaming) a.nested_items;
    static_items = List.map (apply_rename_term rename_binders renaming) a.static_items;
    headers = a.headers;
}
and apply_rename_actor rename_binders renaming a = apply_rename_place (_apply_rename_actor rename_binders renaming) a 

and _apply_rename_term rename_binders (renaming : Atom.atom -> Atom.atom ) place {annotations; decorators;v} = 
{
    annotations = annotations;
    decorators = decorators;
    v = match v with
        | Actor a -> Actor (apply_rename_actor rename_binders renaming a)
        | Class x -> Class (renaming x) 
        | ClassOrInterfaceDeclaration cdcl -> ClassOrInterfaceDeclaration
            {
                headers = cdcl.headers;
                isInterface = cdcl.isInterface; 
                name = if rename_binders then renaming cdcl.name else cdcl.name;
                extends = Option.map (apply_rename_ctype renaming) cdcl.extends;
                implemented_types = List.map (apply_rename_ctype renaming) cdcl.implemented_types;
                body = List.map (apply_rename_term rename_binders renaming) cdcl.body; 
            }
        | Comments c -> Comments c 
        | Event e -> Event (apply_rename_event rename_binders renaming e)
        | Import s -> Import s 
        | MethodDeclaration m0 -> MethodDeclaration (apply_rename_method0 rename_binders renaming m0)
        | RawClass (x,raw) -> RawClass (renaming x, raw)
        | RawTerm raw -> RawTerm raw
        | Stmt stmt -> Stmt (apply_rename_stmt rename_binders renaming stmt)
}
and apply_rename_term rename_binders renaming t = apply_rename_place (_apply_rename_term rename_binders renaming) t 

let rec _replaceterm_term (rename_binders:bool) selector replace place : _term annotated -> _term annotated = function
| t when selector t -> replace t 
| {annotations; decorators; v=Actor a} -> 
    {    
        annotations;
        decorators;
        v = Actor {
            place = a.place; 
            value = {a.value with
                nested_items = List.map (replaceterm_term rename_binders selector replace) a.value.nested_items;
            }
        } 
    }
| {annotations; decorators; v = ClassOrInterfaceDeclaration cdcl} -> 
    {    
        annotations;
        decorators;
        v = ClassOrInterfaceDeclaration {cdcl with 
        body = List.map (replaceterm_term rename_binders selector replace) cdcl.body; 
        }
    }
| t -> t
and replaceterm_term (rename_binders:bool) (selector : _term annotated -> bool) (replace : _term annotated-> _term annotated) t = apply_rename_place (_replaceterm_term rename_binders selector replace) t


let rec apply_rewrite_place (apply_rewrite : Core.Error.place -> 'a -> 'a) ({ Core.AstUtils.place ; Core.AstUtils.value}: 'a Core.AstUtils.placed) = 
    let value = apply_rewrite place value in
    {Core.AstUtils.place; Core.AstUtils.value}

let rec _rewriteexpr_expr selector rewriter place (e, mt): _expr * ctype = 
(match e with 
    | e when selector e -> rewriter e
    | AccessExpr (e1, e2) -> AccessExpr(
        rewriteexpr_expr selector rewriter e1,
        rewriteexpr_expr selector rewriter e2
    )
    | AccessMethod (e, x) -> AccessMethod (
        rewriteexpr_expr selector rewriter e,
        x
    ) 
    | ActivationRef {schema; actor_ref} -> ActivationRef {
        schema;
        actor_ref  = rewriteexpr_expr selector rewriter actor_ref
    }
    | InterceptedActivationRef {actor_ref; intercepted_actor_ref} -> InterceptedActivationRef {
        actor_ref  = rewriteexpr_expr selector rewriter actor_ref;
        intercepted_actor_ref  = Option.map (rewriteexpr_expr selector rewriter) intercepted_actor_ref
    }
    | AssertExpr e -> AssertExpr (
        rewriteexpr_expr selector rewriter e
    ) 
    | BinopExpr (e1, op, e2) -> BinopExpr (
        rewriteexpr_expr selector rewriter e1,
        op,
        rewriteexpr_expr selector rewriter e2
    )
    | CallExpr (e, es) -> CallExpr (
        rewriteexpr_expr selector rewriter e,
        List.map (rewriteexpr_expr selector rewriter) es
    )
    | LambdaExpr (xs, stmt) -> LambdaExpr (
        xs,
        rewriteexpr_stmt selector rewriter stmt
    )
    | LitExpr _ as e -> e
    | This -> This
    | UnopExpr (op, e) -> UnopExpr (
        op,
        rewriteexpr_expr selector rewriter e
    )
    | VarExpr _ as e -> e
    | NewExpr (e, es) ->  NewExpr (
        rewriteexpr_expr selector rewriter e,
        List.map (rewriteexpr_expr selector rewriter) es
    )
    | ClassOf _ as e -> e
    | BlockExpr (b, es) -> BlockExpr (
        b,
        List.map (rewriteexpr_expr selector rewriter) es
    ) 
    | Spawn s -> Spawn {
        context = rewriteexpr_expr selector rewriter s.context;
        actor_expr = rewriteexpr_expr selector rewriter s.actor_expr;
    }
    | CurrentContext -> CurrentContext
    | CurrentSystem -> CurrentSystem
    | RawExpr _ as e -> e
), mt
and rewriteexpr_expr selector rewriter = apply_rewrite_place (_rewriteexpr_expr selector rewriter)
and _rewriteexpr_stmt selector rewriter place : _stmt -> _stmt = function
| AssignExpr (e1, e2) -> AssignExpr (
    rewriteexpr_expr selector rewriter e1,
    rewriteexpr_expr selector rewriter e2
    )
| BlockStmt stmts -> BlockStmt (List.map (rewriteexpr_stmt selector rewriter) stmts)
| BreakStmt as stmt -> stmt
| EmptyStmt as stmt -> stmt
| LetStmt (ct, x, e_opt) -> LetStmt (
    ct,
    x, 
    Option.map (rewriteexpr_expr selector rewriter) e_opt
)
| CommentsStmt _ as stmt -> stmt
| ContinueStmt as stmt -> stmt
| ExpressionStmt e -> ExpressionStmt (rewriteexpr_expr selector rewriter e)
| IfStmt (e, stmt1, stmt2_opt) -> IfStmt (
    rewriteexpr_expr selector rewriter e,
    rewriteexpr_stmt selector rewriter stmt1,
    Option.map (rewriteexpr_stmt selector rewriter) stmt2_opt
)
| ForeachStmt(mt, x, e, stmt) -> ForeachStmt(
    mt,
    x,
    rewriteexpr_expr selector rewriter e,
    rewriteexpr_stmt selector rewriter stmt
)
| ReturnStmt e -> ReturnStmt (rewriteexpr_expr selector rewriter e)
| TryStmt (stmt, branches) ->
    TryStmt(
        rewriteexpr_stmt selector rewriter stmt,
        List.map (function (ct, x, stmt2) -> (
            ct,
            x,
            rewriteexpr_stmt selector rewriter stmt2
        
        )) branches
    )
and rewriteexpr_stmt selector rewriter = apply_rewrite_place (_rewriteexpr_stmt selector rewriter)

