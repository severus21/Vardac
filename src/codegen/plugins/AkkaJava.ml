open Core
open Core.AstUtils
open Jingoo
open Easy_logging

let name = "Akka<Java>"
let logger = Logging.make_logger ("_1_ compspec.plg."^name) Debug [];;

module Rt = Akka
module Lg = Java

(** Finish program*)


(* The source calculus. *)
module S = Rt.Ast 
(* The target calculus. *)
module T = Lg.Ast 


(** Split Akka.AST in multiple file then convert it to Java AST *)
(* - One actor per file *)
(* map : filename java_ast *)
let split_akka_ast_to_files (akka_program:Rt.Ast.program) : (Fpath.t * Rt.Ast.term list) Seq.t = 
    let tbl_files2program : (Fpath.t, Rt.Ast.term list) Hashtbl.t = Hashtbl.create 256 in
    let add_to_files2program file term = 
        try 
            let ast = Hashtbl.find tbl_files2program file in 
            Hashtbl.replace tbl_files2program file (term::ast)
        with | Not_found -> Hashtbl.replace tbl_files2program file [term] 
    in

    let rec split_term_to_files current_file term : unit = 
    match term.value with
    | Rt.Ast.Actor a  -> 
        let sub_actors = List.filter (function | {AstUtils.value=Rt.Ast.Actor _ } -> true | _-> false) a.value.nested_items in 
        let sub_items = List.filter (function | {AstUtils.value=Rt.Ast.Actor _ } -> false | _-> true) a.value.nested_items in 
        
        let a_current_file = if sub_actors = [] then 
                (* parentdir/main_actor.java*)
                Fpath.add_seg (Fpath.parent current_file) (Atom.to_string a.value.name)
            else 
                (* parentdir/main_actor/{main_actor.java, nested_actor1.java, nested_actor_n.java}
                where main_actor will only store the glue and not the subactor/components
                *)
                Fpath.add_seg (Fpath.add_seg (Fpath.parent current_file) (Atom.to_string a.value.name))  (Atom.to_string a.value.name)
        in

        (* add sub actor def to dedicated sub files*)
        List.iter (split_term_to_files a_current_file) a.value.nested_items; 

        (* removes subactors, they have been put in other files *)
        let term = {
            place = term.place;
            value =  Rt.Ast.Actor {
                place = a.place; value = {
                    a.value with nested_items = sub_items
                }
            }
        } in

        add_to_files2program a_current_file term 
    | _ -> add_to_files2program current_file term
    in

    (*TODO finish entrypoint/system*)
    let main_file = List.fold_left Fpath.add_seg (Fpath.v "src/main/java")  [Config.author (); Config.project_name (); "main"] in 
    List.iter (split_term_to_files main_file) akka_program.terms;

    (* Correct terms order *)
    Seq.map (function (target, ast) -> 
        (target, List.rev ast)
    ) (Hashtbl.to_seq tbl_files2program)


let rec finish_place finish_value ({ AstUtils.place ; AstUtils.value}: 'a AstUtils.placed) = 
    let value = finish_value place value in
    {AstUtils.place; AstUtils.value}

let builtin_translation = List.to_seq []
module BuiltinMap = Map.Make(String)                     
let builtin_eval = 
  let translation = BuiltinMap.of_seq builtin_translation in
  function x ->   
  try
    BuiltinMap.find (Atom.value x) translation
  with Not_found ->Atom.value x

let rec finish_ctype place : S._ctype -> T._jtype = function 
    | S.Atomic s -> T.TAtomic s 
    | S.ActorRef t -> T.ClassOrInterfaceType  (Atom.fresh_builtin "ActorRef", [fctype t])  
    | S.Behavior x -> T.ClassOrInterfaceType  (Atom.fresh_builtin "Behavior", [{place; value=T.TAtomic x}]) 
    | S.TFunction (t1, t2) -> T.ClassOrInterfaceType  (Atom.fresh_builtin "Function", [fctype t1; fctype t2]) 
    | S.TList t1 -> T.ClassOrInterfaceType  (Atom.fresh_builtin "List", [fctype t1])
    | S.TMap (t1, t2) -> T.ClassOrInterfaceType  (Atom.fresh_builtin "Map", [fctype t1; fctype t2])
    | S.TOption t1 -> T.ClassOrInterfaceType  (Atom.fresh_builtin "Optional", [fctype t1]) 
    | S.TParam ({value=S.TVar x;_}, t_args) -> T.ClassOrInterfaceType (x, List.map fctype t_args)
    | S.TParam _ -> failwith "Akka -> java, tparam with non VAR ctype is not yet supported" 
    | S.TResult (t1, t2) -> T.ClassOrInterfaceType  (Atom.fresh_builtin "Result", [fctype t1; fctype t2])  (*TODO add include https://github.com/hdevalke/result4j*)
    | S.TSet t1 -> T.ClassOrInterfaceType  (Atom.fresh_builtin "Set", [fctype t1])
    | S.TTuple cts -> begin 
        let cls_name = match List.length cts with
        | 0 -> "Unit" 
        | 1 -> "Pair" 
        | 2 -> "Triplet" 
        | 4 -> "Quartet" 
        | 5 -> "Quintet" 
        | 6 -> "Sextet" 
        | 7 -> "Septet" 
        | 8 -> "Octet" 
        | 9 -> "Ennead"
        | 10 -> "Decade"
        | _ -> failwith "Tuple with length > 10 are not supported by the javatuples library."
        in 
        T.ClassOrInterfaceType (Atom.fresh_builtin cls_name, List.map fctype cts)
    end 
    | S.TVar v -> T.TVar v
    | S.TVoid -> T.TAtomic "void"
    | S.TRaw str -> T.TAtomic str
and fctype ct : T.jtype = finish_place finish_ctype ct


let rec finish_visibility = function
    | S.Private -> T.Private
    | S.Protected -> T.Protected
    | S.Public -> T.Public
and finish_annotation = function
| S.Visibility vis -> T.Visibility (finish_visibility vis)
| S.Static -> T.Static
| S.Final -> T.Final
and finish_annotations annotations = List.map finish_annotation annotations


let finish_unop = Fun.id 
let finish_binop = Fun.id 

let rec finish_literal place : S._literal -> T._literal= function
    | S.EmptyLit -> T.EmptyLit
    | S.VoidLit -> T.VoidLit
    | S.BoolLit b -> T.BoolLit b
    | S.FloatLit f -> T.FloatLit f
    | S.IntLit f -> T.IntLit f
    | S.StringLit s -> T.StringLit s
and fliteral lit : T.literal = finish_place finish_literal lit

and finish_expr place : S._expr -> T._expr = function
    | S.AccessExpr (e1,e2) -> T.AccessExpr (fexpr e1, fexpr e2)
    | S.AssertExpr e -> T.AssertExpr (fexpr e)                   
    | S.BinopExpr (e1, op, e2) -> T.BinaryExpr ( fexpr e1, op, fexpr e2) 
    | S.CallExpr (e1,e2) -> T.AppExpr(fexpr e1, List.map fexpr e2) 
    | S.CurrentContext -> 
        T.AppExpr (
            {place; value=T.VarExpr (Atom.fresh_builtin "getContext")},
            []
        )
    | S.CurrentSystem -> T.AccessExpr 
        ({  
            place;
            value = T.AppExpr 
            ({ place; value=T.VarExpr (Atom.fresh_builtin "getContext")},
            [])
        },
        { 
            place;
            value = T.AppExpr
                ({ place; value =T.VarExpr (Atom.fresh_builtin "getSystem")},
                [])
        }) 
    | S.LambdaExpr (variables, stmt) -> T.LambdaExpr (variables, fstmt stmt)
    | S.LitExpr lit -> LiteralExpr (fliteral lit)
    | S.Spawn {context; actor_expr} -> T.AccessExpr (fexpr context, fexpr actor_expr)
    | S.This -> T.ThisExpr
    | S.UnopExpr (IR.UnpackResult, e) -> 
        T.AppExpr ( 
            {place; value=T.AccessExpr (fexpr e, { place; value=T.VarExpr (Atom.fresh_builtin "unwrap")})},
            []
        ) (* TODO should return form the fct with the error or return the result*) 
    | S.UnopExpr (op, e) -> T.UnaryExpr (op, fexpr e) 
    | S.VarExpr x -> T.VarExpr x             
    | S.RawExpr str -> T.RawExpr str
and fexpr expr : T.expr = finish_place finish_expr expr

and finish_stmt place : S._stmt -> T._stmt = function
    | S.AssignExpr (e1, e2) -> T.ExpressionStmt ( {place; value=T.AssignExpr(fexpr e1, T.AssignOp, fexpr e2)})
    | S.BlockStmt stmts -> T.BlockStmt (List.map fstmt stmts)
    | S.BreakStmt -> T.BreakStmt
    | S.CommentsStmt c -> T.CommentsStmt c
    | S.ContinueStmt -> T.ContinueStmt
    | S.ExpressionStmt e -> T.ExpressionStmt(fexpr e)
    | S.IfStmt (e, stmt1, stmt2_opt) -> T.IfStmt (fexpr e, fstmt stmt1, Option.map fstmt stmt2_opt)              
    | S.LetStmt (ct, x, None) -> 
        T.NamedExpr (fctype ct, x, {place; value = T.VarExpr (Atom.fresh_builtin "null")}) (* TODO FIXME maybe not the semantic that we want, we need to add this to the doc*)  
    | S.LetStmt (ct, x, Some e) -> T.NamedExpr (fctype ct, x, fexpr e) 
    | S.ReturnStmt e -> T.ReturnStmt (fexpr e)
and fstmt stmt : T.stmt = finish_place finish_stmt stmt

let rec finish_state  (state:S.state) : T.str_items list = 
match state.value with
    | s when s.persistent -> failwith "TODO finish _state with persistency" 
    | s -> List.map (function x -> {place = state.place; value=T.Stmt x}) (List.map fstmt s.stmts)

and finish_event place ({vis; name; kind; args}: S._event) :  T._str_items = 
    let generate_field (ct, x) = 
        {place=ct.place; value = T.Body ({place = ct.place; value = T.FieldDeclaration {
            annotations = [T.Visibility T.Public; T.Final];
            type0 = fctype ct;
            name  = x;
            body  = None;
        }})}
    in

    let fields = List.map generate_field args in

    let generate_constructor_stmt (ct, x) : T.stmt =  
        { place = ct.place; value =
        T.ExpressionStmt( 
            { place = ct.place; value =
            T.AssignExpr( 
                { place = ct.place; value = T.AccessExpr (
                    { place = ct.place; value =T.ThisExpr}, { place = ct.place; value =T.VarExpr x})
                },
                T.AssignOp,
                { place = ct.place; value = T.VarExpr x}
            )
            }
        )
        }
    in

    let constructor = 
        { place; value=T.Body ({place; value=T.MethodDeclaration {
            annotations = [];   
            ret_type    = None;
            name        = name;
            parameters  = List.map finish_arg args;
            body        = List.map generate_constructor_stmt args
            ;
        }})}
    in

    T.Body ({place; value = T.ClassOrInterfaceDeclaration {
        isInterface         = false;
        annotations         = [T.Visibility (finish_visibility vis); T.Static; T.Final];
        name                = name;
        parameters          = []; 
        extended_types      = [];
        implemented_types   = [ {place; value=T.TAtomic "Command"} ]; 
        body                = constructor::fields
    }})
and fevent e : T.str_items = finish_place finish_event e

and finish_arg ((ctype,variable):(S.ctype * Atom.atom)) : T.parameter =
    (fctype ctype, variable)
and finish_method place ({annotations; ret_type; name; body; args; is_constructor}: S._method0) : T._body = 
    match body with
    | S.AbstractImpl stmts ->
        T.MethodDeclaration {
            annotations = finish_annotations annotations;   
            ret_type    = if is_constructor then None else Some (fctype ret_type);
            name        =  name;
            parameters  = List.map finish_arg args;
            body        = List.map fstmt stmts;
        }
    | S.BBImpl bbterm ->
        let body = T.RawStmt (
            if bbterm.value.template then
                let jingoo_args = Hashtbl.create (List.length args) in
                let aux_arg (ct,x)= 
                    let buffer = Buffer.create 64 in
                    Lg.Output.output_arg (Format.formatter_of_buffer buffer)(ct, x);
                    Hashtbl.add jingoo_args (Atom.hint x) (Jg_types.Tstr (Buffer.contents buffer)) 
                in
                List.iter aux_arg (List.map finish_arg args);

                let jingoo_ret_type = 
                    let buffer = Buffer.create 64 in
                    Lg.Output.ojtype (Format.formatter_of_buffer buffer) (fctype ret_type);
                    Buffer.contents buffer 
                in

                Jg_template.from_string bbterm.value.body ~models:[
                    ("name", Jg_types.Tstr (Atom.to_string name));
                    ("ret_type", Jg_types.Tstr jingoo_ret_type);
                    ("args", Jg_types.Thash jingoo_args)
                ]
            else
                bbterm.value.body
        ) in
        let body = [{place = bbterm.place; value = body }] in

        T.MethodDeclaration {
            annotations = finish_annotations annotations;   
            ret_type    = if is_constructor then None else Some (fctype ret_type);
            name;
            parameters  = List.map finish_arg args;
            body
        }
and fmethod m : T.str_items = {place=m.place; value=T.Body (finish_place finish_method m)}

and finish_actor place ({name; methods; states; events; nested_items}: S._actor): T._str_items =
    (* At most one constructor *)
    assert( List.length (List.filter (function (m:S.method0) -> m.value.is_constructor) methods) <= 1);

    (** FIXME public/protected/private should parametrized*)

    let extended_type = T.ClassOrInterfaceType (
        Atom.fresh_builtin "AbstractBehavior", 
        [ { 
            place;
            value = T.TAtomic ((Atom.p_to_string builtin_eval name)^".Command")
        }]) in
    let extended_type = {place; value = extended_type} in

    let body : T.str_items list ref = ref [] in
    (* FIXME issue with the type of body*)
    body := !body @ [{place; value=T.Comments (IR.LineComment "Actor state")}];
    body := !body @ (List.flatten (List.map finish_state states));
    body := !body @ [{place; value=T.Comments (IR.LineComment "Actor events")}];
    body := !body @ (List.map fevent events);
    body := !body @ [{place; value=T.Comments (IR.LineComment "Actor internal logics")}];
    body := !body @ (List.map fmethod methods);
    body := !body @ [{place; value=T.Comments (IR.LineComment "Nested structures")}];
    body := !body @ (List.map fterm nested_items);

    T.Body ({ place; value = T.ClassOrInterfaceDeclaration {
        isInterface= false;
        annotations = [T.Visibility T.Public];
        name=name;
        parameters = [];
        extended_types = [extended_type] ;
        implemented_types = [];
        body = !body 
    }})
and factor a : T.str_items = finish_place finish_actor a

and finish_term place : S._term -> T._str_items = function
    | Comments c -> T.Comments c
    | Import s -> T.JModule ({place; value=T.ImportDirective s})
    | Actor a  -> (factor a).value
    | Event e  ->  (fevent e).value
    | Stmt s -> T.Stmt (fstmt s)
    | Class x -> T.Body (
        { 
            place; 
            value = T.ClassOrInterfaceDeclaration {
                isInterface = false;
                annotations = [];
                name = x;
                parameters = []; 
                extended_types = [];
                implemented_types = [];
                body = [] 
            }
        }
    )
    | ClassOrInterfaceDeclaration cdcl ->T.Body (
        { 
            place;
            value = T.ClassOrInterfaceDeclaration {
                isInterface = cdcl.isInterface;
                annotations = finish_annotations cdcl.annotations;
                name = cdcl.name;
                parameters = []; 
                extended_types = [];
                implemented_types = [];
                body = List.map fterm cdcl.body 
            }
        }
    ) 
    | MethodDeclaration m -> (fmethod m).value
    | RawClass (x, raw) ->  T.Body (
        { 
            place;
            value = T.ClassOrInterfaceDeclaration {
                isInterface = false;
                annotations = [];
                name = x;
                parameters = []; 
                extended_types = [];
                implemented_types = [];
                body = [{ place=raw.place; value=T.Raw raw.value}] 
            }
        }
    ) 
    | TemplateClass raw -> T.Raw raw.value (* TODO FIXME we should  keep raw.place here *) 
and fterm t : T.str_items = finish_place finish_term t

and finish_program (program:S.program): (Fpath.t * T.program) Seq.t = 
    (*TODO finish entrypoint/system*)
    program
    |> split_akka_ast_to_files
    |> Seq.map (function file, terms -> file, List.map fterm terms)

let finish_ir_program (ir_program: Plugin.S.program) : (Fpath.t * T.program) Seq.t =
    ir_program
    |> Rt.Finish.finish_program  
    |> dump "Runtime AST" Rt.Ast.show_program  
    |> finish_program
    |> Seq.map (function file, program -> file, dump "Language AST" Lg.Ast.show_program program)


(** Output program*)
let output_program build_dir ir_program =
    ir_program
    |> finish_ir_program
    |> Seq.iter (function (file,program) -> Lg.Output.output_program (Fpath.append build_dir file) program)

let jingoo_env () = [
    ("compiler_version", Jg_types.Tstr Config.version);
    ("compiler_debug", Jg_types.Tbool (Config.debug ()));
    ("compiler_keep_ghost", Jg_types.Tbool (Config.keep_ghost ()));

    ("project_name", Jg_types.Tstr (Config.project_name ()));
    ("author", Jg_types.Tstr (Config.author ()));
]

let custom_template_rules () = [
    (Fpath.v "application.conf.j2", jingoo_env (), List.fold_left Fpath.add_seg (Fpath.v "src/main/resources")  [Config.author (); Config.project_name (); "application.conf"]);
]
let custom_external_rules () = []

    