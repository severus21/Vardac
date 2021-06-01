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

let builtin_translation = List.to_seq []
module BuiltinMap = Map.Make(String)                     
let builtin_eval = 
  let translation = BuiltinMap.of_seq builtin_translation in
  function x ->   
  try
    BuiltinMap.find (Atom.value x) translation
  with Not_found ->Atom.value x


let rec finish_ctype : S.ctype -> T.jtype = function 
    | S.Atomic s -> T.TAtomic s 
    | S.Behavior x -> T.ClassOrInterfaceType  (Atom.fresh_builtin "Behavior", [T.TAtomic x]) 
    | S.TFunction (t1, t2) -> T.ClassOrInterfaceType  (Atom.fresh_builtin "Function", [finish_ctype t1; finish_ctype t2]) 
    | S.TList t1 -> T.ClassOrInterfaceType  (Atom.fresh_builtin "List", [finish_ctype t1])
    | S.TMap (t1, t2) -> T.ClassOrInterfaceType  (Atom.fresh_builtin "Map", [finish_ctype t1; finish_ctype t2])
    | S.TOption t1 -> T.ClassOrInterfaceType  (Atom.fresh_builtin "Optional", [finish_ctype t1]) 
    | S.TResult (t1, t2) -> T.ClassOrInterfaceType  (Atom.fresh_builtin "Result", [finish_ctype t1; finish_ctype t2])  (*TODO add include https://github.com/hdevalke/result4j*)
    | S.TSet t1 -> T.ClassOrInterfaceType  (Atom.fresh_builtin "Set", [finish_ctype t1])
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
        T.ClassOrInterfaceType (Atom.fresh_builtin cls_name, List.map finish_ctype cts)
    end 
    | S.TVar v -> T.TVar v
    | S.TVoid -> T.TAtomic "void"


let finish_visibility = function
    | S.Private -> T.Private
    | S.Protected -> T.Protected
    | S.Public -> T.Public

let finish_unop = Fun.id 
let finish_binop = Fun.id 

let rec finish_literal : S.literal -> T.literal= function
    | S.EmptyLit -> T.EmptyLit
    | S.BoolLit b -> T.BoolLit b
    | S.FloatLit f -> T.FloatLit f
    | S.IntLit f -> T.IntLit f
    | S.StringLit s -> T.StringLit s

and finish_expr : S.expr -> T.expr = function
    | S.AccessExpr (e1,e2) -> T.AccessExpr (finish_expr e1, finish_expr e2)
    | S.AssertExpr e -> T.AssertExpr (finish_expr e)                   
    | S.BinopExpr (e1, op, e2) -> T.BinaryExpr ( finish_expr e1, op, finish_expr e2) 
    | S.CallExpr (e1,e2) -> T.AppExpr(finish_expr e1, List.map finish_expr e2) 
    | S.CurrentContext -> T.AppExpr ((T.VarExpr (Atom.fresh_builtin "getContext")), [T.LiteralExpr T.EmptyLit])
    | S.CurrentSystem -> T.AccessExpr 
        ((T.AppExpr 
            ((T.VarExpr (Atom.fresh_builtin "getContext")),
            [T.LiteralExpr T.EmptyLit])
        ),
        (T.AppExpr
            ((T.VarExpr (Atom.fresh_builtin "getSystem")),
            [T.LiteralExpr T.EmptyLit])
        ))
    | S.LambdaExpr (variables, stmt) -> T.LambdaExpr (variables, finish_stmt stmt)
    | S.LitExpr lit -> LiteralExpr (finish_literal lit)
    | S.Spawn {context; actor_expr} -> T.AppExpr (finish_expr context, [finish_expr actor_expr])
    | S.This -> T.ThisExpr
    | S.UnopExpr (IR.UnpackResult, e) -> T.AppExpr ( T.AccessExpr (finish_expr e, T.VarExpr (Atom.fresh_builtin "unwrap")), []) (* TODO should return form the fct with the error or return the result*) 
    | S.UnopExpr (op, e) -> T.UnaryExpr (op, finish_expr e) 
    | S.VarExpr x -> T.VarExpr x             
and finish_stmt : S.stmt -> T.stmt = function
    | S.AssignExpr (e1, e2) -> T.ExpressionStmt (T.AssignExpr(finish_expr e1, T.AssignOp, finish_expr e2))
    | S.BlockStmt stmts -> T.BlockStmt (List.map finish_stmt stmts)
    | S.BreakStmt -> T.BreakStmt
    | S.CommentsStmt c -> T.CommentsStmt c
    | S.ContinueStmt -> T.ContinueStmt
    | S.ExpressionStmt e -> T.ExpressionStmt(finish_expr e)
    | S.IfStmt (e, stmt1, stmt2_opt) -> T.IfStmt (finish_expr e, finish_stmt stmt1, Option.map finish_stmt stmt2_opt)              
    | S.LetStmt (ct, x, None) -> T.NamedExpr (finish_ctype ct, x, T.VarExpr (Atom.fresh_builtin "null")) (* TODO FIXME maybe not the semantic that we want, we need to add this to the doc*)  
    | S.LetStmt (ct, x, Some e) -> T.NamedExpr (finish_ctype ct, x, finish_expr e) 
    | S.ReturnStmt e -> T.ReturnStmt (finish_expr e)
let rec finish_state : S.state -> T.str_items list = function
    | s when s.persistent -> failwith "TODO finish _state with persistency" 
    | s -> List.map (function x -> T.Stmt x) (List.map finish_stmt s.stmts)
and finish_event ({vis; name; kind; args}: S.event) :  T.str_items = failwith "TODO finish_event" (* TODO *) 
and finish_arg ((ctype,variable):(S.ctype * Atom.atom)) : T.parameter =
    (finish_ctype ctype, variable)
and finish_method ({vis; ret_type; name; body; args; is_constructor}: S.method0) : T.body = 
    T.MethodDeclaration {
        annotations = [T.Visibility (finish_visibility vis)];   
        ret_type    = if is_constructor then None else Some (finish_ctype ret_type);
        name        =  name;
        parameters  = List.map finish_arg args;
        body        = finish_stmt body;
    }
and finish_actor ({name; methods; states; events; nested_items}: S.actor): T.str_items =
    (** FIXME public/protected/private should parametrized*)

    let extended_type = T.ClassOrInterfaceType (Atom.fresh_builtin "AbstractBehavior", [T.TAtomic ((Atom.atom_to_str builtin_eval name)^".Command")]) in

    let body : T.str_items list ref = ref [] in
    (* FIXME issue with the type of body*)
    body := !body @ [T.Comments (IR.LineComment "Actor state")];
    body := !body @ (List.flatten (List.map finish_state states));
    body := !body @ [T.Comments (IR.LineComment "Actor events")];
    body := !body @ (List.map finish_event events);
    body := !body @ [T.Comments (IR.LineComment "Actor internal logics")];
    body := !body @ (List.map (function x -> T.Body (finish_method x)) methods);
    body := !body @ [T.Comments (IR.LineComment "Nested structures")];
    body := !body @ (List.map finish_term nested_items);

    T.Body (T.ClassOrInterfaceDeclaration {
        isInterface= false;
        annotations = [T.Visibility T.Public];
        name=name;
        parameters = [];
        extended_types = [extended_type] ;
        implemented_types = [];
        body = !body 
    })

and finish_term : S.term -> T.str_items = function
    | Comments c -> T.Comments c
    | Import s -> T.JModule (T.ImportDirective s)
    | Actor a  -> finish_actor a
    | Event e  ->  finish_event e
    | Stmt s -> T.Stmt (finish_stmt s)
    | Class x -> T.Body (
        T.ClassOrInterfaceDeclaration {
            isInterface = false;
            annotations = [];
            name = x;
            parameters = []; 
            extended_types = [];
            implemented_types = [];
            body = [] 
        }
    ) (*TODO FIXME x should not be a variable only*) 

and finish_program ({entrypoint; system; terms}: S.program) : T.program = 
    List.map finish_term terms 
    (*TODO finish entrypoint/system*)

let finish_ir_program (ir_program: Core.IR.program) : T.program =
    ir_program
    |> Rt.Finish.finish_program  
    |> dump "Runtime AST" Rt.Ast.show_program  
    |> finish_program
    |> dump "Language AST" Lg.Ast.show_program

(** Output program*)
let output_program build_dir ir_program =
    ir_program
    |> finish_ir_program
    |> Lg.Output.output_program build_dir

let jingoo_env () = [
    ("compiler_version", Jg_types.Tstr Config.version);
    ("compiler_debug", Jg_types.Tbool (Config.debug ()));
    ("compiler_keep_ghost", Jg_types.Tbool (Config.keep_ghost ()));

    ("project_name", Jg_types.Tstr (Config.project_name ()));
    ("author", Jg_types.Tstr (Config.author ()));
]

let init_build build_dir : unit = 
    let build_dir = FilePath.make_filename build_dir in (* FIXME do it after build_dir creation*)

    let externals = FileUtil.ls (FilePath.make_filename ["external"; name]) in
    let aux_external (location : FilePath.filename) = 
        FileUtil.cp [location] (FilePath.concat build_dir (FilePath.basename location))
    in
    List.iter aux_external externals; 

    let templates = FileUtil.ls (FilePath.make_filename ["templates"; name]) in
    let aux_template template =
        print_string template;
        print_newline ();
        let res = Jg_template.from_file template ~models:(jingoo_env ()) in  
        let destfile = FilePath.concat build_dir  
            (FilePath.basename (if FilePath.check_extension template "j2" then 
                FilePath.chop_extension template
            else
                template))
        in
        print_string destfile;
        print_newline ();

        let oc = open_out destfile in
        print_string destfile;
        print_newline ();
        Printf.fprintf oc "%s" res; 
        close_out oc;
    in
    List.iter aux_template templates
    