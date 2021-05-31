open Core
open Ast
open Format 

let failwith s = failwith "[Plg=Java] %s" s
(** FIXME Factorise into a OCaml module since OutputMl will do the smae too *)
let builtin_translation = List.to_seq []
module BuiltinMap = Map.Make(String)                     
let builtin_eval = 
  let translation = BuiltinMap.of_seq builtin_translation in
  function x ->   
  try
    BuiltinMap.find (Atom.value x) translation
  with Not_found ->Atom.value x

let join (separator:string) output_elt out= function 
    | [] -> ()
    | [y] -> output_elt out y;
    | y::ys -> begin
        output_elt out y;    
        List.iter (function y -> fprintf out "%s%a" separator output_elt y) ys   
    end

let rec output_var out (x:Atom.atom)=
  if Atom.builtin x then fprintf out "%s" (builtin_eval x)
  else
    fprintf out "%s%d" (Atom.hint x) (Atom.identity x)
and output_vars out: variable list -> unit = join " , " output_var out

let output_capitalize_var out (x:Atom.atom)=
  if Atom.builtin x then fprintf out "%s" (String.capitalize_ascii (builtin_eval x))
  else
    fprintf out "%s%d" (String.capitalize_ascii (Atom.hint x)) (Atom.identity x) 

let wrapper_newline out f = function x -> ((f out) x; fprintf out "\n")

(** Akka specific *)
let rec output_unop out = function
    | IR.Not -> fprintf out " ! "
    | IR.UnpackResult -> raise (Core.Error.DeadbranchError "output_unop : unpacking a result should have been encoded as a method call when translating from Akka to Java AST.")
and output_binop out = function
    | IR.And -> fprintf out " && "
    | IR.Equal -> fprintf out " == "
    | IR.GreaterThan -> fprintf out " > "
    | IR.GreaterThanEqual -> fprintf out " >= "
    | IR.LessThan -> fprintf out "<"
    | IR.LessThanEqual -> fprintf out "<="
    | IR.Plus -> fprintf out " + "
    | IR.Minus -> fprintf out " - "
    | IR.Mult -> fprintf out " * "
    | IR.Divide -> fprintf out " / "
    | IR.Or -> fprintf out " || "
and output_assignop out : assign_operator -> unit = function
    | AssignOp -> fprintf out "="
and output_literal out : literal -> unit = function
    | EmptyLit ->  fprintf out " () "
    | BoolLit b -> fprintf out " %s " (Bool.to_string b)
    | FloatLit f -> fprintf out " %f " f 
    | IntLit i -> fprintf out " %d " i 
    | StringLit str -> fprintf out "%s" str 

and output_expr out : expr -> unit = function
    | AccessExpr (e1,e2) -> fprintf out "%a.%a" output_expr e1 output_expr e2
    | AppExpr (e1, es) -> 
        let rec output_exprs out = function 
            | [] -> ()
            | [e] -> output_expr out e                   
            | e::es -> begin
                output_expr out e;    
                List.iter (function e -> fprintf out " , "; output_expr out e) es   
            end
        in
        fprintf out "%a(%a)" output_expr e1 output_exprs es
    | AssertExpr e -> fprintf out "assert(%a)" output_expr e                   
    | AssignExpr (e1, op, e2) -> fprintf out "%a %a %a" output_expr e1 output_assignop op output_expr e2
    | BinaryExpr (e1, op, e2) -> fprintf out "%a %a %a" output_expr e1 output_binop op output_expr e2
    | LiteralExpr lit -> output_literal out lit
    | LambdaExpr (variables, stmt) -> fprintf out "( (%a) -> { %a } )" output_vars variables output_stmt stmt
    | ThisExpr -> fprintf out "this";
    | UnaryExpr (op, e) -> fprintf out "%a %a" output_unop op output_expr e
    | VarExpr x -> output_var out x             
and output_exprs out: expr list -> unit = List.iter (output_expr out)

and output_comments out : IR._comments -> unit = function
    | IR.BlockComment str -> fprintf out "/* %s */\n" str
    | IR.DocComment str -> fprintf out "/** %s */\n" str
    | IR.LineComment str -> fprintf out "// %s\n" str

and output_stmt out : stmt -> unit = function
    | BlockStmt stmts -> fprintf out "{\n%a\n}" (function out -> List.iter (output_stmt out)) stmts
    | BreakStmt -> fprintf out "break;\n"
    | CommentsStmt c -> output_comments out c 
    | ContinueStmt -> fprintf out "continue;\n"
    | ExpressionStmt e -> fprintf out "%a;\n" output_expr e 
    | IfStmt (e, stmt1, None) -> fprintf out " if(%a){\n%a\n}\n" output_expr e output_stmt stmt1
    | IfStmt (e, stmt1, Some stmt2) -> fprintf out " if(%a){\n %a \n}else{\n %a \n}\n" output_expr e output_stmt stmt1 output_stmt stmt2
    | NamedExpr (jt, x, e) -> fprintf out "%a %a = %a;\n" output_jtype jt output_var x output_expr e
    | ReturnStmt e -> fprintf out "return %a;\n" output_expr e

and output_annotation out: annotation -> unit = function
    | Visibility vis -> output_visibility out vis 
    | Static -> fprintf out "static" 
    | Final -> fprintf out "final"
and output_annotations out = List.iter (output_annotation out)

and output_visibility out: visibility -> unit = function
    | Public -> fprintf out "public"
    | Private -> fprintf out "private"
    | Protected -> fprintf out "protected"

and output_arg out (jt, var): unit =
    fprintf out "%a %a" output_jtype jt output_var var
and output_args out: (jtype * variable) list -> unit = join " , " output_arg out  

and output_body out : body -> unit = function 
    | ClassOrInterfaceDeclaration cl -> 
        let output_cl_params out = function 
            | [] -> ()
            | params -> fprintf out "(%a)" output_type_params params 
        in
        let output_cl_extended out = function
            | [] -> ()
            | params -> fprintf out " extends %a" output_type_params params
        in
        let output_cl_implements out = function
            | [] -> ()
            | params -> fprintf out " implements %a" output_type_params params
        in
    fprintf out "%a class %a%a%a%a {\n@[<2>%a@]\n}\n" output_annotations cl.annotations output_var cl.name output_cl_params cl.parameters output_cl_extended cl.extended_types output_cl_implements cl.implemented_types output_items cl.body
    | MethodDeclaration m ->
    fprintf out "%a %a %a(%a) {\n@[<4>%a@]\n}\n" output_annotations m.annotations (fun out opt-> ignore (Option.map (output_jtype out) opt)) m.ret_type output_var m.name output_args m.parameters output_stmt m.body 

and output_jmodule out : jmodule -> unit = function
    | ImportDirective str -> fprintf out "import %s;"str 

and output_type_params out : jtype list -> unit = join " , " output_jtype out   

and output_jtype out : jtype -> unit = function
    | ClassOrInterfaceType (x, []) ->  output_var out x    
    | ClassOrInterfaceType (x, params) -> fprintf out {|%a<%a>|} output_var x output_type_params params
    | TAtomic str -> fprintf out {|%s|} str
    | TVar x -> output_var out x 

and output_item out : str_items -> unit = function
    | Body b -> ((wrapper_newline out) output_body) b 
    | Comments c -> ((wrapper_newline out) output_comments) c 
    | JModule jm -> ((wrapper_newline out) output_jmodule) jm 
    | Stmt stmt -> ((wrapper_newline out) output_stmt) stmt 
    | JType jt -> ((wrapper_newline out) output_jtype) jt 
and output_items out : str_items list -> unit = 
    List.iter (output_item out)

let output_program build_dir items : unit =
    let out = open_out (FilePath.make_filename (build_dir @ ["main.java"])) in
    let out = formatter_of_out_channel out in (* TODO propagate the change from Prtinf to format to all *)
    output_items out items