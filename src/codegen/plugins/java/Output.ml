open Core
open Ast
open Format 
open Easy_logging

let name = "Java"
let logger = Logging.make_logger ("_1_ compspec.plg."^name) Debug [];;


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

let pp_list sep ppx out l = 
    let pp_sep out () = Format.fprintf out "%s@ " sep in
    Format.pp_print_list ~pp_sep ppx out l

let pp_break_list sep ppx out l = 
    let pp_sep out () = Format.fprintf out "%s@;" sep in
    Format.pp_print_list ~pp_sep ppx out l

let rec output_var out (x:Atom.atom)=
  if Atom.is_builtin x then fprintf out "%s" (builtin_eval x)
  else
    fprintf out "%s%d" (Atom.hint x) (Atom.identity x)
and output_vars out: variable list -> unit = pp_list ", " output_var out

let output_capitalize_var out (x:Atom.atom)=
  if Atom.is_builtin x then fprintf out "%s" (String.capitalize_ascii (builtin_eval x))
  else
    fprintf out "%s%d" (String.capitalize_ascii (Atom.hint x)) (Atom.identity x) 

let wrapper_newline out f = function x -> ((f out) x; fprintf out "\n")

let rec output_placed out output_value ({ AstUtils.place ; AstUtils.value}: 'a AstUtils.placed) = 
    let comment = AstUtils.BlockComment (Error.show ~display_line:false place) in
    fprintf out "%a@;%a" output_comments comment output_value value
and output_comments out : AstUtils._comments -> unit = function
    | AstUtils.BlockComment str -> fprintf out "/* %s@;*/" str
    | AstUtils.DocComment str -> fprintf out "/** %s@;*/" str
    | AstUtils.LineComment str -> fprintf out "// %s" str



(** Akka specific *)
let rec output_unop out = function
    | AstUtils.Not -> pp_print_string out "!"
    | AstUtils.UnpackOrPropagateResult -> raise (Core.Error.DeadbranchError "output_unop : unpacking a result should have been encoded as a method call when translating from Akka to Java AST.")
and output_binop out = function
    | AstUtils.And -> pp_print_string out "&&"
    | AstUtils.Equal -> pp_print_string out "=="
    | AstUtils.GreaterThan -> pp_print_string out ">" 
    | AstUtils.GreaterThanEqual -> pp_print_string out ">="
    | AstUtils.LessThan -> pp_print_string out "<"
    | AstUtils.LessThanEqual -> pp_print_string out "<="
    | AstUtils.Plus -> pp_print_string out "+"
    | AstUtils.Minus -> pp_print_string out "-"
    | AstUtils.Mult -> pp_print_string out "*"
    | AstUtils.Divide -> pp_print_string out "/"
    | AstUtils.Or -> pp_print_string out "||"
and output_assignop out : assign_operator -> unit = function
    | AssignOp -> pp_print_string out "="
and output_literal out : _literal -> unit = function
    | BoolLit b -> pp_print_bool out b
    | FloatLit f -> pp_print_float out f 
    | IntLit i -> pp_print_int out i 
    | StringLit str -> fprintf out "\"%s\"" str 
    | VoidLit -> pp_print_string out "null" 
and oliteral out : literal -> unit = function lit ->
match Config.provenance_lvl () with
| Config.None | Config.Medium -> output_literal out lit.value
| Config.Full -> output_placed out output_literal lit 

and output_expr out (e, _): unit = 
match e with
    | AccessExpr (e1,e2) -> fprintf out "%a.%a" oexpr e1 oexpr e2
    | AccessMethod (e1,e2) -> fprintf out "%a::%a" oexpr e1 output_var e2
    | AppExpr (e1, es) -> 
        fprintf out "@[%a(@[<hv>@;<0 3>%a@])@]" oexpr e1 oexprs es
    | AssertExpr e -> fprintf out "assert(%a)" oexpr e                   
    | AssignExpr (e1, op, e2) -> fprintf out "%a %a %a" oexpr e1 output_assignop op oexpr e2
    | BinaryExpr (e1, op, e2) -> fprintf out "%a %a %a" oexpr e1 output_binop op oexpr e2
    | CastExpr (ct, ({value=LambdaExpr _,_} as lambda)) -> (* Otherwise Java error "error: lambda expression not expected here"*)
        fprintf out "( (%a) %a )" ojtype ct oexpr lambda 
    | CastExpr (ct, e) -> fprintf out "(%a) %a" ojtype ct oexpr e
    | LiteralExpr lit -> oliteral out lit
    | LambdaExpr (params, stmt) -> 
        let rec output_lambda_param out ((jt, x):jtype * variable) =
            match jt.value with
            | TUnknown -> output_var out x
            | _ -> fprintf out "%a %a" ojtype jt output_var x 
        in
        let output_lambda_params out = pp_list ", " output_lambda_param out in
        
        fprintf out "( (%a) -> { %a } )" output_lambda_params params ostmt stmt
    | NewExpr (e1, es) -> fprintf out "new @[%a(@[<hv>@;<0 3>%a@])@]" oexpr e1 oexprs es
    | ThisExpr -> pp_print_string out "this";
    | UnaryExpr (op, e) -> fprintf out "%a %a" output_unop op oexpr e
    | VarExpr x -> output_var out x             
    | RawExpr str -> pp_print_string out str
    | TernaryExpr (e1, e2, e3) -> fprintf out "%a ? %a : %a" oexpr e1 oexpr e2 oexpr e3 
and oexpr out : expr -> unit = function e ->
    match Config.provenance_lvl () with
    | Config.None | Config.Medium -> output_expr out e.value
    | Config.Full -> output_placed out output_expr e 
and oexprs out: expr list -> unit = pp_list ", " oexpr out

and obbbody out : blackbox_body -> unit = function 
    | Text t -> fprintf out "%s" t
    | Varda e -> oexpr out e
and output_bbterm out : _blackbox_term -> unit = pp_list " " obbbody out
and obbterm out (bbterm:blackbox_term) = 
    match Config.provenance_lvl () with
    | Config.None -> output_bbterm out bbterm.value
    | Config.Medium | Config.Full -> output_placed out output_bbterm bbterm


and output_stmt out : _stmt -> unit = function
    | BlockStmt stmts -> fprintf out "{@;@[<v 3>%a@]@;<0 -3>}" (pp_list "" ostmt) stmts
    | BreakStmt -> fprintf out "break;"
    | CommentsStmt c -> output_comments out c 
    | ContinueStmt -> fprintf out "continue;"
    | EmptyStmt -> ()
    | ExpressionStmt e -> fprintf out "%a;" oexpr e 
    | IfStmt (e, stmt1, None) -> fprintf out "if(@[<hv 3>%a@]){@;@[<v 3>@;%a@]@;}" oexpr e ostmt stmt1
    | IfStmt (e, stmt1, Some stmt2) -> fprintf out "if(@[<hv 3>%a@]){@;@[<v 3>@;%a@]@;}else{@;@[<v 3>@;%a@]@;}" oexpr e ostmt stmt1 ostmt stmt2
    | ForStmt (jt, x, e, stmt) -> 
        assert(jt.value <> TUnknown);
        fprintf out "for(@[<hv 3>%a %a : %a @]){@;@[<v 3>@;%a@]@;}" ojtype jt output_var x oexpr e ostmt stmt
    | NamedExpr (jt, x, Some e) -> 
        assert(jt.value <> TUnknown);
        fprintf out "%a %a = @[<hv>%a@];" ojtype jt output_var x oexpr e
    | NamedExpr (jt, x, None) -> 
        assert(jt.value <> TUnknown);
        fprintf out "%a %a;" ojtype jt output_var x 
    | ReturnStmt e -> fprintf out "return %a;" oexpr e
    | RawStmt str -> pp_print_string out str
    | BBStmt bb -> obbterm out bb
    | TryStmt  (stmt, branches) -> 
        fprintf out "try{@[<hv>%a@]}%a" 
            ostmt stmt
            (pp_list "" (fun out (ct, x, stmt) -> 
                fprintf out "catch (%a %a){@[<hv>%a@]}" 
                    ojtype ct   
                    output_var x
                    ostmt stmt
            )) branches
and ostmt out: stmt -> unit = function stmt ->
    match Config.provenance_lvl () with
    | Config.None | Config.Medium -> output_stmt out stmt.value
    | Config.Full -> output_placed out output_stmt stmt 

and output_decorator out: decorator -> unit = function
    | Override -> pp_print_string out "@Override" 
    | JsonCreator -> pp_print_string out "@JsonCreator" 
    | JsonProperty x -> fprintf out "@JsonProperty(\"%a\")" output_var x
and output_decorators out = function
    | [] -> ()
    | l -> fprintf out "@[<h>%a@] " (pp_list "@ " output_decorator) l

and output_annotation out: annotation -> unit = function
    | Visibility vis -> output_visibility out vis 
    | Static -> pp_print_string out "static" 
    | Final -> pp_print_string out "final"
and output_annotations out = function
    | [] -> ()
    | l -> fprintf out "@[<h>%a@] " (pp_list "" output_annotation) l

and output_visibility out: visibility -> unit = function
    | Public -> pp_print_string out "public"
    | Private -> pp_print_string out "private"
    | Protected -> pp_print_string out "protected"

and output_arg out (decorators, jt, var): unit =
    fprintf out "%a %a %a" (pp_list " " output_decorator) decorators  ojtype jt output_var var
and output_args out: (decorator list * jtype * variable) list -> unit = pp_list ", " output_arg out  

and output_body_v out : _body -> unit = function 
    | ClassOrInterfaceDeclaration cl -> 
        let output_cl_params out = function 
            | [] -> ()
            | params -> fprintf out "(%a)" output_type_params params 
        in
        let output_kind out = function 
        | true -> fprintf out "interface"
        | false -> fprintf out "class"
        in
        let output_cl_extended out = function
            | [] -> ()
            | params -> fprintf out " extends %a" output_type_params params
        in
        let output_cl_implements out = function
            | [] -> ()
            | params -> fprintf out " implements %a" output_type_params params
        in
        fprintf out 
            "%a %a%a%a%a {@;@[<v 3>@;%a@]@;}" 
            output_kind cl.isInterface
            output_var cl.name 
            output_cl_params cl.parameters
            output_cl_extended cl.extended_types
            output_cl_implements cl.implemented_types
            output_items cl.body
    | FieldDeclaration f -> 
        assert(f.type0.value <> TUnknown);
        fprintf out "%a %a%a;@;"
            ojtype f.type0
            output_var f.name
            (fun out opt-> ignore (Option.map (fprintf out " = %a" oexpr) opt)) f.body
    | MethodDeclaration m ->
        Option.map (function (jt:jtype) -> assert(jt.value <> TUnknown)) m.ret_type;
        List.iter (function ((_,jt,_):'a * jtype * 'b) -> assert(jt.value <> TUnknown)) m.parameters;
        fprintf out 
            "%a%a(@[<hv 3>%a@]) {@;@[<v 3>@;%a@]@;}"
            (fun out opt-> ignore (Option.map (fprintf out "%a " ojtype) opt)) m.ret_type 
            output_var m.name 
            output_args m.parameters 
            (pp_list "" ostmt) m.body 
and output_body out {annotations; decorators; v}: unit =
    fprintf out 
        "%a@;%a%a"
        output_decorators decorators 
        output_annotations annotations 
        output_body_v v
and obody out : body -> unit = function b ->
    match Config.provenance_lvl () with
    | Config.None -> output_body out b.value
    | Config.Medium | Config.Full -> output_placed out output_body b 

and output_jmodule out : _jmodule -> unit = function
    | ImportDirective str -> fprintf out "import %s;" str 
    | PackageDeclaration str -> fprintf out "package %s;" str
and ojmodule out : jmodule -> unit = function jm ->
    match Config.provenance_lvl () with
    | Config.None | Config.Medium -> output_jmodule out jm.value
    | Config.Full -> output_placed out output_jmodule jm 

and output_type_params out : jtype list -> unit = pp_list ", " ojtype out   

and output_jtype place out : _jtype -> unit = function
    | ClassOrInterfaceType (t, []) ->  ojtype out t  
    | ClassOrInterfaceType (t, params) -> fprintf out "%a<@[<hv>%a@]>" ojtype t output_type_params params
    | TArray t -> fprintf out "%a[]" ojtype t 
    | TAtomic str -> pp_print_string out str
    | TVar x -> output_var out x 
    | TAccess (t1, t2) -> fprintf out "%a.%a" ojtype t1 ojtype t2
    | TUnknown -> fprintf out "ErrorTUnknownRemains" 
    (* TODO uncomment
        raise (Error.PlacedDeadbranchError (place, "TUnknown can not be translated to Java code - it should have been resolved to a concrete type first or this is a mock type annotation that should not be translated to Java code."))
    *)
and ojtype out : jtype -> unit = function jt ->
    match Config.provenance_lvl () with
    | Config.None | Config.Medium -> output_jtype jt.place out jt.value
    | Config.Full -> output_placed out (output_jtype jt.place) jt 

and output_item out : _str_items -> unit = function
    | Body b -> obody out b 
    | Comments c -> output_comments out c 
    | JModule jm -> ojmodule out jm 
    | Stmt stmt -> ostmt out stmt 
    | JType jt -> ojtype out jt 
    | Raw str -> pp_print_string out str
    | BBItem bbitem -> obbterm out bbitem
and oitem out : str_items -> unit = function item ->
    match Config.provenance_lvl () with
    | Config.None -> output_item out item.value
    | Config.Medium | Config.Full -> output_placed out output_item item 
and output_items out : str_items list -> unit = pp_break_list "" oitem out 

let output_program package_name outpath items : unit =
    let outfile = (Fpath.to_string outpath)^".java" in
    let parentdir = (Fpath.parent outpath) in
    Utils.create_directory_hierarchy parentdir;

    let out = open_out outfile in
    let out = formatter_of_out_channel out in (* TODO propagate the change from Prtinf to format to all *)

    (* Add imports*)
    (* TODO define imports outside this function,   *)
    let mock_placed value =  { 
        AstUtils.place = Error.forge_place "Lg=Java/output_program" 0 0; 
        value
    } in
    let items = (List.map mock_placed [
        JModule (mock_placed (PackageDeclaration (String.lowercase_ascii package_name)));

        (* Java *)
        JModule (mock_placed(ImportDirective "java.util.*"));
        JModule (mock_placed(ImportDirective "java.time.Duration"));
        JModule (mock_placed(ImportDirective "java.util.concurrent.CompletableFuture"));
        JModule (mock_placed(ImportDirective "java.util.UUID"));
        JModule (mock_placed(ImportDirective "java.util.stream.Collectors"));
        JModule (mock_placed(ImportDirective "java.util.function.Function"));
        JModule (mock_placed(ImportDirective "java.util.function.BiFunction"));

        (* Jackson (serialization) *)
        JModule (mock_placed(ImportDirective "com.fasterxml.jackson.annotation.JsonProperty"));
        JModule (mock_placed(ImportDirective "com.fasterxml.jackson.annotation.JsonCreator"));


        (* Akka imports *)
        JModule (mock_placed(ImportDirective "akka.actor.typed.ActorRef"));
        JModule (mock_placed(ImportDirective "akka.actor.typed.Behavior"));
        JModule (mock_placed(ImportDirective "akka.actor.typed.javadsl.AbstractBehavior"));
        JModule (mock_placed(ImportDirective "akka.actor.typed.javadsl.ActorContext"));
        JModule (mock_placed(ImportDirective "akka.actor.typed.javadsl.Behaviors"));
        JModule (mock_placed(ImportDirective "akka.actor.typed.javadsl.Receive"));
        JModule (mock_placed(ImportDirective "akka.actor.typed.javadsl.TimerScheduler"));
        JModule (mock_placed(ImportDirective "akka.actor.typed.receptionist.Receptionist"));
        JModule (mock_placed(ImportDirective "akka.cluster.ClusterEvent"));
        JModule (mock_placed(ImportDirective "akka.cluster.typed.Cluster"));
        JModule (mock_placed(ImportDirective "akka.cluster.typed.Subscribe"));

        (* gRPC imports *)
        JModule (mock_placed(ImportDirective "akka.grpc.javadsl.ServiceHandler"));
        JModule (mock_placed(ImportDirective "akka.http.javadsl.model.HttpRequest"));
        JModule (mock_placed(ImportDirective "akka.http.javadsl.model.HttpResponse"));
        JModule (mock_placed(ImportDirective "java.util.concurrent.CompletionStage"));
        JModule (mock_placed(ImportDirective "akka.http.javadsl.Http"));
        JModule (mock_placed(ImportDirective "com.typesafe.config.Config"));
        JModule (mock_placed(ImportDirective "com.typesafe.config.ConfigFactory"));
        JModule (mock_placed(ImportDirective "akka.stream.SystemMaterializer"));
        JModule (mock_placed(ImportDirective "akka.stream.Materializer"));
        JModule (mock_placed(ImportDirective "akka.http.javadsl.ServerBinding"));
        JModule (mock_placed(ImportDirective "akka.actor.typed.ActorSystem"));
        JModule (mock_placed(ImportDirective "akka.actor.typed.javadsl.AskPattern"));

        (* Other dependencies *)
        JModule (mock_placed(ImportDirective "io.vavr.*"));
        JModule (mock_placed(ImportDirective "io.vavr.control.*"));

        (* Plugin (external) libs imports *)
        JModule (mock_placed(ImportDirective "com.lg4dc.*"));
        JModule (mock_placed(ImportDirective "com.lg4dc.timers.*"));
        JModule (mock_placed(ImportDirective "com.bmartin.*"));
    ])
    @ items in

    (* Configure formatter *)
    pp_set_margin out 80;
    pp_set_max_indent out 40;

    fprintf out "@[<v>%a@]@." output_items items