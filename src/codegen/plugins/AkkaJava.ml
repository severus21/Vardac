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

let cstate : Rt.Finish.collected_state ref = ref (Rt.Finish.empty_cstate ())


let fplace = (Error.forge_place "Plg=AkkaJava" 0 0)
let auto_place smth = {place = fplace; value=smth}

    (*
        TODO TODOC
        event e -> stage1 
        A -> A include stage1 
        event b -> stage2
        B -> B include stage1 et include stage2
        C e b -> C include stage1 et include stage 2

        + decoupage en mains

        TODO refactor
    *)

(*
    flag_stage_or_component (true=component, false=intermediate stage), 
    stage_name or component_name,
    file,
    imports,
    ast  
*)
type stage_kind = 
| ActorStage
| ClassOrInterfaceDeclarationStage
| EventStage
| AnonymousStage
and stage_entry = {
    kind: stage_kind;
    name: Atom.atom;
    sub_stages: stage_entry list;
    ast: Rt.Ast.term list; 

    imports: Rt.Ast.term list;
    file: Fpath.t option;
    package_name: string option;
    main: Target.maindef option;
    (* such that 
        import parents stages
        import substages

        such that ast rewritten to specify from which stage
        t -> stage_n.t
    *)
}
let print_stages stages =
    logger#error "Stage list";
    let rec _print_stages step = function
    | [] -> ()
    | stage::stages when stage.file = None ->
        logger#error "%s - Stage [%s] in ()" step (Atom.to_string stage.name); 
        _print_stages (step^"  ") stage.sub_stages;
        _print_stages step stages
    | stage::stages ->
        logger#error "%s - Stage [%s] in (%s)" step (Atom.to_string stage.name) (Fpath.to_string (Option.get stage.file));
        _print_stages (step^"  ") stage.sub_stages;
        _print_stages step stages
    in 
    _print_stages "" stages

(** Split Akka.AST in multiple file then convert it to Java AST *)
(* - One actor per file *)
(* map : filename java_ast *)
let split_akka_ast_to_files (target:Core.Target.target) (akka_program:Rt.Ast.program) : (string * Fpath.t * Rt.Ast.term list) list = 
    (*(* Some string -> implies that the file is a main file where string is the name of the main class/and of the file *)
    let tbl_files2program : (Fpath.t, string option * Rt.Ast.term list) Hashtbl.t = Hashtbl.create 256 in
    let add_to_files2program file main_name_opt terms = 
        try 
            let _main_name_opt, ast = Hashtbl.find tbl_files2program file in 

            (* _main_name_opt must be used since subsequent call with the main file will have a None parameter *)
            match _main_name_opt with
            | Some _ -> Hashtbl.replace tbl_files2program file (_main_name_opt, (terms @ ast))
            | None -> 
                (* 
                    Make the effect of the  fct on tbl_files2program idempotent when processing multiple time the same components
                    Since it will be called 1 time per main of the target - potentially on the same AST => multiple call for a component
                *)
                () 

        with | Not_found -> Hashtbl.add tbl_files2program file (main_name_opt, terms) 
    in
    *)
    let generation_dir = List.fold_left Fpath.add_seg (Fpath.v "src/main/java")  [Config.author (); Config.project_name ()] in

    (********* Stage spliting *********)

    let extract_imports terms : Rt.Ast.term list * Rt.Ast.term list = 
        let rec _extract_imports : Rt.Ast.term list -> Rt.Ast.term list * Rt.Ast.term list = function
        | [] -> [], []
        | t::ts -> begin
            let imports, others = _extract_imports ts in
            match t.value with
            | Rt.Ast.Import _ -> t::imports, others 
            | _ -> imports, t::others
        end
        in
        let imports, others =  _extract_imports terms in
        imports, others 
    in
    
    let wrap_stage_ast (stage: stage_entry) : stage_entry =

        let wrap_main (_main:Rt.Ast.method0) = auto_place ( Rt.Ast.MethodDeclaration (auto_place {
            Rt.Ast.annotations = [Rt.Ast.Visibility Rt.Ast.Public; Rt.Ast.Static];
            ret_type = auto_place Rt.Ast.TVoid;    
            name = Atom.fresh_builtin "main";
            args= [auto_place (Rt.Ast.Atomic "String[]"), Atom.fresh_builtin "args"];
            is_constructor = false;
            body = _main.value.body 
        })) in

        let body = match stage.main with
        | None -> stage.ast
        | Some mdef -> (* wrap inside a main function *)
            logger#error "stage %s searching for %s" (Atom.to_string stage.name) mdef.entrypoint;
            let rec _extract_method : Rt.Ast.term list -> bool * Rt.Ast.term list * Rt.Ast.method0 option * Rt.Ast.term list = function
            | [] -> false, [], None, []
            | [{value=Rt.Ast.Actor a;place=_place}] when (Atom.hint a.value.name) = mdef.component -> 
                let _, xs, Some _main, ys = _extract_method (List.map 
                    (function m -> auto_place(Rt.Ast.MethodDeclaration m))
                    a.value.methods) in
                let transform = function | {value=Rt.Ast.MethodDeclaration m;} -> m in
                let xs, ys = List.map transform xs, List.map transform ys in 
                let {value=Rt.Ast.MethodDeclaration wraped_main; place=_} = wrap_main _main in
                true, [{
                    value=Rt.Ast.Actor {
                        place=a.place;
                        value={a.value with methods = xs@[wraped_main]@ys}
                    };
                    place=_place
                };], None, []
            | ({value=Rt.Ast.MethodDeclaration m;} as t)::xs when (Atom.hint m.value.name) = mdef.entrypoint -> 
                true, [], Some m, xs 
            | x::xs ->
                let flag, ys1, t, ys2 = _extract_method xs in
                flag, x::ys1, t, ys2
            in

            let flag,xs, _main_opt, ys = _extract_method stage.ast in
            if flag = false then Error.error target.place "Entrypoint [%s::%s] not found" mdef.component mdef.entrypoint;

            match _main_opt with
            | Some _main -> xs @ [ wrap_main _main ] @ ys 
            | None -> xs @ ys
        in
        let stage = { stage with ast = body } in

        if stage.kind <> AnonymousStage then stage
        else 
            let imports, others = extract_imports stage.ast in
            if others = [] then stage
            else begin 
            {
                stage with ast = 
                imports @ 
                [
                    auto_place (Rt.Ast.ClassOrInterfaceDeclaration {
                        isInterface = false;
                        annotations = [Rt.Ast.Visibility Rt.Ast.Public];
                        name = stage.name;
                        extended_types = [];
                        implemented_types = [];
                        body = others 
                    })
                ]
            }
            end
    in



    (*
        @param acc_current_stage - list of terms composing the current term (in reverse order)
    *)
    let group_per_stage_or_component terms : stage_entry list = 
        let rec _group_per_stage_or_component acc_current_stage : Rt.Ast.term list -> stage_entry list = 
            let stageofacc () = 
                let stage_name = Atom.fresh "Stage" in
                {
                    kind = AnonymousStage;
                    name = stage_name;
                    ast = List.rev acc_current_stage; 
                    sub_stages = [];
                    
                    imports = [];
                    file = None;
                    package_name = None;
                    main = None;
                }
            in    

            function 
            | [] -> [stageofacc ()]
            | term::ts -> begin
                match term.value with
                (* the events of an actor should not be defined externaly
                | Rt.Ast.Event e -> begin
                    (* No sub-stages *)
                    
                    let event_stage = {
                        kind = EventStage;
                        name = e.value.name;
                        ast = [term];
                        sub_stages = [];

                        imports = [];
                        file = None;
                        main = None;
                    } in 
                    (stageofacc ()) :: event_stage :: (_group_per_stage_or_component [] ts)
                end
                *)
                | Rt.Ast.ClassOrInterfaceDeclaration cid -> begin
                    (* No sub-stages *)
                    
                    let cid_stage = {
                        kind = ClassOrInterfaceDeclarationStage;
                        name = cid.name;
                        ast = [term];
                        sub_stages = [];

                        imports = [];
                        file = None;
                        package_name = None;
                        main = None ;
                    } in 
                    (stageofacc ()) :: cid_stage :: (_group_per_stage_or_component [] ts)
                end
                | Rt.Ast.Actor a -> begin
                    let sub_stages = 
                        _group_per_stage_or_component [] a.value.nested_items
                    in
                    let sub_stages = List.filter (function stage -> stage.ast <> []) sub_stages in

                    let actor_stage = {
                        kind = ActorStage;
                        name = a.value.name;
                        ast = [{
                            place = term.place;
                            value = Rt.Ast.Actor { 
                                place = a.place;
                                value = {a.value with nested_items = [] }
                            }
                        }];
                        sub_stages = sub_stages;

                        imports = [];
                        file = None;
                        package_name = None;
                        main = None;
                    } in 
                    (stageofacc ()) :: actor_stage :: (_group_per_stage_or_component [] ts)
                end
                | _ -> _group_per_stage_or_component (term::acc_current_stage) ts
            end 
        in
        List.filter (function stage -> stage.ast <> []) (_group_per_stage_or_component [] terms)
    in

    let stages = group_per_stage_or_component akka_program.terms in

    let rec external_binders_of_stage stage : Rt.Ast.variable list = 
        let aux t : Rt.Ast.variable list = 
            match t.value with
            | Rt.Ast.Actor a -> [a.value.name]
            | Rt.Ast.ClassOrInterfaceDeclaration cid when (Atom.hint cid.name) = "Stage"-> begin 
                let mock_stage = {stage with ast = cid.body } in
                external_binders_of_stage mock_stage
            end
            | Rt.Ast.ClassOrInterfaceDeclaration cid -> [cid.name]
            | Rt.Ast.Event e -> [e.value.name]
            | Rt.Ast.Stmt {value = LetStmt (_,x,_);} -> [x]
            | t -> []
        in

        List.flatten (List.map aux stage.ast)
    in

    let apply_rename_stage renaming stage = 
        {stage with ast = List.map (Rt.Ast.apply_rename_term renaming) stage.ast}
    in

    (* No clash of variables - since they are unique -> Atom *)
    let rec rename_stages : stage_entry list -> stage_entry list = 
        (*let base_rename = (Printf.sprintf "%s.%s" (Config.author ()) (Config.project_name ())) in*)
        let hydrate_state stage state x = 
            Hashtbl.add state x (
                if stage.name = x then (
                    Atom.refresh_hint x (Option.get stage.package_name^"."^(Atom.hint x))
                ) else (
                    Atom.refresh_hint x (Option.get stage.package_name^"."^(Atom.to_string stage.name)^"."^(Atom.hint x))
                )
            ) 
        in

    function
    | [] -> []
    | stage :: stages ->
        (*** rename subsequent stages - that may depends of the variables binded by the current stage ***)
        let to_rename1 = external_binders_of_stage stage in
        let _state1 : (Atom.atom, Atom.atom) Hashtbl.t = Hashtbl.create (List.length to_rename1) in
        List.iter (hydrate_state stage _state1) to_rename1;
        (*List.iter (function y -> Hashtbl.add _state1 y (Atom.refresh_hint y (base_rename^"."^(if Atom.hint stage.name = "Stage" then String.capitalize_ascii (Atom.to_string stage.name) else (Atom.to_string stage.name) )^"."^(Atom.hint y)))) to_rename1;*)
        let renaming1 (x:Atom.atom) : Atom.atom = 
            match Hashtbl.find_opt _state1 x with 
            | None -> x
            | Some new_x -> new_x
        in

        let stages = List.map (apply_rename_stage renaming1) stages in

        (*** Rename current stage ast according to variable binded by substages ***)
        let make_subrenaming sub_stage =
            let to_rename2 = external_binders_of_stage sub_stage in
            let _state2 : (Atom.atom, Atom.atom) Hashtbl.t  = Hashtbl.create (List.length to_rename2) in

            List.iter (hydrate_state sub_stage _state2) to_rename2;
            (*List.iter (function y -> Hashtbl.add _state2 y (Atom.refresh_hint y (base_rename^"."^(if Atom.hint stage.name = "Stage" then String.capitalize_ascii (Atom.to_string sub_stage.name) else (Atom.to_string sub_stage.name) )^"."^(Atom.hint y)))) to_rename2;*)
            let renaming2 (x:Atom.atom) : Atom.atom = 
                match Hashtbl.find_opt _state2 x with 
                | None -> x
                | Some new_x -> new_x
            in
            renaming2
        in 

        let stage = List.fold_left (fun current_stage sub_stage -> apply_rename_stage (make_subrenaming sub_stage) current_stage) stage stage.sub_stages in

        (*** Renaming sub_stages with respect to each others ***)
        let stage = { stage with sub_stages = rename_stages stage.sub_stages} in


        (*** Recursive call on the tail ***)
        stage::(rename_stages stages)
        (* TODO deals with children*)
    in


    let generate_file_of_stage (parent_opt:stage_entry option) stage = 
        let parent_dir = match parent_opt with
            | None -> generation_dir
            | Some parent -> 
                assert( parent.file <> None);
                (Fpath.parent (Option.get parent.file))
        in

        if stage.sub_stages = [] then 
            (* parentdir/stage.java*)
            Fpath.add_seg parent_dir (Atom.to_string stage.name)
        else 
            (* parentdir/main_actor/{main_actor.java, nested_actor1.java, nested_actor_n.java}
            where main_actor will only store the glue and not the subactor/components
            *)
            Fpath.add_seg (Fpath.add_seg parent_dir (String.lowercase_ascii (Atom.to_string stage.name)))  (Atom.to_string stage.name)
    in

    (* Generating stage imports and file name 
        @param package_name - package_name of the parent stage
        @param imports - list of imports from predecessing stages (in reverse order)
        @return - (import list of the last processed stage, stages annotated with imports)
    *)
    let rec hydrate_stages (package_name:string) (imports:Rt.Ast.term list) parent_opt : stage_entry list -> Rt.Ast.term list * stage_entry list= function
    | [] -> imports, []
    | stage :: stages ->
        let stage = {stage with file = Some (generate_file_of_stage parent_opt stage)} in
        let sub_imports, sub_stages = hydrate_stages (package_name^"."^(Atom.to_string stage.name)) imports (Some stage) stage.sub_stages in
        let stage = { stage with 
            imports = List.rev sub_imports;
            package_name = if stage.sub_stages = [] then Some package_name else Some (package_name^"."^(String.lowercase_ascii (Atom.to_string stage.name)));
            sub_stages;
            main = (
                match stage.kind with 
                | ActorStage ->    
                    logger#error "hydrate main %s" (Atom.to_string stage.name);
                    if parent_opt = None then logger#error "eee %s" (Atom.to_string stage.name);
                    List.find_opt (function (main:Target.maindef) -> main.component = Atom.hint stage.name && parent_opt = None) target.value.codegen.mains
                | _ ->
                   None 
            )
        } in

        (* Design choice: subcomponents/stages are private - i.e. do not reuse sub_imports for stages*)
        let current_import = 
            let base_import = (Printf.sprintf "%s.%s" (Config.author ()) (Config.project_name ())) in
            if stage.kind <> AnonymousStage then
                auto_place (Rt.Ast.Import (base_import^"."^(String.lowercase_ascii (Atom.to_string stage.name))^"."^(Atom.to_string stage.name)))
            else
                auto_place (Rt.Ast.Import (base_import^"."^(String.lowercase_ascii (Atom.to_string stage.name))^".*"))
        in

        (* TODO Get ride of import logic *) 
        (*let imports = current_import::imports in*)
       
        let _, stages = (hydrate_stages package_name imports parent_opt stages) in
        imports, stage::stages
    in
   
    (* Stages with imports and files *)
    let _, stages = hydrate_stages (Printf.sprintf "%s.%s" (Config.author ()) (Config.project_name ())) [] None stages in

    (* Wrap stages *)
    let rec wrap_stage stage =
        let stage = wrap_stage_ast stage in
        { stage with sub_stages = List.map wrap_stage stage.sub_stages; }
    in
    let stages = List.map wrap_stage stages in


    (* TODO Add stages for mains ?? *)

    let put_imports_first terms : Rt.Ast.term list =
        let imports, others = extract_imports terms in
        imports @ others
    in
    
    (* Renaming needs that package_name is set for each stage *)
    let stages = rename_stages stages in

    (* Generate the output *)
    let rec flatten_stages : stage_entry list -> (string * Fpath.t * Rt.Ast.term list) list = function
    | [] -> []
    | stage::stages -> 
        let tmps = flatten_stages stage.sub_stages in
        let new_ast = put_imports_first (stage.imports@stage.ast) in 

        (Option.get stage.package_name, (Option.get stage.file), new_ast) :: tmps @ (flatten_stages stages)
    in
    flatten_stages stages

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
    | S.TFunction (t1, t2) -> T.ClassOrInterfaceType  (Atom.fresh_builtin "Function", [fctype t1; fctype t2]) 
    | S.TList t1 -> T.ClassOrInterfaceType  (Atom.fresh_builtin "List", [fctype t1])
    | S.TMap (t1, t2) -> T.ClassOrInterfaceType  (Atom.fresh_builtin "Map", [fctype t1; fctype t2])
    | S.TOption t1 -> T.ClassOrInterfaceType  (Atom.fresh_builtin "Optional", [fctype t1]) 
    | S.TAccess (t1, t2) -> T.TAccess (fctype t1, fctype t2)
    | S.TParam ({value=S.TVar x;_}, t_args) -> T.ClassOrInterfaceType (x, List.map fctype t_args)
    | S.TParam _ -> failwith "Akka -> java, tparam with non VAR ctype is not yet supported" 
    | S.TResult (t1, t2) -> 
        (* 
            Encoding as the Either<left, right> for Vavr,
            left denotes the Err and right denotes the Ok
        *)
        T.ClassOrInterfaceType  (Atom.fresh_builtin "Either", [fctype t2; fctype t1]) 
    | S.TSet t1 -> T.ClassOrInterfaceType  (Atom.fresh_builtin "Set", [fctype t1])
    | S.TTuple cts -> begin 
        let cls_name = match List.length cts with
        | n when n < 9 -> "Tuple"^(string_of_int n) 
        | _ -> failwith "Tuple with length > 8 are not supported by the Vavr library."
        in 
        T.ClassOrInterfaceType (Atom.fresh_builtin cls_name, List.map fctype cts)
    end 
    | S.TVar v -> T.TVar v
    | S.TVoid -> T.TAtomic "Void"
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
    | S.VoidLit -> T.VoidLit
    | S.BoolLit b -> T.BoolLit b
    | S.FloatLit f -> T.FloatLit f
    | S.IntLit f -> T.IntLit f
    | S.StringLit s -> T.StringLit s
and fliteral lit : T.literal = finish_place finish_literal lit

and finish_expr place : S._expr -> T._expr = function
    | S.AccessExpr (e1,e2) -> T.AccessExpr (fexpr e1, fexpr e2)
    | S.AccessMethod (e1,x) -> T.AccessMethod (fexpr e1, x)
    | S.AssertExpr e -> T.AssertExpr (fexpr e)                   
    | S.BinopExpr (e1, op, e2) -> 
        T.BinaryExpr ( fexpr e1, op, fexpr e2) 
    | S.CallExpr (e1,e2) -> T.AppExpr(fexpr e1, List.map fexpr e2) 
    | S.ClassOf ct -> begin
        match ct.value with
        | S.Atomic x -> T.VarExpr (Atom.fresh_builtin x)
        | S.TVar x -> T.AccessExpr( 
            {place = ct.place; value=T.VarExpr x}, 
            {place = ct.place; value = T.VarExpr (Atom.fresh_builtin "class")}
            )
    end 
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
        (*  Encoding
            e.getOrElseThrow(() -> new RuntimeException("The result is failure, can access the success."))
        *)
        T.AppExpr ( 
            {place; value=T.AccessExpr (fexpr e, { place; value=T.VarExpr (Atom.fresh_builtin "getOrElseThrow")})},
            [
                {place; value = LambdaExpr ([], {place; value=
                    ReturnStmt {place; value = 
                        NewExpr (
                            auto_place (T.VarExpr (Atom.fresh_builtin "RuntimeException")),
                            [
                                auto_place (T.LiteralExpr (auto_place (T.StringLit "The result is failure, can access the success.")))
                            ]
                        )
                    } 
                })}
            ]
        ) (* TODO should return form the fct with the error or return the result*) 
    | S.UnopExpr (op, e) -> T.UnaryExpr (op, fexpr e) 
    | S.VarExpr x -> T.VarExpr x             
    | S.NewExpr (e, es) -> T.NewExpr (fexpr e, List.map fexpr es)             
    | S.RawExpr str -> T.RawExpr str
and fexpr expr : T.expr = finish_place finish_expr expr

and finish_stmt place : S._stmt -> T._stmt = function
    | S.AssignExpr (e1, e2) -> T.ExpressionStmt ( {place; value=T.AssignExpr(fexpr e1, T.AssignOp, fexpr e2)})
    | S.BlockStmt stmts -> T.BlockStmt (List.map fstmt stmts)
    | S.BreakStmt -> T.BreakStmt
    | S.CommentsStmt c -> T.CommentsStmt c
    | S.ContinueStmt -> T.ContinueStmt
    | S.ExpressionStmt e -> T.ExpressionStmt(fexpr e)
    | S.EmptyStmt -> T.EmptyStmt
    | S.IfStmt (e, stmt1, stmt2_opt) -> T.IfStmt (fexpr e, fstmt stmt1, Option.map fstmt stmt2_opt)              
    | S.LetStmt (ct, x, None) -> 
        T.NamedExpr (fctype ct, x, None) (* TODO FIXME maybe not the semantic that we want, we need to add this to the doc*)  
    | S.LetStmt (ct, x, Some e) -> T.NamedExpr (fctype ct, x, Some (fexpr e))
    | S.ReturnStmt e -> T.ReturnStmt (fexpr e)
and fstmt stmt : T.stmt = finish_place finish_stmt stmt

let rec finish_state  (state:S.state) : T.str_items list = 
match state.value with
    | s when s.persistent -> failwith "TODO finish _state with persistency" 
    | s -> List.map (function x -> {place = state.place; value=T.Stmt x}) (List.map fstmt s.stmts)

and finish_event place ({vis; name; kind; args}: S._event) :  T._str_items = 
    let fplace = place@(Error.forge_place "Plg=AkkaJava/finish_event" 0 0) in
    let auto_place smth = {place = fplace; value=smth} in

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
            annotations = [T.Visibility T.Public];   
            ret_type    = None;
            name        = name;
            parameters  = List.map finish_arg args;
            body        = List.map generate_constructor_stmt args
            ;
        }})}
    in

    (* Compute implemented types 
        pong implements Actor1.Command, Actor2.Command, ...
    *)
    
    let implemented_types = 
        match Hashtbl.find_opt (!cstate).event2receptionists name with
        | None -> Error.plog_warning logger fplace "Event %s can not be received by any component" (Atom.hint name); [] (*FIXME do this check inside the IR *) 
        | Some actor_names -> 
            List.map fctype (List.map (Akka.Misc.t_command_of_actor fplace) actor_names)
    in

    T.Body ({place; value = T.ClassOrInterfaceDeclaration {
        isInterface         = false;
        annotations         = [T.Visibility (finish_visibility vis); T.Static; T.Final];
        name                = name;
        parameters          = []; 
        extended_types      = [];
        implemented_types   = implemented_types; 
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

and finish_actor place ({name; methods; states; events; nested_items; receiver}: S._actor): T._str_items =
    let fplace = place@(Error.forge_place "Plg=Akka/finish_actor" 0 0) in
    let auto_place smth = {place = fplace; value=smth} in

    (* At most one constructor *)
    assert( List.length (List.filter (function (m:S.method0) -> m.value.is_constructor) methods) <= 1);

    (** FIXME public/protected/private should parametrized*)

    let extended_type = auto_place (T.ClassOrInterfaceType (
        Atom.fresh_builtin "AbstractBehavior", 
        [ 
            fctype (Rt.Misc.t_command_of_actor place name)     
        ]) 
    ) in

    let command_cl = auto_place ( T.Body (
        auto_place (T.ClassOrInterfaceDeclaration {
            isInterface = true;
            annotations = [T.Visibility T.Public];
            name = Rt.Misc.a_command; 
            parameters = [];
            extended_types = [];
            implemented_types = [];
            body = [];
        })
    )) in

    let body : T.str_items list ref = ref [] in
    (* FIXME issue with the type of body*)
    body := !body @ [{place; value=T.Comments (IR.LineComment "Actor state")}];
    body := !body @ (List.flatten (List.map finish_state states));
    body := !body @ [{place; value=T.Comments (IR.LineComment "Actor events")}];
    body := !body @ (List.map fevent events);
    body := !body @ [{place; value=T.Comments (IR.LineComment "Actor internal logics")}];
    body := !body @ (List.map fmethod methods);
    body := !body @ [{place; value=T.Comments (IR.LineComment "Nested structures")}];
    body := command_cl :: (!body @ (List.map fterm nested_items));
    begin match receiver with
    | Some receiver  ->     
        body := !body @ [{place; value=T.Comments (IR.LineComment "Receiver")}];
        body := !body @ [fmethod receiver];
    | None -> ()
    end;

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
    | Actor a  -> (factor a).value
    | Import s -> T.JModule ({place; value=T.ImportDirective s})
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
                extended_types = List.map fctype cdcl.extended_types;
                implemented_types = List.map fctype cdcl.implemented_types;
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

and finish_program target (_cstate, program): (string * Fpath.t * T.program) List.t = 
    cstate := _cstate;
    (*TODO finish entrypoint/system*)
    program
    |> split_akka_ast_to_files target
    |> List.map (function package_name, file, terms -> package_name, file, List.map fterm terms)

let finish_ir_program target (ir_program: Plugin.S.program) : (string * Fpath.t * T.program) List.t =
    ir_program
    |> Rt.Finish.finish_program  
    |> (function (cstate,ast) -> cstate, (dump "Runtime AST" Rt.Ast.show_program ast))
    |> finish_program target
    |> List.map (function package_name, file, program -> package_name, file, dump "Language AST" Lg.Ast.show_program program)


(** Output program*)
let output_program target build_dir ir_program =
    ir_program
    |> finish_ir_program target
    |> List.iter (function (package_name, file, program) -> Lg.Output.output_program package_name (Fpath.append build_dir file) program)

let jingoo_env (target:Core.Target.target) = [
    ("compiler_version", Jg_types.Tstr Config.version);
    ("compiler_debug", Jg_types.Tbool (Config.debug ()));
    ("compiler_keep_ghost", Jg_types.Tbool (Config.keep_ghost ()));

    ("project_name", Jg_types.Tstr (Config.project_name ()));
    ("author", Jg_types.Tstr (Config.author ()));
    ("target_mains", Jg_types.Tlist 
        (List.map (function ({Core.Target.name;}:Core.Target.maindef) -> Jg_types.Tstr name) target.value.codegen.mains)
    )
]

let custom_template_rules target = [
    (Fpath.v "application.conf.j2", jingoo_env target, List.fold_left Fpath.add_seg (Fpath.v "src/main/resources")  [Config.author (); Config.project_name (); "application.conf"]);
]
let custom_external_rules () = []

    