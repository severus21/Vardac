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
    @return n-elmts, List.length-n elmts
    TODO move it in some lib files
*)
let rec split_list n = assert(n>=0); function
| [] -> assert(n=0); [], []
| x::xs when n > 0-> 
    let ys1, ys2 = split_list (n-1) xs in
    x::ys1, ys2
| x::xs when n = 0 ->
    [], x::xs
| _ -> failwith "deadbranch see assert n>=0"

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
| MainStage
and stage_entry = {
    kind: stage_kind;
    name: Atom.atom;
    sub_stages: stage_entry list;
    ast: S.term list; 

    imports: S.term list;
    file: Fpath.t option;
    package_name: string option;
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
let split_akka_ast_to_files (target:Core.Target.target) (akka_program:S.program) : (string * Fpath.t * S.term list) list = 
    let generation_dir = List.fold_left Fpath.add_seg (Fpath.v "src/main/java")  [Config.author (); Config.project_name ()] in

    (********* Stage spliting *********)

    let extract_imports terms : S.term list * S.term list = 
        let rec _extract_imports : S.term list -> S.term list * S.term list = function
        | [] -> [], []
        | t::ts -> begin
            let imports, others = _extract_imports ts in
            match t.value.v with
            | S.Import _ -> t::imports, others 
            | _ -> imports, t::others
        end
        in
        let imports, others =  _extract_imports terms in
        imports, others 
    in

    let wrap_main (name:string) (guardian:S.expr) (_main:S.method0) : S.term= 
        (* e.g. ActorSystem<?> system = ActorSystem.create(KeyValueStoreActorSystem.create(),
            KeyValueStoreActorSystem.NAME,
            config.withFallback(ConfigFactory.load()));*)
        let system_name = "system_"^(Config.project_name ()) in
        let a_system = Atom.fresh "system" in
        let actor_system = auto_place (S.LetStmt (
            auto_place (S.TParam (
                auto_place (S.Atomic "ActorSystem"),
                [auto_place (S.Atomic "?")]
            )),
            a_system,
            Some (auto_place ( S.CallExpr(
                auto_place (S.AccessExpr(
                    auto_place (S.VarExpr (Atom.fresh_builtin "ActorSystem")),
                    auto_place (S.VarExpr (Atom.fresh_builtin "create"))
                )),
                [
                    (* specify the guardian actor *)
                    auto_place (S.CallExpr(
                        auto_place (S.AccessExpr(
                            guardian,
                            auto_place (S.VarExpr (Atom.fresh_builtin "create"))
                        )),
                        []
                    ));
                    auto_place (S.LitExpr (auto_place (S.StringLit system_name)))
                ]
            )))
        )) in        

        auto_place ( {
            S.annotations = [S.Visibility S.Public];
            decorators = [];
            v = S.ClassOrInterfaceDeclaration {
                isInterface = false;
                name = Atom.fresh_builtin (String.capitalize_ascii name);
                extended_types = [];
                implemented_types = [];
                body = [
                    auto_place ({
                        S.annotations = [S.Visibility S.Public; S.Static];
                        decorators = [];
                        v = S.MethodDeclaration (auto_place {
                            S.annotations = [];
                            decorators = [];
                            v = {
                                S.ret_type = auto_place S.TVoid;    
                                name = Atom.fresh_builtin "main";
                                args= [auto_place (S.Atomic "String[]"), Atom.fresh_builtin "args"];
                                is_constructor = false;
                                body = match _main.value.v.body with 
                                    |S.AbstractImpl stmts -> S.AbstractImpl (actor_system::stmts) 
                                    | _ -> _main.value.v.body 
                            }
                        }) 
                    })
                ]
            }
        })
    in
    
    let wrap_stage_ast (stage: stage_entry) : stage_entry =
        if stage.kind <> AnonymousStage then stage
        else 
            (* here we could find a main function *)
            let imports, others = extract_imports stage.ast in
            if others = [] then stage
            else begin 
                let others = List.map (function (o:S.term) -> {
                    o with value = {
                        S.annotations = [S.Visibility S.Public] @ o.value.annotations;
                        decorators = o.value.decorators;
                        v = o.value.v
                    }
                }) others in
                List.iter (function (o:S.term) -> assert(o.value.annotations <> [])) others;

                {
                    stage with ast = 
                    imports @ 
                    [
                        auto_place ({
                            S.annotations = [S.Visibility S.Public];
                            decorators = [];
                            v = S.ClassOrInterfaceDeclaration {
                                isInterface = false;
                                name = stage.name;
                                extended_types = [];
                                implemented_types = [];
                                body = others; 
                            }
                        })
                    ]
                }
            end
    in



    (*
        @param acc_current_stage - list of terms composing the current term (in reverse order)
    *)
    let group_per_stage_or_component terms : stage_entry list = 
        let rec _group_per_stage_or_component acc_current_stage : S.term list -> stage_entry list = 
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
                }
            in    

            function 
            | [] -> [stageofacc ()]
            | term::ts -> begin
                match term.value with
                (* the events of an actor should not be defined externaly
                i.e. ignore the S.Event case
                *)
                | {v=S.ClassOrInterfaceDeclaration cid} -> begin
                    (* No sub-stages *)
                    
                    let cid_stage = {
                        kind = ClassOrInterfaceDeclarationStage;
                        name = cid.name;
                        ast = [term];
                        sub_stages = [];

                        imports = [];
                        file = None;
                        package_name = None;
                    } in 
                    (stageofacc ()) :: cid_stage :: (_group_per_stage_or_component [] ts)
                end
                | {v=S.Actor a} -> begin
                    let sub_stages = 
                        _group_per_stage_or_component [] a.value.nested_items
                    in
                    let sub_stages = List.filter (function stage -> stage.ast <> []) sub_stages in

                    let actor_stage = {
                        kind = ActorStage;
                        name = a.value.name;
                        ast = [{
                            place = term.place;
                            value = {
                                annotations = [S.Visibility S.Public];
                                decorators = [];
                                v = S.Actor { 
                                    place = a.place;
                                    value = {a.value with nested_items = [] }
                                }
                            }
                        }];
                        sub_stages = sub_stages;

                        imports = [];
                        file = None;
                        package_name = None;
                    } in 
                    (stageofacc ()) :: actor_stage :: (_group_per_stage_or_component [] ts)
                end
                | _ -> _group_per_stage_or_component (term::acc_current_stage) ts
            end 
        in
        List.filter (function stage -> stage.ast <> []) (_group_per_stage_or_component [] terms)
    in

    let stages = group_per_stage_or_component akka_program.terms in

    (**** Handle the nomain + laststage main ****)
    let generate_guardian name stmts : S.term = 
        auto_place({
            S.annotations = [S.Visibility S.Public];
            decorators = []; 
            v = S.Actor ( auto_place({
                S.name = name;
                methods = [
                    auto_place {
                        S.decorators = [];
                        annotations = [S.Visibility S.Public];
                        v = {
                            S.ret_type = auto_place S.TVoid;
                            name = name;
                            args = [];
                            is_constructor = true;
                            body = AbstractImpl stmts
                        }
                    }
                ];
                receiver = None;
                states = [];
                events = [];
                nested_items = [];
            }))
        })
    in

    let add_guardian (mdef:Target.maindef) stage : stage_entry list =
        assert(stage.sub_stages = [] && (Atom.hint mdef.bootstrap = "laststage"));

        let getlasts_stmts (terms:S.term list) : S.stmt list * S.term list= 
            (* aux is tail rec *)
            let rec aux collected_stmts = function
            | [] -> collected_stmts, []
            | {value={S.v=S.Stmt stmt}}::xs -> 
                aux (stmt::collected_stmts) xs
            | ts -> collected_stmts, ts
            in
            aux [] (List.rev terms) 
        in

        let last_stmts, previous_terms = getlasts_stmts stage.ast in
        let actor_name = Atom.fresh_builtin "Laststage" in
        [
            {   stage with 
                ast = previous_terms
            };
            {   
                kind = ActorStage;
                name = actor_name;
                sub_stages = []; 
                ast = (fst (extract_imports stage.ast)) @ [generate_guardian actor_name last_stmts];
                imports = [];
                file = None;
                package_name = None;
            };
        ]
    in

    let guardian_name_of (mdef:Target.maindef) = 
        if Atom.hint mdef.bootstrap = "laststage" then
            Atom.fresh_builtin (String.capitalize_ascii (Atom.hint mdef.bootstrap))
        else mdef.bootstrap
    in

    let generate_no_main (mdef:Target.maindef) = 
        assert(Atom.is_builtin mdef.entrypoint && Atom.hint mdef.entrypoint = "no_main");

        (* Just starts the actor system with the specific guardian i.e. mdef.bootstrap *)
        wrap_main mdef.name (auto_place (S.VarExpr (guardian_name_of mdef))) (auto_place {
            S.annotations = [S.Visibility S.Public];
            decorators = [];
            v = {
                S.ret_type = auto_place S.TVoid;
                name = Atom.fresh_builtin (String.capitalize_ascii (Atom.hint mdef.entrypoint));
                body = AbstractImpl [];
                args = [];
                is_constructor = false; 
            }
        }) 
    in

    let add_no_main mdef stage = 
        { stage with ast = stage.ast @ [generate_no_main mdef] }
    in

    let prepare_stage stages (mdef: Target.maindef) : stage_entry list =
        let add_guardians stages = match Atom.hint mdef.bootstrap with
            | "laststage" -> 
                let _stages, [laststage] = split_list (List.length stages - 1) stages in
                assert("Stage" = Atom.hint laststage.name); (* FIXME fragile *)
                _stages @  (add_guardian mdef laststage)
            | _ -> stages
        in

        let add_main_classes stages = match Atom.hint mdef.entrypoint with
            | "no_main" -> 
                let _stages, [laststage] = split_list (List.length stages - 1) stages in
                _stages @ [add_no_main mdef laststage]
            | _ -> 
                let selector = function 
                    | {S.v=S.MethodDeclaration x} -> x.value.v.name = mdef.entrypoint 
                    | _-> false
                in
                let replace = function
                    | {S.v=S.MethodDeclaration m0} -> 
                        (wrap_main mdef.name (auto_place (S.VarExpr (guardian_name_of mdef))) m0).value 
                in
                List.map (function stage -> {stage with ast = List.map (S.replaceterm_term false selector replace) stage.ast }) stages 
        in
        add_main_classes (add_guardians stages)
    in

    let stages = List.fold_left prepare_stage stages target.value.codegen.mains in
    (********************)

    (***** Create dedicated stage for main cl - assuming no one depends on them ******)
    let make_is_main () : Atom.atom -> bool =
        let state : (string, unit) Hashtbl.t = Hashtbl.create (List.length target.value.codegen.mains) in
        List.iter (function (mdef:Target.maindef) -> Hashtbl.add state (String.capitalize_ascii mdef.name) ()) target.value.codegen.mains;

        function name -> (Atom.is_builtin name) && (None <> Hashtbl.find_opt state (Atom.hint name)) 
    in 
    let is_main : Atom.atom -> bool = make_is_main () in

    let rec extract_main_terms = function
    | [] -> [], []
    | ({value={S.v=S.ClassOrInterfaceDeclaration cid}} as t)::ts when is_main cid.name -> 
        let mains, others = extract_main_terms ts in
        t::mains, others
    | ({value={S.v=S.ClassOrInterfaceDeclaration cid}} as t)::ts-> 
        let mains, others = extract_main_terms ts in
        mains, t::others
    | t::ts -> 
        let mains, others = extract_main_terms ts in
        mains, t::others
    in

    let rec extract_main_stages = function
    | [] ->  [], []
    | stage::stages ->
        let mains_t, others_t = extract_main_terms stage.ast in
        let submains, subothers = extract_main_stages stage.sub_stages in
        let stage = {stage with
            sub_stages = subothers;
            ast = others_t;
        } in
        let current_mains = List.map (function ({value={S.v=S.ClassOrInterfaceDeclaration cid}} as t) -> {
            kind = MainStage;
            name = cid.name;
            sub_stages = [];
            ast = [t];
            imports = [];
            file = None;
            package_name = None;
        }) mains_t in
        
        let mains, others = extract_main_stages stages in

        mains@submains@current_mains, stage::others
    in
    let mains, others = extract_main_stages stages in
    let mains = List.map ( function stage ->
        { stage with ast = [
                auto_place ({
                    S.annotations = [];
                    decorators = [];
                    v = S.Import "akka.actor.typed.ActorSystem"
                })
            ] @ stage.ast
        }
    ) mains in
    let stages = others@mains in

    (********************)
    
    (***** Hydrate stages *****)

    let rec external_binders_of_stage stage : S.variable list = 
        let aux (t:S.term) : S.variable list = 
            match t.value.v with
                | S.Actor a -> [a.value.name]
                | S.ClassOrInterfaceDeclaration cid when (Atom.hint cid.name) = "Stage"-> begin 
                    let mock_stage = {stage with ast = cid.body } in
                    external_binders_of_stage mock_stage
                end
                | S.ClassOrInterfaceDeclaration cid -> [cid.name]
                | S.Event e -> [e.value.name]
                | S.Stmt {value = LetStmt (_,x,_);} -> [x]
                | t -> []
        in

        List.flatten (List.map aux stage.ast)
    in

    let rec apply_rename_stage renaming stage = 
        {stage with 
            ast = List.map (S.apply_rename_term false renaming) stage.ast;
            sub_stages = List.map (apply_rename_stage renaming) stage.sub_stages
        }
    in

    let main_state_rename = Hashtbl.create 128 in

    (* No clash of variables - since they are unique -> Atom *)
    let rec collect_renaming_stage (stage : stage_entry) : unit = 
        (*let base_rename = (Printf.sprintf "%s.%s" (Config.author ()) (Config.project_name ())) in*)
        let hydrate_state stage state x = 
            Hashtbl.add state x (
                let tmp = 
                if stage.name = x then (
                    Atom.refresh_hint x (Option.get stage.package_name^"."^(Atom.hint x))
                ) else (
                    Atom.refresh_hint x (Option.get stage.package_name^"."^(String.lowercase_ascii (Atom.to_string stage.name))^"."^(Atom.hint x))
                )
                in
                logger#error ">> %s -> %s" (Atom.to_string x) (Atom.to_string tmp);
                tmp
            ) 
        in
        (*** 
            - rename subsequent stages - that may depends of the variables binded by the current stage 
            - previous stages can also depend on current stage renaming since component dependencies can be circular
            - substage renaming is encapsulated
        ***)
        let to_rename1 = external_binders_of_stage stage in
        List.iter (hydrate_state stage main_state_rename) to_rename1;
        List.iter collect_renaming_stage stage.sub_stages;
    in

    let rec collect_renaming_stages stages : unit = List.iter collect_renaming_stage stages in

    let rec rename_stages (stages:stage_entry list) : stage_entry list = 
        collect_renaming_stages stages;
        logger#error "main %d" (Hashtbl.length main_state_rename);

        let main_renaming (x:Atom.atom) : Atom.atom = 
            match Hashtbl.find_opt main_state_rename x with 
            | None -> x
            | Some new_x ->
                    new_x
        in
        List.map (apply_rename_stage main_renaming) stages 
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
    let rec hydrate_stages (package_name:string) (imports:S.term list) parent_opt : stage_entry list -> S.term list * stage_entry list= function
    | [] -> imports, []
    | stage :: stages ->
        let stage = {stage with file = Some (generate_file_of_stage parent_opt stage)} in
        let sub_imports, sub_stages = hydrate_stages (package_name^"."^(String.lowercase_ascii(Atom.to_string stage.name))) imports (Some stage) stage.sub_stages in
        let stage = { stage with 
            imports = List.rev sub_imports;
            package_name = if stage.sub_stages = [] then Some package_name else Some (package_name^"."^(String.lowercase_ascii (Atom.to_string stage.name)));
            sub_stages;
        } in

        (* Design choice: subcomponents/stages are private - i.e. do not reuse sub_imports for stages*)
        let current_import = 
            let base_import = (Printf.sprintf "%s.%s" (Config.author ()) (Config.project_name ())) in
            if stage.kind <> AnonymousStage then
                auto_place (S.Import (base_import^"."^(String.lowercase_ascii (Atom.to_string stage.name))^"."^(Atom.to_string stage.name)))
            else
                auto_place (S.Import (base_import^"."^(String.lowercase_ascii (Atom.to_string stage.name))^".*"))
        in

        (* TODO Get ride of import logic *) 
        (*let imports = current_import::imports in*)
        let _, stages = (hydrate_stages package_name imports parent_opt stages) in
        imports, stage::stages
    in
   
    (* Stages with imports, files and main *)
    let _, stages = hydrate_stages (Printf.sprintf "%s.%s" (Config.author ()) (Config.project_name ())) [] None stages in

    (* Wrap stages *)
    let rec wrap_stage stage =
        let stage = wrap_stage_ast stage in
        { stage with sub_stages = List.map wrap_stage stage.sub_stages; }
    in
    let stages = List.map wrap_stage stages in

    let put_imports_first terms : S.term list =
        let imports, others = extract_imports terms in
        imports @ others
    in
    
    (* Renaming needs that package_name is set for each stage *)
    let stages = rename_stages stages in

    (* Generate the output *)
    let rec flatten_stages : stage_entry list -> (string * Fpath.t * S.term list) list = function
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


let rec finish_visibility = function
    | S.Private -> T.Private
    | S.Protected -> T.Protected
    | S.Public -> T.Public

and finish_decorator = function
| S.Override -> T.Override
and finish_decorators decorators = List.map finish_decorator decorators

and finish_annotation = function
| S.Visibility vis -> T.Visibility (finish_visibility vis)
| S.Static -> T.Static
| S.Final -> T.Final
and finish_annotations annotations = List.map finish_annotation annotations
and finish_annoted (finish_v : Error.place -> 'a -> 'b) place (smth:'a S.annotated) : 'b T.annotated = 
    {
        T.annotations = finish_annotations smth.annotations;
        decorators = finish_decorators smth.decorators;
        v = finish_v place smth.v
    }

let builtin_translation = List.to_seq []
module BuiltinMap = Map.Make(String)                     
let builtin_eval = 
  let translation = BuiltinMap.of_seq builtin_translation in
  function x ->   
  try
    BuiltinMap.find (Atom.value x) translation
  with Not_found ->Atom.value x

let rec finish_ctype place : S._ctype -> T._jtype = 
    let fplace = place@(Error.forge_place "Plg=AkkaJava/finish_event" 0 0) in
    let auto_place smth = {place = fplace; value=smth} in
function 
    | S.Atomic s -> T.TAtomic s 
    | S.ActorRef t -> T.ClassOrInterfaceType  (auto_place (T.TAtomic "ActorRef"), [fctype t])  
    | S.TFunction (t1, t2) -> T.ClassOrInterfaceType  ( auto_place (T.TAtomic "Function"), [fctype t1; fctype t2]) 
    | S.TList t1 -> T.ClassOrInterfaceType  (auto_place (T.TAtomic "List"), [fctype t1])
    | S.TMap (t1, t2) -> T.ClassOrInterfaceType  (auto_place (T.TAtomic "Map"), [fctype t1; fctype t2])
    | S.TOption t1 -> T.ClassOrInterfaceType  (auto_place(T.TAtomic "Optional"), [fctype t1]) 
    | S.TAccess (t1, t2) -> T.TAccess (fctype t1, fctype t2)
    | S.TParam (t, t_args) -> T.ClassOrInterfaceType (fctype t, List.map fctype t_args)
    | S.TResult (t1, t2) -> 
        (* 
            Encoding as the Either<left, right> for Vavr,
            left denotes the Err and right denotes the Ok
        *)
        T.ClassOrInterfaceType  (auto_place (T.TAtomic "Either"), [fctype t2; fctype t1]) 
    | S.TSet t1 -> T.ClassOrInterfaceType  (auto_place (T.TAtomic "Set"), [fctype t1])
    | S.TTuple cts -> begin 
        let cls_name = match List.length cts with
        | n when n < 9 -> "Tuple"^(string_of_int n) 
        | _ -> failwith "Tuple with length > 8 are not supported by the Vavr library."
        in 
        T.ClassOrInterfaceType (auto_place(T.TAtomic cls_name), List.map fctype cts)
    end 
    | S.TVar v -> T.TVar v
    | S.TVoid -> T.TAtomic "Void"
    | S.TRaw str -> T.TAtomic str
and fctype ct : T.jtype = finish_place finish_ctype ct



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
        {
            place = ct.place; 
            value = T.Body ({place = ct.place; value = {
                T.annotations = [T.Visibility T.Public; T.Final];
                decorators = [];
                v = T.FieldDeclaration {
                    type0 = fctype ct;
                    name  = x;
                    body  = None;
                }
            }})
        }
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
        { 
            place; 
            value = T.Body ({place; value= {
                T.annotations = [T.Visibility T.Public];
                decorators  = [];
                v = T.MethodDeclaration {
                    ret_type    = None;
                    name        = name;
                    parameters  = List.map finish_arg args;
                    body        = List.map generate_constructor_stmt args
                    ;
                }
            }})
        }
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

    T.Body {
        place; 
        value = {
            T.annotations         = [T.Visibility (finish_visibility vis); T.Static; T.Final];
            decorators = [];
            v = T.ClassOrInterfaceDeclaration {
                isInterface         = false;
                name                = name;
                parameters          = []; 
                extended_types      = [];
                implemented_types   = implemented_types; 
                body                = constructor::fields
            }
        }
    }
and fevent e : T.str_items = finish_place finish_event e

and finish_arg ((ctype,variable):(S.ctype * Atom.atom)) : T.parameter =
    (fctype ctype, variable)
and finish_method_v is_actor_method place ({ret_type; name; body; args; is_constructor}: S._method0) : T._body = 
    match body with
    | S.AbstractImpl stmts when is_constructor ->
        let args = match is_actor_method with
        | true -> ((Rt.Misc.t_actor_context place None, Rt.Misc.a_context)::args)
        | false -> args
        in
        
        let stmts = match is_actor_method with
        | true -> auto_place ( S.ExpressionStmt (Rt.Misc.e_super place [auto_place(S.VarExpr Rt.Misc.a_context)]))::stmts
        | false ->  stmts
        in


        (* FIXME check in IR that onstratup no type i.e. void*)
        T.MethodDeclaration {
            ret_type    = None;
            name        = name;
            parameters  = List.map finish_arg args;
            body        = List.map fstmt stmts;
        }
    | S.AbstractImpl stmts ->
        T.MethodDeclaration {
            ret_type    = Some (fctype ret_type);
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
            ret_type    = if is_constructor then None else Some (fctype ret_type);
            name;
            parameters  = List.map finish_arg args;
            body
        }
and fmethod is_actor_method m : T.str_items = {place=m.place; value= T.Body (finish_place (finish_annoted (finish_method_v is_actor_method)) m)}

and finish_actor place ({name; methods; states; events; nested_items; receiver}: S._actor): T._str_items =
    let fplace = place@(Error.forge_place "Plg=Akka/finish_actor" 0 0) in
    let auto_place smth = {place = fplace; value=smth} in

    (* At most one constructor *)
    assert( List.length (List.filter (function (m:S.method0) -> m.value.v.is_constructor) methods) <= 1);

    (** FIXME public/protected/private should parametrized*)

    let extended_type = auto_place (T.ClassOrInterfaceType (
        auto_place (T.TAtomic "AbstractBehavior"), 
        [ 
            fctype (Rt.Misc.t_command_of_actor place name)     
        ]) 
    ) in

    let command_cl = auto_place ( T.Body (
        auto_place ({
            T.annotations = [T.Visibility T.Public];
            decorators = [];
            v = T.ClassOrInterfaceDeclaration {
                isInterface = true;
                name = Rt.Misc.a_command; 
                parameters = [];
                extended_types = [];
                implemented_types = [];
                body = [];
            }
        })
    )) in

    (**** generate actor create method ****)
    (* public static <K, V> Behavior<Command> create(int transactionId,
    ActorRef<TransactionManagerActor.BeginTransactionReplyEvent> replyTo,
    ActorRef<ShardingEnvelope<JournalActor.Command>> journalShard) {
return Behaviors.setup(context -> {
context.getLog().debug("TransactionCoordinatorActor::create()");
return new TransactionCoordinatorActor<K, V>(context, transactionId, replyTo, journalShard);
});
}*)
    (* TODO check in IR at most once constructor/destructor *)
    let constructor_opt  = List.find_opt (function (m:S.method0) -> m.value.v.is_constructor) methods in
    let constructor_args = match constructor_opt with | None -> [] |Some constructor -> constructor.value.v.args in 

    let methods = match constructor_opt with
    | Some _ -> methods
    | None -> (* add a default constructor, will be hydrated with context when running fmethod *)
        auto_place ({
            S.annotations = [S.Visibility S.Public];   
            decorators = [];
            v = {
                S.ret_type    = auto_place S.TVoid;
                name        = name;
                args        = []; 
                body        = AbstractImpl []; 
                is_constructor = true;
            }
        })::methods
    in

    let arg_lambda = auto_place (S.LambdaExpr (
        [Rt.Misc.a_context],
        auto_place (S.BlockStmt [
            auto_place (S.ExpressionStmt (
                Rt.Misc.e_debug_of 
                    place 
                    (auto_place (S.VarExpr Rt.Misc.a_context)) 
                    [
                        auto_place (S.LitExpr (auto_place (S.StringLit (Atom.to_string name^"::create"))))
                    ]
            ));
            auto_place (S.ReturnStmt (auto_place (
                S.NewExpr (
                    auto_place (S.VarExpr name),
                    List.map (function x -> auto_place (S.VarExpr x)) (Rt.Misc.a_context::(List.map snd constructor_args))

                )
            )));
        ])
    )) in
    let m_create : S.method0 = auto_place {
        S.annotations = [S.Visibility S.Public; S.Static];
        decorators = [];
        v = {
            S.ret_type = Rt.Misc.t_behavior_of_actor place name;
            name = Rt.Misc.a_create_method;
            body = S.AbstractImpl [auto_place (S.ReturnStmt (Rt.Misc.e_setup_behaviors place [arg_lambda]))];
            args = constructor_args;
            is_constructor = false;
        }
    } in
    let methods = methods @ [m_create] in
    

    (*************)

    let body : T.str_items list ref = ref [] in
    (* FIXME issue with the type of body*)
    body := !body @ [{place; value=T.Comments (IR.LineComment "Actor state")}];
    body := !body @ (List.flatten (List.map finish_state states));
    body := !body @ [{place; value=T.Comments (IR.LineComment "Actor events")}];
    body := !body @ (List.map fevent events);
    body := !body @ [{place; value=T.Comments (IR.LineComment "Actor internal logics")}];
    body := !body @ (List.map (fmethod true) methods);
    body := !body @ [{place; value=T.Comments (IR.LineComment "Nested structures")}];
    body := command_cl :: (!body @ (List.map fterm nested_items));
    begin match receiver with
    | Some receiver  ->     
        body := !body @ [{place; value=T.Comments (IR.LineComment "Receiver")}];
        body := !body @ [fmethod true receiver];
    | None -> ()
    end;

    T.Body { 
        place; 
        value = {
            T.annotations = [T.Visibility T.Public];
            decorators = [];
            v = T.ClassOrInterfaceDeclaration {
                isInterface= false;
                name=name;
                parameters = [];
                extended_types = [extended_type] ;
                implemented_types = [];
                body = !body 
            }
        }
    } 
and factor a : T.str_items = finish_place finish_actor a

and finish_term place {S.annotations; decorators; v=t}: T._str_items =
match t with 
    | S.Comments c -> T.Comments c
    | Actor a  -> (factor a).value
    | Import s -> T.JModule ({place; value=T.ImportDirective s})
    | Event e  ->  (fevent e).value
    | Stmt s -> T.Stmt (fstmt s)
    | Class x -> T.Body (
        { 
            place; 
            value = {
            T.annotations = finish_annotations annotations;
            decorators = finish_decorators decorators;
                v = T.ClassOrInterfaceDeclaration {
                    isInterface = false;
                    name = x;
                    parameters = []; 
                    extended_types = [];
                    implemented_types = [];
                    body = [] 
                }
            }
        }
    )
    | ClassOrInterfaceDeclaration cdcl ->T.Body { 
        place;
        value = {
            T.annotations = finish_annotations annotations;
            decorators = finish_decorators decorators;
            v = T.ClassOrInterfaceDeclaration {
                isInterface = cdcl.isInterface;
                name = cdcl.name;
                parameters = []; 
                extended_types = List.map fctype cdcl.extended_types;
                implemented_types = List.map fctype cdcl.implemented_types;
                body = List.map fterm cdcl.body 
            }
        }
    }
    | MethodDeclaration m -> 
        (* Needs to put external annotations/decorators insde the Body of method since str_items can not be annotated *)
        let external_annotations = finish_annotations annotations in
        let external_decorators = finish_decorators decorators in

        let ({value=(T.Body body);} as m) = (fmethod false m) in

        T.Body { body with
            value = { body.value with
                T.annotations = external_annotations @ body.value.annotations;
                decorators  = external_decorators @ body.value.decorators 
            }
        } 
    | RawClass (x, raw) ->  T.Body { 
        place;
        value = { 
            T.annotations = finish_annotations annotations;
            decorators = finish_decorators decorators;
            v = T.ClassOrInterfaceDeclaration {
                isInterface = false;
                name = x;
                parameters = []; 
                extended_types = [];
                implemented_types = [];
                body = [{ place=raw.place; value=T.Raw raw.value}] 
            }
        }
    }
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
    |> (function (cstate,ast) -> cstate, (dump "Runtime AST" S.show_program ast))
    |> finish_program target
    |> List.map (function package_name, file, program -> package_name, file, dump "Language AST" Lg.Ast.show_program program)


(** Output program*)
let output_program target build_dir ir_program =
    ir_program
    |> finish_ir_program target
    |> List.map (function (package_name, file, program) -> package_name, file, (dump (Printf.sprintf "Lg AST for file %s" (Fpath.to_string file)) T.show_program program)
    )
    |> List.map (function (package_name, file, program) -> (package_name, file, Lg.Clean.clean_program program))
    |> List.map (function (package_name, file, program) -> package_name, file, (dump (Printf.sprintf "Cleaned Lg AST for file %s" (Fpath.to_string file)) T.show_program program))
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

    