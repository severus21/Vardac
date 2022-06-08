open Core
open Core.AstUtils
open Jingoo
open Easy_logging
open Plg

let name = "Akka<Java>"


let fplace = (Error.forge_place "Plg=AkkaJava" 0 0)
let auto_fplace smth = {place = fplace; value=smth}
module S_A2 = Ast.AstUtil2.Make(struct let fplace = fplace end)

module Make (Arg: Plugin.CgArgSig) = struct
    let headers = Arg.headers
    let dependencies = Arg.dependencies

    let name = "Akka<Java>"
    let logger = Logging.make_logger ("_1_ compspec.plg."^name) Debug [];;

    module Rt = Akka
    module Lg = Java

    (** Finish program*)


    (* The source calculus. *)
    module S = Rt.Ast 
    (* The target calculus. *)
    module T = Lg.Ast 


    (* TODO move all templates externals into akka/templates_sites *)
    let templates_location = 
        match Mysites.Sites.templates with
        | [templates_location] -> templates_location
        | _ -> raise (Error.DeadbranchError "templates site not found for Akka plugin")
    let externals_location = 
        match Mysites.Sites.externals with
        | [externals_location] -> externals_location
        | _ -> raise (Error.DeadbranchError "templates site not found for Akka plugin")

    let fplace = (Error.forge_place "Plg=AkkaJava" 0 0)
    let auto_place smth = {place = fplace; value=smth}

    type blackbox_term = Rt.Ast.blackbox_term

    module MakeRt2Lg(Arg:sig 
        val target:Core.Target.target 
        val cstate:Rt.Finish.collected_state
    end) = struct
        (** Inner state *)
        let current_headers = ref []

        (** Exported state *)
        let istate : Plg.Interface_plugin.istate ref = ref (Plg.Interface_plugin.empty_istate ())
        let cstate : Rt.Finish.collected_state ref = ref (Rt.Finish.empty_cstate ())

        let rename_cstate renaming = 
            let e2rs = Hashtbl.to_seq (!cstate).event2receptionists in
            let e2rs = Seq.map (function (k,v) -> k, List.map renaming v) e2rs in
            (* No Hashtbl.reset since we do not alter the keys *)
            Hashtbl.replace_seq (!cstate).event2receptionists e2rs;

            let e2rs = Hashtbl.to_seq (!cstate).external2receptionists in
            let e2rs = Seq.map (function (k,v) -> k, List.map renaming v) e2rs in
            (* No Hashtbl.reset since we do not alter the keys *)
            Hashtbl.replace_seq (!cstate).external2receptionists e2rs;

            (!cstate).collected_components := Atom.Set.map (function name -> renaming name) !((!cstate).collected_components);

            (!cstate).guardian_components := Atom.Set.map (function name -> renaming name) !((!cstate).guardian_components)

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
                let a_system = Atom.fresh "system" in
                let actor_system = auto_place (S.LetStmt (
                    auto_place (S.TParam (
                        auto_place (S.Atomic "ActorSystem"),
                        [auto_place (S.Atomic "?")]
                    )),
                    a_system,
                    Some (auto_place ( S.CallExpr(
                        auto_place (S.AccessExpr(
                            auto_place (S.VarExpr (Atom.builtin "ActorSystem"), auto_place S.TUnknown),
                            auto_place (S.VarExpr (Atom.builtin "create"), auto_place S.TUnknown)
                        ), auto_place S.TUnknown),
                        [
                            (* specify the guardian actor *)
                            auto_place (S.CallExpr(
                                auto_place (S.AccessExpr(
                                    guardian,
                                    auto_place (S.VarExpr (Atom.builtin "create"), auto_place S.TUnknown)
                                ), auto_place S.TUnknown),
                                [
                                    (* set run_now to true *)
                                    S_A2.e2_lit (S.BoolLit true);
                                ]
                            ), auto_place S.TUnknown);
                            auto_place (S.LitExpr (auto_place (S.StringLit Akka.Misc.system_name)), auto_place S.TUnknown);
                            auto_place (S.CallExpr (
                                auto_place (S.AccessExpr(
                                    auto_place (S.VarExpr (Atom.builtin "AbstractMain"), auto_place S.TUnknown),
                                    auto_place (S.VarExpr (Atom.builtin "get_config"), auto_place S.TUnknown)
                                ), auto_place S.TUnknown),
                                [
                                    auto_place (S.VarExpr (Atom.builtin "args"), auto_place S.TUnknown)
                                ]
                            ), auto_place S.TUnknown)
                        ]
                    ), auto_place S.TUnknown))
                )) in        

                auto_place ( {
                    S.annotations = [S.Visibility S.Public];
                    decorators = [];
                    v = S.ClassOrInterfaceDeclaration {
                        headers = [];
                        isInterface = false;
                        name = Atom.builtin (String.capitalize_ascii name);
                        extended_types = [];
                        implemented_types = [];
                        body = [
                            auto_place ({
                                S.annotations = [S.Visibility S.Public; S.Static];
                                decorators = [];
                                v = S.MethodDeclaration _main 
                            });
                            auto_place ({
                                S.annotations = [S.Visibility S.Public; S.Static];
                                decorators = [];
                                v = S.MethodDeclaration (auto_place {
                                    S.annotations = [];
                                    decorators = [];
                                    v = {
                                        S.ret_type = auto_place (S.Atomic "void");    
                                        name = Atom.builtin "main";
                                        args= [auto_place (S.Atomic "String[]"), Atom.builtin "args"];
                                        throws          = [];
                                        is_constructor = false;
                                        body = match _main.value.v.body with 
                                            |S.AbstractImpl _ -> S.AbstractImpl (
                                                auto_place(S.ExpressionStmt(
                                                    auto_place (S.CallExpr (
                                                        auto_place (S.VarExpr _main.value.v.name, auto_place S.TUnknown),
                                                        [
                                                            auto_place (S.VarExpr (Atom.builtin "args"), auto_place S.TUnknown)
                                                        ]
                                                    ), auto_place S.TUnknown)
                                                ))
                                                :: actor_system
                                                :: []
                                            ) 
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
                                S.annotations = [S.Visibility S.Public; S.Static] @ o.value.annotations;
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
                                        headers = [];
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

            let stages = group_per_stage_or_component akka_program in

            (**** Handle the nomain + laststage main ****)
            let generate_guardian name stmts : S.term = 
                let run_method = Atom.fresh "run" in
                auto_place({
                    S.annotations = [S.Visibility S.Public];
                    decorators = []; 
                    v = S.Actor ( auto_place({
                        S.extended_types = [Rt.Misc.t_lg4dc_abstract_system fplace];
                        implemented_types = [];
                        is_guardian = true;
                        name = name;
                        headers = [];
                        methods = [
                            auto_place {
                                S.decorators = [];
                                annotations = [S.Visibility S.Public];
                                v = {
                                    S.ret_type = auto_place (S.Atomic "Void");
                                    name = run_method;
                                    args = [];
                                    throws          = [];
                                    is_constructor = false;
                                    body = AbstractImpl (
                                        List.map 
                                            (S.rewriteexpr_stmt (function 
                                                |CurrentContext -> true 
                                                |_-> false
                                                ) 
                                                (function 
                                                |CurrentContext -> S.VarExpr (Atom.builtin "this.context")
                                                | _ -> raise (Error.DeadbranchError "selector prevents accessing this branch")
                                                )
                                            )
                                            stmts
                                    )
                                }
                            };
                            auto_place {
                                S.decorators = [];
                                annotations = [S.Visibility S.Public];
                                v = {
                                    S.ret_type = auto_place (S.Atomic "Void");
                                    name = name;
                                    args = [
                                        auto_place (S.Atomic "String"), Atom.builtin "name";
                                        auto_place (S.Atomic "Wait"), Atom.builtin "wait";
                                        auto_place (S.Atomic "boolean"), Atom.builtin "run_now"
                                    ];
                                    throws          = [];
                                    is_constructor = true;
                                    body = AbstractImpl ([
                                        auto_place (S.IfStmt(
                                            S_A2.e2_e (S.BinopExpr(
                                                S_A2.e2var (Atom.builtin "run_now"),
                                                AstUtils.Equal,
                                                S_A2.e2_lit (S.BoolLit true)
                                            )),
                                            auto_fplace (S.ExpressionStmt(
                                                S_A2.e2_e (S.CallExpr(
                                                    S_A2.e2_e(S.AccessExpr(
                                                        S_A2.e2_e S.This,
                                                        S_A2.e2var run_method
                                                    )),
                                                    []
                                                ))
                                            )),
                                            None
                                        ))
                                    ])
                                }
                            }
                        ];
                        receiver = auto_place {
                            S.annotations     = [ S.Visibility S.Public ];
                            decorators      = [ S.Override];
                            v = {
                                S.args          = [];
                                throws          = [];
                                body            = S.AbstractImpl ([
                                    auto_place (S.ReturnStmt(
                                        auto_place(S.CallExpr(
                                            auto_place(S.AccessExpr(
                                                auto_place(S.CallExpr(
                                                    auto_place(S.VarExpr (
                                                        Atom.builtin "newReceiveBuilder"
                                                    ), auto_place S.TUnknown),
                                                    []
                                                ), auto_place S.TUnknown), 
                                                auto_place(S.VarExpr (
                                                    Atom.builtin "build"
                                                ), auto_place S.TUnknown)
                                            ), auto_place S.TUnknown),
                                            []
                                        ), auto_place S.TUnknown)
                                    ))
                                ]);
                                name            = Atom.builtin "createReceive";
                                ret_type        = Rt.Misc.t_receive_of_actor fplace name;
                                is_constructor  = false
                            }
                        };
                        states = [];
                        events = [];
                        nested_items = [];
                        static_items = [];
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
                let actor_name = Atom.builtin "Laststage" in
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
                    Atom.builtin (String.capitalize_ascii (Atom.hint mdef.bootstrap))
                else mdef.bootstrap
            in

            let generate_no_main (mdef:Target.maindef) = 
                assert(Atom.is_builtin mdef.entrypoint && Atom.hint mdef.entrypoint = "no_main");

                (* Just starts the actor system with the specific guardian i.e. mdef.bootstrap *)
                wrap_main mdef.name (auto_place (S.VarExpr (guardian_name_of mdef), auto_place S.TUnknown)) (auto_place {
                    S.annotations = [S.Visibility S.Public];
                    decorators = [];
                    v = {
                        S.ret_type = auto_place (S.Atomic "void");
                        name = Atom.builtin (String.capitalize_ascii (Atom.hint mdef.entrypoint));
                        body = AbstractImpl [];
                        args = [auto_place (S.Atomic "String[]"), Atom.builtin "args"];
                        throws          = [];
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
                        logger#error "%s" (Atom.to_string laststage.name);
                        assert("Stage" = Atom.hint laststage.name); (* FIXME fragile *)
                        _stages @  (add_guardian mdef laststage)
                    | _ -> 
                        let rec update_guardian = function
                        | [] -> []
                        | stage::stages when stage.name = mdef.bootstrap -> begin 
                            assert( List.length stage.ast = 1);

                            match List.hd stage.ast with 
                            | {place; value={annotations; decorators; v=S.ClassOrInterfaceDeclaration guardian}} -> begin
                                (* guardian like added by interface plgs*)
                                let ast = {place; value={S.annotations; decorators; v=S.ClassOrInterfaceDeclaration guardian}} in
                                let stage = { stage with ast = [ast]} in
                                stage::stages
                            end
                            | {place; value={annotations; decorators; v=S.Actor guardian}} -> begin
                                let a_run_method = Atom.fresh "run" in
                                let run_method args stmts = auto_place {
                                    S.decorators = [];
                                    annotations = [S.Visibility S.Public];
                                    v = {
                                        S.ret_type = auto_place (S.Atomic "Void");
                                        name = a_run_method;
                                        args = args;
                                        throws          = [];
                                        is_constructor = false;
                                        body = AbstractImpl (stmts)
                                    }
                                } in



                                let rec change_constructor : S.method0 list -> S.method0 list = function
                                | [] -> []
                                | m::ms when m.value.v.is_constructor ->
                                begin
                                    let rmethod = match m.value.v.body with
                                        | AbstractImpl stmts -> [run_method m.value.v.args stmts]
                                        | _ -> []
                                    in

                                    let m = { m with
                                        value = { m.value with
                                            annotations = m.value.annotations;
                                            v = { m.value.v with
                                                S.ret_type = auto_place (S.Atomic "Void");
                                                name = m.value.v.name;
                                                args = 
                                                    (auto_place (S.Atomic "String"), Atom.builtin "name")
                                                    :: (auto_place (S.Atomic "Wait"), Atom.builtin "wait")
                                                    :: (auto_place (S.Atomic "boolean"), Atom.builtin "run_now")
                                                    :: m.value.v.args;
                                                is_constructor = true;
                                                body = match m.value.v.body with
                                                | AbstractImpl stmts -> AbstractImpl ([
                                                    auto_place (S.IfStmt(
                                                        S_A2.e2_e (S.BinopExpr(
                                                            S_A2.e2var (Atom.builtin "run_now"),
                                                            AstUtils.Equal,
                                                            S_A2.e2_lit (S.BoolLit true)
                                                        )),
                                                        auto_fplace (S.ExpressionStmt(
                                                            S_A2.e2_e (S.CallExpr(
                                                                S_A2.e2_e(S.AccessExpr(
                                                                    S_A2.e2_e S.This,
                                                                    S_A2.e2var a_run_method
                                                                )),
                                                                List.map (function (_, x) -> S_A2.e2var x) m.value.v.args 
                                                            ))
                                                        )),
                                                        None
                                                    ))
                                                ])
                                                | b -> b
                                            }
                                        }  
                                    } in

                                    m::(rmethod@ms)

                                    
                                end
                                | m::ms -> m::(change_constructor ms)
                                in
                                let guardian = {
                                    guardian with 
                                        value = {guardian.value with 
                                            is_guardian = true;
                                            extended_types = [Rt.Misc.t_lg4dc_abstract_system fplace];
                                            methods = change_constructor guardian.value.methods;
                                        }
                                    }
                                in

                                let ast = {place; value={S.annotations; decorators; v=S.Actor guardian}} in
                                let stage = { stage with ast = [ast]} in
                                stage::stages
                            end
                        end
                        | stage::stages -> stage:: (update_guardian stages)
                        in

                        update_guardian stages
                in

                let add_main_classes stages = match Atom.hint mdef.entrypoint with
                    | "no_main" -> begin
                        match split_list (List.length stages - 1) stages with
                        | _stages, [laststage] -> _stages @ [add_no_main mdef laststage]
                        | _ -> raise (Error.DeadbranchError "no last stage found") 
                    end
                    | _ -> 
                        let selector = function 
                            | {S.v=S.MethodDeclaration x} -> x.value.v.name = mdef.entrypoint 
                            | _-> false
                        in
                        let replace = function
                            | {S.v=S.MethodDeclaration m0} -> 
                                (wrap_main mdef.name (auto_place (S.VarExpr (guardian_name_of mdef), auto_place S.TUnknown)) m0).value 
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
                let current_mains = List.map (function 
                    |({value={S.v=S.ClassOrInterfaceDeclaration cid}} as t) -> {
                        kind = MainStage;
                        name = cid.name;
                        sub_stages = [];
                        ast = [t];
                        imports = [];
                        file = None;
                        package_name = None;
                    }
                    | _ -> raise (Error.DeadbranchError "ill-formed main")
                ) mains_t in
                
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
                        if stage.name = x then (
                            Atom.refresh_hint x (Option.get stage.package_name^"."^(Atom.hint x))
                        ) else (
                            Atom.refresh_hint x (Option.get stage.package_name^"."^(Atom.to_string stage.name)^"."^(Atom.hint x))
                        )
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

                let main_renaming (x:Atom.atom) : Atom.atom = 
                    match Hashtbl.find_opt main_state_rename x with 
                    | None -> x
                    | Some new_x ->
                            new_x
                in

                rename_cstate main_renaming;
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
        with Not_found -> Atom.value x

        let rec finish_ctype place : S._ctype -> T._jtype = 
            let fplace = place@(Error.forge_place "Plg=AkkaJava/finish_ctype" 0 0) in
            let auto_place smth = {place = fplace; value=smth} in
        function 
            | S.Atomic s -> T.TAtomic s 
            | S.ActorRef {value=S.TVar x} -> T.ClassOrInterfaceType  (auto_place (T.TAtomic "ActorRef"), [fctype (Rt.Misc.t_command_of_actor place x)])  
            | S.ActorRef ct -> T.ClassOrInterfaceType  (auto_place (T.TAtomic "ActorRef"), [fctype ct]) 
            | S.TActivationRef {value=S.TVar x} -> T.ClassOrInterfaceType  (auto_place (T.TAtomic "ActivationRef"), [fctype (Rt.Misc.t_command_of_actor place x)])  
            | S.TActivationRef ct -> T.ClassOrInterfaceType  (auto_place (T.TAtomic "ActivationRef"), [fctype ct]) 
            | S.TFunction (t1, t2) -> T.ClassOrInterfaceType  ( auto_place (T.TAtomic "Function"), [fctype t1; fctype t2]) 
            | S.TArray t1 -> T.ClassOrInterfaceType  (auto_place (T.TArray (fctype t1)), [])
            | S.TList t1 -> T.ClassOrInterfaceType  (auto_place (T.TAtomic "List"), [fctype t1])
            | S.TMap (t1, t2) -> T.ClassOrInterfaceType  (auto_place (T.TAtomic "HashMap"), [fctype t1; fctype t2])
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
            | S.TRaw str -> T.TAtomic str
            | S.TUnknown -> T.TUnknown
            | S.TBB bbterm -> T.TBB (fbbterm bbterm)
        and fctype ct : T.jtype = map_place finish_ctype ct

        and finish_unop = Fun.id 
        and finish_binop = Fun.id 

        and finish_literal place : S._literal -> T._literal= function
            | S.VoidLit -> T.VoidLit
            | S.BoolLit b -> T.BoolLit b
            | S.FloatLit f -> T.FloatLit f
            | S.IntLit f -> T.IntLit f
            | S.StringLit s -> T.StringLit s
        and fliteral lit : T.literal = map_place finish_literal lit

        and finish_expr place (e, ct) : T._expr * T.jtype = 
        let fplace = place@(Error.forge_place "Plg=AkkaJava/finish_expr" 0 0) in
        let auto_place smth = {place = fplace; value=smth} in
        (match e with
            | S.AccessExpr (e1,e2) -> T.AccessExpr (fexpr e1, fexpr e2)
            | S.AccessMethod (e1,x) -> T.AccessMethod (fexpr e1, x)
            | S.ActivationRef{schema; actor_ref} ->
                (* public ActivationRef (String componentSchema, ActorRef actorRef, Boolean isInterceptor, Optional<ActivationRef> interceptedActivationRef_opt) *)
                T.NewExpr(
                    auto_place(T.VarExpr(Atom.builtin "ActivationRef"), auto_place T.TUnknown),
                    [
                        fexpr schema;
                        fexpr actor_ref;
                        auto_place (T.LiteralExpr (auto_place(T.BoolLit false)), auto_place T.TUnknown);
                        fexpr (Rt.Misc.e_none fplace);
                    ]
                )              
            | S.InterceptedActivationRef {actor_ref; intercepted_actor_ref} -> 
                T.NewExpr(
                    auto_place(T.VarExpr(Atom.builtin "ActivationRef"), auto_place T.TUnknown),
                    [
                        fexpr actor_ref;
                        match intercepted_actor_ref with
                        | None -> fexpr (Rt.Misc.e_none fplace)
                        | Some e2 -> fexpr (Rt.Misc.e_some fplace e2) 
                    ]
                )              
            | S.AssertExpr e -> T.AssertExpr (fexpr e)                   
            | S.BinopExpr (e1, op, e2) -> 
                T.BinaryExpr ( fexpr e1, op, fexpr e2) 
            | S.CallExpr (e1,e2) -> T.AppExpr(fexpr e1, List.map fexpr e2) 
            | S.CastExpr (ct,e) -> T.CastExpr(fctype ct, fexpr e) 
            | S.ClassOf ct -> begin
                match ct.value with
                | S.Atomic x -> T.VarExpr (Atom.builtin x)
                | S.TVar x -> T.AccessExpr( 
                    {place = ct.place; value=T.VarExpr x, auto_place T.TUnknown}, 
                    {place = ct.place; value = T.VarExpr (Atom.builtin "class"), auto_place T.TUnknown}
                    )
                | _ -> Error.perror place "This not a Java class, can not get class name"
            end 
            | S.CurrentContext -> 
                T.AppExpr (
                    {place; value=T.VarExpr (Atom.builtin "getContext"), auto_place T.TUnknown},
                    []
                )
            | S.CurrentSystem -> T.AccessExpr 
                ({  
                    place;
                    value = T.AppExpr 
                    ({ place; value=T.VarExpr (Atom.builtin "getContext"), auto_place T.TUnknown},
                    []), auto_place T.TUnknown
                },
                { 
                    place;
                    value = T.AppExpr
                        ({ place; value =T.VarExpr (Atom.builtin "getSystem"), auto_place T.TUnknown},
                        []), auto_place T.TUnknown
                }) 
            | S.LambdaExpr (params, stmt) -> 
                T.LambdaExpr (
                    List.map (function (mt, x) -> fctype mt, x) params, 
                    fstmt stmt
                )
            | S.LitExpr lit -> LiteralExpr (fliteral lit)
            | S.Spawn {context; actor_expr} -> T.AccessExpr (fexpr context, fexpr actor_expr)
            | S.TernaryExpr (e1, e2, e3) -> T.TernaryExpr (fexpr e1, fexpr e2, fexpr e3)
            | S.This -> T.ThisExpr
            | S.BlockExpr (b, es) -> begin
                match b with
                | List when es = [] -> T.NewExpr(
                    auto_place(T.VarExpr(Atom.builtin "ArrayList"), auto_place T.TUnknown),
                    []
                ) 
                | List -> T.AppExpr(
                    auto_place(T.VarExpr(Atom.builtin "List.of"), auto_place T.TUnknown),
                    List.map fexpr es
                )
                | Set when es = [] -> T.NewExpr(
                    auto_place(T.VarExpr(Atom.builtin "HashSet"), auto_place T.TUnknown),
                    []
                ) 
                | Set -> T.AppExpr(
                    auto_place(T.VarExpr(Atom.builtin "Set.of"), auto_place T.TUnknown),
                    List.map fexpr es
                )
                | Tuple when es = [] -> T.NewExpr(
                    auto_place(T.VarExpr(Atom.builtin "Tuple"), auto_place T.TUnknown),
                    []
                ) 
                | Tuple -> T.AppExpr(
                    auto_place(T.VarExpr(Atom.builtin "Tuple.of"), auto_place T.TUnknown),
                    List.map fexpr es
                )
                | Block -> raise (Error.PlacedDeadbranchError (place, "Block expr should have been compiled away!"))
            end
            | S.Block2Expr (b, ees) -> begin
                match b with
                | Dict when ees = [] -> T.NewExpr(
                    auto_place(T.VarExpr(Atom.builtin "HashMap"), auto_place T.TUnknown),
                    []
                ) 
                | Dict -> 
                    let es = List.map (function (e1, e2) -> auto_place (S.BlockExpr(AstUtils.Tuple, [e1; e2]), auto_place S.TUnknown) ) ees in
                    T.AccessExpr(
                        fexpr (auto_place(S.BlockExpr (AstUtils.List, es),  auto_place S.TUnknown)),
                        auto_place (T.RawExpr "stream().collect(Collectors.toMap(t -> t.0, t -> t.1));", auto_place T.TUnknown)
                    )
            end
            | S.UnopExpr (AstUtils.UnpackOrPropagateResult, e) -> 
                (*  Encoding
                    e.getOrElseThrow(t -> new RuntimeException("The result is failure, can access the success."))
                *)
                let t = Atom.fresh "t" in

                let sign = fctype (auto_place (S.TFunction(
                    snd e.value,
                    ct
                ))) in

                T.AppExpr ( 
                    auto_place (T.AccessExpr (
                        fexpr e, 
                        auto_place(T.VarExpr (Atom.builtin "getOrElseThrow"), auto_place T.TUnknown)
                    ), auto_place T.TUnknown),
                    [
                        auto_place (T.LambdaExpr (
                            [auto_place T.TUnknown, t], 
                            auto_place ( T.ReturnStmt (auto_place( 
                                    T.NewExpr (
                                        auto_place (T.VarExpr (Atom.builtin "RuntimeException"), auto_place T.TUnknown),
                                        [
                                            auto_place (T.LiteralExpr (auto_place (T.StringLit "The result is failure, can access the success.")), auto_place T.TUnknown)
                                        ]
                                    ), auto_place T.TUnknown
                                ))
                            )
                        ), sign)
                    ]
                ) (* TODO should return form the fct with the error or return the result*) 
            | S.UnopExpr (op, e) -> T.UnaryExpr (op, fexpr e) 
            | S.VarExpr x -> T.VarExpr x             
            | S.NewExpr (e, es) -> T.NewExpr (fexpr e, List.map fexpr es)             
            | S.RawExpr str -> T.RawExpr str
            | S.BBExpr bbterm -> T.BBExpr (fbbterm bbterm) 
        ), fctype ct
        and fexpr expr : T.expr = map_place finish_expr expr

        and finish_bbterm place {S.language; body} = 
            List.map (
                function 
                | S.Text t -> T.Text t
                | S.Varda e -> T.Varda (fexpr e)
            ) body
        and fbbterm bbterm: T.blackbox_term = (map_place finish_bbterm) bbterm

        and finish_stmt place : S._stmt -> T._stmt = 
        let fplace = place@(Error.forge_place "Plg=AkkaJava/finish_stmt" 0 0) in
        let auto_place smth = {place = fplace; value=smth} in
        function
            | S.AssignExpr (e1, e2) -> T.ExpressionStmt ( auto_place(T.AssignExpr(fexpr e1, T.AssignOp, fexpr e2), auto_place T.TUnknown))
            | S.BlockStmt stmts -> T.BlockStmt (List.map fstmt stmts)
            | S.BreakStmt -> T.BreakStmt
            | S.CommentsStmt c -> T.CommentsStmt c
            | S.ContinueStmt -> T.ContinueStmt
            | S.ExpressionStmt e -> T.ExpressionStmt(fexpr e)
            | S.EmptyStmt -> T.EmptyStmt
            | S.IfStmt (e, stmt1, stmt2_opt) -> T.IfStmt (fexpr e, fstmt stmt1, Option.map fstmt stmt2_opt)              
            | S.ForStmt (mt, x, e, stmt) -> T.ForStmt(fctype mt, x, fexpr e, fstmt stmt)
            | S.LetStmt (ct, x, None) -> 
                T.NamedExpr (fctype ct, x, None) (* TODO FIXME maybe not the semantic that we want, we need to add this to the doc*)  
            | S.LetStmt (ct, x, Some e) -> T.NamedExpr (fctype ct, x, Some (fexpr e))
            | S.ReturnStmt e -> T.ReturnStmt (fexpr e)
            | S.TryStmt (stmt, branches) -> T.TryStmt (
                fstmt stmt,
                List.map (function (ct, x, stmt)->
                    fctype ct,
                    x,
                    fstmt stmt
                ) branches
            )
            | S.RawStmt str -> T.RawStmt str
            | S.TemplateStmt (str, inner_models) -> 
                    let str = Jg_template.from_string str ~models:(inner_models@TemplatesHelper.default_jingoo_models) in
                    T.RawStmt str
        and fstmt stmt : T.stmt = map_place finish_stmt stmt

        let rec finish_state  (state:S.state) : T.str_items list = 
        match state.value with
            | s when s.persistent -> failwith "TODO finish _state with persistency" 
            | s -> List.map (function x -> {place = state.place; value=T.Stmt x}) (List.map fstmt s.stmts)

        and finish_event place ({vis; name;  args; headers}: S._event) :  T._str_items = 
            let fplace = place@(Error.forge_place "Plg=AkkaJava/finish_event" 0 0) in
            let auto_place smth = {place = fplace; value=smth} in

            (* Collect specific import *)
            current_headers := headers @ !current_headers;

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
                            { place = ct.place; value =T.ThisExpr, auto_place T.TUnknown}, { place = ct.place; value =T.VarExpr x, auto_place T.TUnknown}), auto_place T.TUnknown
                        },
                        T.AssignOp,
                        { place = ct.place; value = T.VarExpr x, auto_place T.TUnknown}
                    ), auto_place T.TUnknown
                    }
                )
                }
            in

            let constructor = 
                { 
                    place; 
                    value = T.Body ({place; value= {
                        T.annotations = [T.Visibility T.Public];
                        decorators  = [T.JsonCreator];
                        v = T.MethodDeclaration {
                            ret_type    = None;
                            name        = name;
                            parameters  = List.map (function (decorators, ct, x) -> (T.JsonProperty x::decorators,ct, x)) (List.map finish_arg args);
                            throws      = [];
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

            (* generate the _0_, ..., _n_ getters *)
            let make_getters args = 
                let fplace = (Error.forge_place "Plg=Akka/make_getter" 0 0) in
                let auto_fplace smth = {place = fplace; value=smth} in
                let make_getter i (ct, name) = fterm (auto_fplace {
                    S.annotations = [];
                    decorators = [];
                    v = S.MethodDeclaration (auto_fplace {
                        S.annotations = [S.Visibility S.Public];
                        decorators = [];
                        v = {
                            S.ret_type = ct;
                            name = Atom.builtin (Printf.sprintf "_%d_" i);
                            body = S.AbstractImpl [
                                auto_fplace (S.ReturnStmt (
                                    auto_fplace (S.AccessExpr(
                                        auto_fplace (S.This, auto_fplace S.TUnknown),
                                        auto_fplace (S.VarExpr name, auto_fplace S.TUnknown)
                                    ), ct)
                                ))
                            ];
                            args = [];
                            throws      = [];
                            is_constructor = false; 
                        }
                    })
                }) in
                List.mapi make_getter args
            in
            let getters = make_getters args in
            
            logger#info "event %s can be received by up to %d" (Atom.to_string name) (List.length implemented_types);

            T.Body {
                place; 
                value = {
                    T.annotations         = [T.Visibility (finish_visibility vis); T.Static; T.Final];
                    decorators = [];
                    v = T.ClassOrInterfaceDeclaration {
                        isInterface         = false;
                        name                = name;
                        parameters          = []; 
                        extended_types      = [fctype (Rt.Misc.t_lg4dc_event fplace None)];
                        implemented_types   = implemented_types; 
                        body                = constructor::(fields@getters)
                    }
                }
            }
        and fevent e : T.str_items = map_place finish_event e

        and finish_arg ((ctype,variable):(S.ctype * Atom.atom)) : T.parameter =
            ([], fctype ctype, variable)
        and finish_method_v is_guardian is_actor_method place ({ret_type; name; body; args; is_constructor; throws}: S._method0) : T._body = 
            match body with
            | S.AbstractImpl stmts when is_constructor ->
                let args = match is_actor_method with
                | true -> 
                    (
                        if is_guardian then
                            (auto_place (S.TParam (
                                auto_place (S.TVar (Atom.builtin "ActorContext")),
                                [ auto_place(S.Atomic "SpawnProtocol.Command")]
                            )), Rt.Misc.a_context)
                        else
                        (Rt.Misc.t_actor_context place None, Rt.Misc.a_context)
                    )
                    :: (Rt.Misc.t_actor_timer place is_guardian None, Rt.Misc.a_timers)
                    :: (
                        if is_guardian then []
                        else
                            [Rt.Misc.t_actor_guardian place, Rt.Misc.a_guardian]
                    ) @ args
                | false -> args
                in
                
                let stmts = match is_actor_method with
                | true -> begin
                    let l_event_name : Atom.atom = (Atom.fresh "e") in
                    let l_event : S.expr = auto_place (S.VarExpr l_event_name, auto_place S.TUnknown) in
                    let generate_case_for_timer (event_name, handler_name) = 
                        (* 
                            if eventName.isInstance(e){
                                    handler( getContext(), this.frozen_sessions,
                            this.dead_sessions, 
                            msg)
                                return null;)
                            }
                        *)
                        auto_place( S.IfStmt(
                            Rt.Misc.e_is_instance fplace (auto_place (S.VarExpr (Atom.builtin event_name), auto_place S.TUnknown)) l_event,
                            auto_place(S.BlockStmt [
                                auto_place (S.ExpressionStmt( auto_place (S.CallExpr( 
                                    auto_place (S.VarExpr (Atom.builtin handler_name), auto_place S.TUnknown),
                                    [
                                        auto_place(S.CastExpr(
                                            auto_place (S.TVar (Atom.builtin "ActorContext")),
                                            Rt.Misc.e_get_context fplace
                                        ), auto_place S.TUnknown);
                                        auto_place(S.CastExpr(
                                            auto_place (S.TVar (Atom.builtin "ActivationRef")),
                                            Rt.Misc.e_get_self_activation fplace (Rt.Misc.e_get_context fplace)
                                        ), auto_place S.TUnknown);
                                        Rt.Misc.e_this_frozen_sessions fplace; 
                                        Rt.Misc.e_this_dead_sessions fplace; 
                                        Rt.Misc.e_this_intermediate_states fplace;
                                        l_event;
                                    ]
                                ), auto_place S.TUnknown)));
                                auto_place (S.ReturnStmt (auto_place(S.LitExpr (auto_place S.VoidLit), auto_place S.TUnknown)));
                            ]),
                            None
                        ))
                    in

                    let set_timers : S.stmt = 
                        auto_place(S.AssignExpr( 
                            Rt.Misc.e_this_timers fplace,
                            auto_place(S.VarExpr Rt.Misc.a_timers, auto_place S.TUnknown)
                        ))
                    in 

                    let set_guardian : S.stmt = 
                        if is_guardian then
                            auto_place(S.AssignExpr( 
                                Rt.Misc.e_this_guardian fplace,
                                Rt.Misc.e_get_self_actor place (Rt.Misc.e_get_context place) 
                            ))
                        else
                            auto_place(S.AssignExpr( 
                                Rt.Misc.e_this_guardian fplace,
                                auto_place(S.VarExpr Rt.Misc.a_guardian, auto_place S.TUnknown)
                            ))
                    in 

                    (
                        if is_guardian then
                            (auto_place ( S.ExpressionStmt (Rt.Misc.e_super place [
                                auto_place(S.VarExpr Rt.Misc.a_context, auto_place S.TUnknown);
                                auto_place(S.VarExpr Rt.Misc.a_timers, auto_place S.TUnknown);
                                auto_place (S.VarExpr (Atom.builtin "name"), auto_place S.TUnknown);
                                auto_place (S.VarExpr (Atom.builtin "wait"), auto_place S.TUnknown)
                            ])))
                        else
                            (auto_place ( S.ExpressionStmt (Rt.Misc.e_super place [auto_place(S.VarExpr Rt.Misc.a_context, auto_place S.TUnknown)])))
                    )
                    :: set_timers
                    :: set_guardian
                    :: stmts
                end
                | false ->  stmts
                in


                (* FIXME check in IR that onstratup no type i.e. void*)
                T.MethodDeclaration {
                    ret_type    = None;
                    name        = name;
                    parameters  = List.map finish_arg args;
                    body        = List.map fstmt stmts;
                    throws      = throws;
                }
            | S.AbstractImpl stmts ->
                T.MethodDeclaration {
                    ret_type    = Some (fctype ret_type);
                    name        =  name;
                    parameters  = List.map finish_arg args;
                    body        = List.map fstmt stmts;
                    throws      = throws;
                }
            | S.BBImpl bbterm ->
                let jingoo_args = Hashtbl.create (List.length args) in
                let aux_arg (decorators, ct,x)= 
                    let buffer = Buffer.create 64 in
                    Lg.Output.output_arg (Format.formatter_of_buffer buffer)(decorators, ct, x);
                    Hashtbl.add jingoo_args (Atom.hint x) (Jg_types.Tstr (Buffer.contents buffer)) 
                in
                List.iter aux_arg (List.map finish_arg args);

                let jingoo_ret_type = 
                    let buffer = Buffer.create 64 in
                    Lg.Output.ojtype (Format.formatter_of_buffer buffer) (fctype ret_type);
                    Buffer.contents buffer 
                in
                let models = [
                    ("name", Jg_types.Tstr (Atom.to_string name));
                    ("ret_type", Jg_types.Tstr jingoo_ret_type);
                    ("args", Jg_types.Thash jingoo_args)
                ] in

                T.MethodDeclaration {
                    ret_type    = if is_constructor then None else Some (fctype ret_type);
                    name;
                    parameters  = List.map finish_arg args;
                    throws      = throws;
                    body        = [ {place=bbterm.place; value=T.BBStmt (fbbterm models bbterm)}]
                }
        and fmethod is_guardian is_actor_method m : T.str_items = {place=m.place; value= T.Body (map_place (finish_annoted (finish_method_v is_guardian is_actor_method)) m)}


        and finish_bbterm models place (bbterm:S._blackbox_term) = 
            List.map (function
                | S.Text str -> T.Text str
                | S.Varda e ->  T.Varda (fexpr e)
                | S.Template (str, inner_models) -> 
                    let str = Jg_template.from_string str ~models:(inner_models@models) in
                    T.Text str
            ) bbterm.body
        and fbbterm models : S.blackbox_term -> T.blackbox_term = map_place (finish_bbterm (models@TemplatesHelper.default_jingoo_models))

        and finish_actor place ({is_guardian; extended_types; implemented_types; name; methods; states; events; nested_items; static_items; receiver; headers}: S._actor): T._str_items =
            let fplace = place@(Error.forge_place "Plg=Akka/finish_actor" 0 0) in
            let auto_place smth = {place = fplace; value=smth} in

            (* At most one constructor *)
            assert( List.length (List.filter (function (m:S.method0) -> m.value.v.is_constructor) methods) <= 1);

            (* Remove from collected_components *)
            if is_guardian then begin
                (!cstate).collected_components := Atom.Set.remove name !((!cstate).collected_components);
                (!cstate).guardian_components := Atom.Set.add name !((!cstate).guardian_components);
            end;
            (* TODO add to a guardian_components *)


            (* Collect specific import *)
            current_headers := headers @ !current_headers;


            (** FIXME public/protected/private should parametrized*)

            let extended_types = match extended_types with
            | [] -> [ 
                auto_place (T.ClassOrInterfaceType (
                    fctype (Rt.Misc.t_lg4dc_abstract_component fplace),
                    [ 
                        fctype (Rt.Misc.t_command_of_actor place name)     
                    ]) 
                )
                (*auto_place (T.ClassOrInterfaceType (
                    auto_place (T.TAtomic "AbstractBehavior"), 
                    [ 
                        fctype (Rt.Misc.t_command_of_actor place name)     
                    ]) 
                )*)
            ]
            | _ -> List.map fctype extended_types
            in

            let command_cl = auto_place ( T.Body (
                auto_place ({
                    T.annotations = [T.Visibility T.Public];
                    decorators = [];
                    v = T.ClassOrInterfaceDeclaration {
                        isInterface = true;
                        name = Rt.Misc.a_command; 
                        parameters = [];
                        extended_types = [fctype (Rt.Misc.t_cborserializable fplace)];
                        implemented_types = [];
                        body = [];
                    }
                })
            )) in

            (**** generate actor create method ****)
            (* TODO check in IR at most once constructor/destructor *)
            let constructor_opt  = List.find_opt (function (m:S.method0) -> m.value.v.is_constructor) methods in
            let constructor_args = match constructor_opt with | None -> [] |Some constructor -> constructor.value.v.args in 

            let states =
                (*if is_guardian then states 
                else*)
                (* TimerScheduler<Command> timers;*)
                auto_place {S.persistent = false; stmts = [
                    auto_place(S.LetStmt(
                        Rt.Misc.t_actor_timer fplace is_guardian None, Rt.Misc.a_timers, None ))
                    ]}
                :: auto_place {S.persistent = false; stmts = [
                    auto_place(S.LetStmt(
                        Rt.Misc.t_actor_guardian fplace, Rt.Misc.a_guardian, None ))
                    ]}
                :: states
            in

            let methods = match constructor_opt with
            | None when is_guardian = false -> (* add a default constructor, will be hydrated with context when running fmethod *)
                auto_place ({
                    S.annotations = [S.Visibility S.Public];   
                    decorators = [];
                    v = {
                        S.ret_type    = auto_place (S.Atomic "Void");
                        name        = name;
                        args        = []; 
                        throws      = [];
                        body        = AbstractImpl [
                        ]; 
                        is_constructor = true;
                    }
                })::methods
            | _ -> methods
            in

            let arg_lambda = auto_place (S.LambdaExpr (
                [auto_place S.TUnknown, Rt.Misc.a_context],
                auto_place (S.BlockStmt [
                    auto_place (S.ReturnStmt (
                        auto_place(S.CallExpr(
                            Rt.Misc.e_behaviors_with_timers fplace,
                            [
                                auto_place (S.LambdaExpr (
                                    [auto_place S.TUnknown, Rt.Misc.a_timers],
                                    auto_place (S.BlockStmt [
                                        auto_place (S.ExpressionStmt (
                                        Rt.Misc.e_debug_of 
                                            place 
                                            (auto_place (S.VarExpr Rt.Misc.a_context, auto_place S.TUnknown)) 
                                            [
                                                auto_place (S.LitExpr (auto_place (S.StringLit (Atom.to_string name^"::create"))), auto_place S.TUnknown)
                                            ]
                                        ));
                                        auto_place (S.ReturnStmt (auto_place (
                                            S.NewExpr (
                                                auto_place (S.VarExpr name, auto_place S.TUnknown),
                                                List.map (function x -> auto_place (S.VarExpr x, auto_place S.TUnknown)) (Rt.Misc.a_context
                                                ::Rt.Misc.a_timers
                                                ::Rt.Misc.a_guardian
                                                ::(List.map snd constructor_args))

                                            ), auto_place S.TUnknown
                                        )));
                                    ])
                                ), auto_place S.TUnknown)
                            ]
                    ), auto_place S.TUnknown)))
                ])
            ), auto_place S.TUnknown) in

            let arg_lambda_guardian = auto_place (S.LambdaExpr (
                [auto_place S.TUnknown, Rt.Misc.a_context],
                auto_place (S.BlockStmt [
                    auto_place (S.ReturnStmt (
                        auto_place(S.CallExpr(
                            Rt.Misc.e_behaviors_with_timers fplace,
                            [
                                auto_place (S.LambdaExpr (
                                    [auto_place S.TUnknown, Rt.Misc.a_timers],
                                    auto_place (S.BlockStmt [
                                        auto_place (S.ExpressionStmt (
                                        Rt.Misc.e_debug_of 
                                            place 
                                            (auto_place (S.VarExpr Rt.Misc.a_context, auto_place S.TUnknown)) 
                                            [
                                                auto_place (S.LitExpr (auto_place (S.StringLit (Atom.to_string name^"::create"))), auto_place S.TUnknown)
                                            ]
                                        ));
                                        auto_place (S.ReturnStmt (auto_place (
                                            S.NewExpr (
                                                auto_place (S.VarExpr name, auto_place S.TUnknown),
                                                List.map (function x -> auto_place (S.VarExpr x, auto_place S.TUnknown)) (Rt.Misc.a_context::Rt.Misc.a_timers::(List.map snd constructor_args))

                                            ), auto_place S.TUnknown
                                        )));
                                    ])
                                ), auto_place S.TUnknown)
                            ]
                    ), auto_place S.TUnknown)))
                ])
            ), auto_place S.TUnknown) in
            (*let arg_lambda_guardian = auto_place (S.LambdaExpr (
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
                    auto_place(S.ExpressionStmt( auto_place (S.CallExpr(
                        auto_place (S.VarExpr (Atom.builtin "prepare_create")),
                        [
                            auto_place (S.VarExpr (Atom.builtin "context"));
                            auto_place (S.VarExpr (Atom.builtin "name"));
                            auto_place (S.VarExpr (Atom.builtin "wait"))
                        ]

                    ))));
                    auto_place(S.ReturnStmt( auto_place (S.NewExpr(
                        auto_place (S.VarExpr name),
                        [
                            auto_place(S.VarExpr Rt.Misc.a_context);
                            auto_place (S.VarExpr (Atom.builtin "wait"))
                        ]

                    ))));
                ])
            )) in*)
            let m_create : S.method0 = auto_place {
                S.annotations = [S.Visibility S.Public; S.Static];
                decorators = [];
                v = {
                    S.ret_type = Rt.Misc.t_behavior_of_actor place name;
                    name = Rt.Misc.a_create_method;
                    body = S.AbstractImpl [auto_place (S.ReturnStmt (Rt.Misc.e_setup_behaviors place [arg_lambda]))];
                    args = (Rt.Misc.t_actor_guardian fplace, Rt.Misc.a_guardian)::constructor_args;
                    throws      = [];
                    is_constructor = false;
                }
            } in


            (*
            public static Behavior<SpawnProtocol.Command> create() {
                return create(null, null);
            }
            *)
            let m_create_guadrian1 : S.method0   = auto_place {
                S.annotations = [S.Visibility S.Public; S.Static];
                decorators = [];
                v = {
                    S.ret_type = Rt.Misc.t_behavior_of_spawnprotocol place;
                    name = Rt.Misc.a_create_method;
                    body = S.AbstractImpl [
                        auto_place (S.ReturnStmt( 
                            S_A2.e2_e (S.CallExpr(
                                S_A2.e2var Rt.Misc.a_create_method,
                                [
                                    S_A2.e2_lit (S.VoidLit);
                                    S_A2.e2_lit (S.VoidLit);
                                    S_A2.e2var (Atom.builtin "run_now")
                                ]
                            ))
                        ))
                    ];
                    args = List.filter (function |(_,x)-> (Atom.hint x <> "wait") && (Atom.hint x <> "name")) constructor_args;
                    throws      = [];
                    is_constructor = false;
                }
            } in

            (*
            public static Behavior<SpawnProtocol.Command> create(String name, Wait wait) {
                return Behaviors.setup(context -> {
                    prepare_create(context, name, wait);

                    start_guardian(context); 

                    return finish_create(wait);
                });
            }
            *)
            let m_create_guadrian2 : S.method0   = auto_place {
                S.annotations = [S.Visibility S.Public; S.Static];
                decorators = [];
                v = {
                    S.ret_type = Rt.Misc.t_behavior_of_spawnprotocol place;
                    name = Rt.Misc.a_create_method;
                    body = S.AbstractImpl [auto_place (S.ReturnStmt (Rt.Misc.e_setup_behaviors place [arg_lambda_guardian]))];
                    args = [
                        auto_place (S.Atomic "String"), Atom.builtin "name";
                        auto_place (S.Atomic "Wait"), Atom.builtin "wait";
                        auto_place (S.Atomic "boolean"), Atom.builtin "run_now";
                    ];
                    throws      = [];
                    is_constructor = false;
                }
            } in
            let methods = 
                if is_guardian then 
                    methods @ [m_create_guadrian1; m_create_guadrian2] 
                else 
                    methods @ [m_create] 
            in
            

            (*************)

            let body : T.str_items list ref = ref [] in
            (* FIXME issue with the type of body*)
            body := !body @ [{place; value=T.Comments (AstUtils.LineComment "Actor state")}];
            body := !body @ (List.flatten (List.map finish_state states));
            body := !body @ [{place; value=T.Comments (AstUtils.LineComment "Actor events")}];
            body := !body @ (List.map fevent events);
            body := !body @ [{place; value=T.Comments (AstUtils.LineComment "Actor internal logics")}];
            body := !body @ (List.map (fmethod is_guardian true) methods);
            body := !body @ [{place; value=T.Comments (AstUtils.LineComment "Nested structures")}];
            if is_guardian = false then body := command_cl :: !body;
            body := !body @ (List.map fterm nested_items);
            body := !body @ [{place; value=T.Comments (AstUtils.LineComment "Static definitions")}];
            body := !body @ (List.map fterm static_items);
            if is_guardian = false then begin
                body := !body @ [{place; value=T.Comments (AstUtils.LineComment "Receiver")}];
                body := !body @ [fmethod is_guardian true receiver];
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
                        extended_types = extended_types;
                        implemented_types = List.map fctype implemented_types;
                        body = !body 
                    }
                }
            } 
        and factor a : T.str_items = map_place finish_actor a

        (*
            is_cl_toplevel
        *)
        and finish_term is_cl_toplevel place {S.annotations; decorators; v=t}: T._str_items =
        match t with 
            | S.Comments c -> T.Comments c
            | Actor a  -> (factor a).value
            | Import s -> T.JModule ({place; value=T.ImportDirective s})
            | Event e  ->  (fevent e).value
            | Stmt {place=p2; value= S.LetStmt(t, x, e_opt)} when is_cl_toplevel -> 
                (* Toplevel stmt -> FieldDeclaration *)
                T.Body {
                    place = p2;
                    value = {
                        annotations = [T.Visibility T.Public] @ (finish_annotations annotations);
                        decorators = finish_decorators decorators;
                        v = FieldDeclaration {
                            type0 = fctype t;
                            name = x;
                            body = Option.map fexpr e_opt
                        }
                    }
                }
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
            | ClassOrInterfaceDeclaration cdcl -> begin
                current_headers := cdcl.headers @ !current_headers;

                T.Body { 
                    place;
                    value = {
                        T.annotations = finish_annotations annotations;
                        decorators = finish_decorators decorators;
                        v = T.ClassOrInterfaceDeclaration {
                            isInterface = cdcl.isInterface;
                            name = cdcl.name;
                            parameters = []; 
                            extended_types = List.map fctype cdcl.extended_types;
                            implemented_types = 
                            List.map fctype cdcl.implemented_types @ 
                            (* External events should implements command type *)
                            (match Hashtbl.find_opt (!cstate).external2receptionists cdcl.name with
                            | None -> []
                            | Some actor_names -> 
                                List.map fctype (List.map (Akka.Misc.t_command_of_actor fplace) actor_names)
                            );
                            body = List.map (fterm ~is_cl_toplevel:true) cdcl.body 
                        }
                    }
                }
            end
            | MethodDeclaration m -> 
                (* Needs to put external annotations/decorators insde the Body of method since str_items can not be annotated *)
                let external_annotations = finish_annotations annotations in
                let external_decorators = finish_decorators decorators in

                let body = 
                    match fmethod false false m with
                    | ({value=(T.Body body);}) -> body 
                    | _ -> raise (Error.DeadbranchError "fmethod impl prevents accessing this branch")
                in

                T.Body { body with
                    value = { body.value with
                        T.annotations = external_annotations @ body.value.annotations;
                        decorators  = external_decorators @ body.value.decorators 
                    }
                } 
            | RawClass (x, bb_term) ->  T.Body { 
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
                        body = [{ place=bb_term.place; value=T.BBItem (fbbterm [] bb_term)}] 
                    }
                }
            }
            | RawTerm bbterm -> T.BBItem (fbbterm [] bbterm)  
        and fterm ?(is_cl_toplevel=false) t : T.str_items = map_place (finish_term is_cl_toplevel) t

        let finish_program (program:Akka.Ast.program) : ((string * Fpath.t) * Java.Ast.program) list = 
            cstate := Arg.cstate;
            (*TODO finish entrypoint/system*)
            program
            |> split_akka_ast_to_files Arg.target
            |> List.map (function package_name, file, terms -> 
                current_headers := [];
                let terms = List.map fterm terms in
                let imports = List.map (function x ->
                    auto_place (T.BBItem( auto_place [T.Text x]))
                    ) !current_headers 
                in

                (package_name, file), imports@terms
            )

        (*****************************************************)
        let displayed_pass_shortdescription = "Codegen Language AST"
        let displayed_ast_name = "Language AST"
        let show_ast = true
        let precondition program = program
        let postcondition program = program
        let apply_program = finish_program
    end

    (*****************************************************)
    module RtPrepare = Core.IRICompilationPass.Make(Rt.Prepare)


    type plgstate = Rt.Finish.collected_state
    type iplgstate = Plg.Interface_plugin.istate
    let plgstate = ref (Rt.Finish.empty_cstate ())
    let iplgstate = ref (Plg.Interface_plugin.empty_istate ())

    let finish_ir_program (target:Core.Target.target) project_dir build_dir (ir_program: Plugin.S.program) : ((string * Fpath.t) * T.program) List.t =

        let module RFinish = Rt.Finish.Make(struct let target = target end) in
        let module RtFinish = Rt.IRI2AstCompilationPass.Make(RFinish) in

        let (module RtGenInterface), (module RtGenInterfacePass) = Akka.Interfaces.load_and_make target.value.codegen.interface_plg build_dir target in

        let target, ir_after_interface_program, interface_program = 
            match RtGenInterfacePass.apply ir_program with
            | [(target, ir_after_interface_program), interface_program] -> target, ir_after_interface_program, interface_program
            | _ -> failwith "RtGenInterfacePass returns an ill-formed results"
        in

        (* Add new compilation targets, must be set before calling resolve_templates *)
        (!RFinish.cstate).target <- Some target;

        let ir_program = ir_after_interface_program in

        (* Warning: must be called after exactly one apply, since it needs Module state *)
        RtGenInterface.update_build_dir project_dir build_dir;
        RtGenInterface.resolve_templates project_dir build_dir;

        let program = ir_program
            |> RtPrepare.apply
            |> RtFinish.apply
        in
        let program = program @ interface_program in

        let module Akka2Java0 = MakeRt2Lg(struct
            let target = target
            let cstate = !RFinish.cstate
        end) in 
        let module Akka2Java = Akka2JavaCompilationPass.Make(struct include Akka2Java0 end) in

        let res = Akka2Java.apply program in
        plgstate := !Akka2Java0.cstate; (* Order matters state could be udated by "AAkka2Java.apply program"*)
        iplgstate := !RtGenInterface.istate;
        res


    (** Output program*)
    let output_program target project_dir build_dir ir_program =
        let headers = auto_place (T.Raw headers) in

        ir_program
        |> finish_ir_program target project_dir build_dir
        |> List.rev (* order stages by dependencies order *)
        |> List.map (function ((package_name, file), program) -> 
            let module Clean = Lg.Clean.Make(struct let filename = (Fpath.to_string file) end) in
            let module Clean = Lg.AstCompilationPass.Make(Clean) in
            let module HumanReadable = Lg.HumanReadable.Make(struct let filename = (Fpath.to_string file) end) in
            let module HumanReadable = Lg.AstCompilationPass.Make(HumanReadable) in
            
            package_name, file, 
                program
                |> Clean.apply
                |> HumanReadable.apply 
        )
        (* Add general headers *)
        |> List.map (function (package_name, file, program) -> (package_name, file, headers :: program))
        |> List.iter (function (package_name, file, program) -> Lg.Output.output_program package_name (Fpath.append build_dir file) program)

    let auto_jingoo_env (cstate:Rt.Finish.collected_state) (istate:Plg.Interface_plugin.istate) places = 
        let target:Core.Target.target = 
            try Option.get cstate.target 
            with Invalid_argument _ -> failwith "[akka] target has not been set into cstate before calling auto_jinja_env"
        in

        let rec prepare_places parent_name : IR.vplace list -> (string * IR.vplace) list = function 
        | [] -> []
        | p::ps -> 
            let inner_parent_name = match parent_name with
                | "" -> Atom.hint p.name
                | _ -> parent_name^"::"^(Atom.hint p.name)
            in
            (inner_parent_name, p):: 
            (prepare_places inner_parent_name p.children) @ (prepare_places parent_name ps)
        in 

        let places = prepare_places "" places in

        let component_names = List.of_seq (Atom.Set.to_seq(!(cstate.collected_components))) in

        let models = [
            ("target_mains", Jg_types.Tlist 
                (List.map (function ({Core.Target.name;}:Core.Target.maindef) -> Jg_types.Tstr name) target.value.codegen.mains)
            );
            ("target_entrypoints", Jg_types.Tlist 
                (List.map (function ({Core.Target.bootstrap;}:Core.Target.maindef) -> Jg_types.Tstr (Atom.to_string bootstrap)) target.value.codegen.mains)
            );
            ("components", Jg_types.Tlist (
                List.map (function a -> Jg_types.Tstr (Atom.to_string a)) component_names 
            ));
            ("components_command", Jg_types.Tlist (
                List.map (function a -> Jg_types.Tstr ((Atom.to_string a)^".Command")) component_names
            ));
            ("system_name", Jg_types.Tstr ( Akka.Misc.system_name));
            ("vplaces", Jg_types.Tlist (
                List.map (function ((key,vp):string * IR.vplace) -> 
                    Jg_types.Tobj [
                        "key", Jg_types.Tstr key;
                        "vp", Jg_types.Tobj [
                            "name", Jg_types.Tstr (Atom.hint vp.name)
                        ] 
                    ]
                ) places
            ));
            ("dependencies", Jg_types.Tstr (dependencies));
        ] @ istate.jingoo_models in

        (* User-define is a template *)
        let models =  
            ("user_defined_targets", Jg_types.Tstr (Jg_template.from_string target.value.user_defined ~models:(models@TemplatesHelper.default_jingoo_models))) :: models in

        models

    let custom_template_rules () = [
    ]
    let custom_external_rules () = []
end