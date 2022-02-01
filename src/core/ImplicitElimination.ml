open IR
open Easy_logging
open Utils
open AstUtils

let logger = Logging.make_logger "_1_ compspec" Debug [];;
module type Params = sig
    val gamma : (IR.expr_variable, IR.main_type) Hashtbl.t
    val targets : Target.targets 
end

module type Sig = sig
    include IRCompilationPass.Pass
end

module Make (Args : Params ) : Sig = struct
    (*
        - make explict the implicit constructor arguments and stored then as component attributes
            after rewrite all ImplicitVarExpr should have been rewritten to classical VarExpr and this...
    *)
    include Args 
    (* We need targets since guardian can not have any implicit variables - since there is no shared memory between guardians and instance of guardians 
    TODO TODOC somewhere *)



    module Env = Set.Make( struct
      type t = main_type * Atom.atom
      let compare (a1:t) (a2:t) = Atom.compare (snd a1) (snd a2) 
    end)

    let print_env env = 
        Format.fprintf Format.std_formatter "Env: {%a}\n\n" 
            (Error.pp_list 
                "; " 
                (fun out (_,x) -> Format.fprintf out "%s" (Atom.to_string x))
            ) 
            (List.of_seq (Env.to_seq env))

    (* Component name -> implicit set*)
    let implicits : (Atom.atom, Env.t) Hashtbl.t = Hashtbl.create 32  
    (* component name -> list (component name of an inner spawn, already_binded variable of the scope of this spawn) *)
    let inner_spawns : (Atom.atom, (component_expr * Atom.Set.t)list) Hashtbl.t  = Hashtbl.create 32
    (* (selector, rewriter) that should be applied globaly to rewrite spawn *)
    let spawn_rewritings = ref []

    (* return name of intermediate states * citems *)
    let rec rewrite_component_item place : _component_item -> _component_item = 
    let fplace = (Error.forge_place "Core.Rewrite" 0 0) in
    let auto_place smth = {place = place; value=smth} in
    let auto_fplace smth = {place = fplace; value=smth} in
    function
    | State _ as citem -> citem
    | Contract _ as citem -> citem 
    | Method m as citem -> citem
    |InPort _ as citem -> citem
    | Outport _ as citem -> citem
    | Term t -> Term (rterm t)
    | Include _ as citem -> citem
    and rcitem citem : component_item = map_place rewrite_component_item citem

    and rewrite_component_dcl place : _component_dcl -> _component_dcl = 
    let fplace = (Error.forge_place "Core.Rewrite" 0 0) in
    let auto_place smth = {place = place; value=smth} in
    let auto_fplace smth = {place = fplace; value=smth} in
    function
    | ComponentAssign _ as cdcl -> cdcl
    | ComponentStructure cdcl -> 
        let body = cdcl.body in
        (* Make implicit constructor variables explicit *)
        let _, implicit_vars = free_vars_component_dcl (List.fold_left (fun set {value=mt,x} -> Atom.Set.add x set ) Atom.Set.empty cdcl.args)  {place; value=ComponentStructure cdcl} in (* Variable coming from outside the component, that have not been reduced to value during partial evaluation*)
       
        logger#info "registering in implicits %s" (Atom.to_string cdcl.name);
        Hashtbl.add implicits cdcl.name (Env.of_seq (List.to_seq implicit_vars)); (* Free vars only at this point, implicit comming from inner spawn will be computed later on *)

        (* C, scope*)
        let selector = function | Spawn _ as e -> true | _ -> false in
        let collector parent_opt env e = 
            match fst e.value with  
            | Spawn {c; args; at} -> [(c, env)] 
            | _ -> []
        in
        let spawns = collect_expr_component_dcl (Some cdcl.name) Atom.Set.empty selector collector {place; value=ComponentStructure cdcl} in (* NB: need to call collect_expr_component_dcl directly in order to include component wide scope *)
        let spawns = match spawns with | _, elts, _ -> elts in

        (match Hashtbl.find_opt inner_spawns cdcl.name with
        | None -> Hashtbl.add inner_spawns cdcl.name spawns;
        | Some already ->  Hashtbl.add inner_spawns cdcl.name  (already @ spawns)
        );
        
        (* Collect spawn rewriting with implicit *)
        let select_spawn = function 
            | Spawn {c={value=VarCExpr name, _} as c; args; at} when name = cdcl.name -> true
            | Spawn {c={value=VarCExpr name, _}} -> logger#debug "spawn of %s in %s" (Atom.to_string name) (Atom.to_string cdcl.name); false
            | Spawn _ -> failwith "Not a VarCExpr in Spawn, should not happen after partial evaluation" 
            | _ -> false
        in
        let replace_spawn = function
            | Spawn {c={value=VarCExpr name, _} as c; args; at} when name = cdcl.name-> 
                logger#debug "Replacing spawn for %s" (Atom.to_string name);
                Spawn {
                c; 
                args = (List.map (function(mt, x) -> 
                    auto_fplace (AccessExpr (
                        auto_fplace (This, auto_fplace (
                            CompType (auto_fplace(
                                CompTUid cdcl.name 
                            ))
                        )), 
                        auto_fplace (VarExpr x, mt)
                    ),
                    mt)
                ) implicit_vars)@args;
                at} 
            | _ as e -> e 
        in
        spawn_rewritings := (select_spawn, replace_spawn) :: !spawn_rewritings; (* Collect golbal rewriting - works since each binders create a unique variable name *)

        let body = List.map rcitem body in
        
        ComponentStructure { cdcl with body }
    and rcdcl cdcl = map_place rewrite_component_dcl cdcl 

    and rewrite_term place = function
    | EmptyTerm -> EmptyTerm
    | Comments c -> Comments c
    | Stmt stmt -> Stmt stmt
    | Component cdcl -> Component (rcdcl cdcl)
    | Function fcdcl -> Function fcdcl
    | Typealias _ as t -> t
    | Typedef _ as t -> t
    | Derive derive -> Derive derive
    and rterm term = map_place rewrite_term term


    (* Hidden implicit vars = implicit vars coming from implicit vars of inner spawn *)
    and compute_hidden_implicit_vars () =
        let fplace = (Error.forge_place "Core.Rewrite.compute_hidden_implicit_vars" 0 0) in
        let auto_fplace smth = {place = fplace; value=smth} in
        (* Given data
            implicits (* based implicit without hidden *)
            inner_spawn (* list inner spawn with its scope*)
        *)

        let openkeys = ref (Atom.Set.of_seq (Hashtbl.to_seq_keys implicits)) in

        (*how to stop exploration detecter per component when fix point
        openkeuys= empty
        *)
        let update k =
            let spawns = try Hashtbl.find inner_spawns k with Not_found -> (logger#error "pass1.5: %s not found in inner_spawns" (Atom.to_string k); raise Not_found) in
            if spawns = [] then 
                openkeys := Atom.Set.remove k !openkeys (* NB. Env compare method only take atom into account, main type is ignored *)
            else
            List.iter (function (c, env) ->
                match fst c.value with 
                | VarCExpr c -> begin 
                    let implicit_args = try Hashtbl.find implicits c with Not_found -> (logger#error "pass 1.5: %s not found in implicits" (Atom.to_string c); raise Not_found) in     
                    (* Transform to an Env.t i.e. x -> (mt, x)
                        is not used since env is only used to get ride of variables
                    *)
                    let env = Env.of_seq (Seq.map (function x -> (auto_fplace EmptyMainType, x)) (Atom.Set.to_seq env)) in
                    (*Format.fprintf Format.std_formatter "Round for %s [spawn %s]\n" (Atom.to_string k) (Atom.to_string c);
                    print_env env;*)
                    let discovered_implicits = Env.diff implicit_args env in
                    let new_implicits = try Env.diff discovered_implicits (Hashtbl.find implicits k) with Not_found -> (logger#error "%s not found in implicits" (Atom.to_string k); raise Not_found)in
                    if new_implicits <> Env.empty then( 
                        Format.fprintf Format.std_formatter "%s find implicit %a \n" 
                            (Atom.to_string k) 
                            (Error.pp_list 
                                "@;" 
                                (fun out (_, x) -> Format.fprintf out "%s" (Atom.to_string x))
                            ) 
                            (List.of_seq (Env.to_seq new_implicits));
                        let tmp = Hashtbl.find implicits k in
                        Hashtbl.add implicits k (Env.union tmp new_implicits); 

                        (* Keep key open*)
                    )else (
                        openkeys := Atom.Set.remove k !openkeys; (* NB. Env compare method only take atom into account, main type is ignored *)
                    )
                end
                | _ -> raise (Error.DeadbranchError "Component expr should have been statically reduce to a value (i.e. a VarCExpr)") (* TODO FIXME VarCExpr is not the accurate name because is not a variable, it is a kind of literal*)
            ) spawns

        in
        while (!openkeys <> Atom.Set.empty) do 
           let k = Atom.Set.min_elt !openkeys in
           update k
        done;

    (* Second pass of rewriting using information gathered during the first pass *)
    and rewrite_component_item2 place : _component_item -> _component_item = function
    | Term t -> Term (rterm2 t)
    | _ as item -> item
    and rcitem2 citem : component_item = map_place rewrite_component_item2 citem

    and rewrite_component_dcl2 place : _component_dcl -> _component_dcl = 
        let fplace = (Error.forge_place "Core.Rewrite" 0 0) in
        let auto_place smth = {place = place; value=smth} in
        let auto_fplace smth = {place = fplace; value=smth} in
    function
    | ComponentAssign _ as cdcl -> cdcl
    | ComponentStructure cdcl -> 
        (* Rewrite spawn with implicit has been done during the intermediate pass *)

        (* Processing sub-components first *)
        let body = List.map rcitem2 cdcl.body in


        (* Make implicit constructor variables explicit *)
        compute_hidden_implicit_vars ();
        let implicit_vars = try Hashtbl.find implicits cdcl.name with Not_found -> (logger#error "pass2: %s not found in implicits" (Atom.to_string cdcl.name); raise Not_found)in 
        let implicit_vars = List.of_seq (Env.to_seq implicit_vars) in
        (*let _, implicit_vars = free_vars_component_dcl (List.fold_left (fun set {value=mt,x} -> Atom.Set.add x set ) Atom.Set.empty cdcl.args)  {place; value=ComponentStructure cdcl} in (* Variable coming from outside the component, that have not been reduced to value during partial evaluation*)*)

        (* Must be done in pass2 since we need to detect the implicit variables introduced by calling spawn *)
        if implicit_vars <> [] && Target.is_guardian targets cdcl.name then
            Error.error place "Guardian component can not have implicit variables - since there is no shared memory between instances of guardians\n@[<hov>%a@]" (Error.pp_list "\n" (fun out (mt, x) -> Format.fprintf out "- %s" (Atom.to_string x))) implicit_vars;

        (* [(mt, implicit_x, explicit_x)]*)
        let implicit_vars = List.map (function (mt, x) -> (mt, x, (Atom.fresh ("explicit_"^(Atom.hint x))) )) implicit_vars in

        Printf.fprintf stdout  "Implicit vars for %s\n" (Atom.to_string cdcl.name);
        print_string "> implict -> explicit\n";
        List.iter (function (mt, x, y) -> 
           Printf.fprintf stdout "> %s -> %s\n" (Atom.to_string x) (Atom.to_string y)
        ) implicit_vars;
        print_string "\n\n";

        (* Add fields to store implicit_vars *) 
        let body = 
            (List.map (
                function (mt, _, x) -> 
                auto_fplace (State (auto_fplace (StateDcl {
                    ghost = false;
                    type0 = mt;
                    name = x;
                    body = None;
                })))
            ) implicit_vars)
            @ body
        in

        (* Add implicit fields hydratation onstartup *)
        let has_constructor = List.exists (function |{value=Method m} -> m.value.on_startup | _ -> false) body in
        let rec update_constructor = function
        | [] -> []
        | {place; value=Method m}::citems when m.value.on_startup -> 
            {
                place;
                value=Method {m with value = {m.value with 
                    args = (List.map (function (mt, _, x) -> auto_fplace (mt,x)) implicit_vars) @ m.value.args;
                    body = (List.map (function (mt, x, y) -> auto_fplace(AssignThisExpr (y, auto_fplace (VarExpr x, mt))) ) implicit_vars) @ m.value.body
                }}
            }::(update_constructor citems) 
        | citem::citems ->  citem::(update_constructor citems) in
        let make_constructor () = 
            auto_fplace(Method (auto_fplace {
                annotations = [];
                ghost = false;
                ret_type = auto_fplace (CType (auto_fplace (TFlatType TVoid)));
                name = Atom.fresh "implicit2explicit_constructor";
                args = List.map (function (mt, _, x) -> auto_fplace (mt,x)) implicit_vars;
                body = List.map (function (mt, x, y) -> auto_fplace(AssignThisExpr (y, auto_fplace (VarExpr x, mt))) ) implicit_vars;
                contract_opt = None;
                on_destroy = false;
                on_startup = true;
            }))
        in
        let body = 
            if has_constructor then update_constructor body 
            else (make_constructor ())::body
        in
            
        (* Use explicit instead of explicit *)
        let replace_implict_var (mt, x, y) stmts = List.map (rewrite_expr_component_item 
            (function | (VarExpr z) | (ImplicitVarExpr z) when z = x -> true | _ -> false)
            (function _ -> VarExpr y)
        ) stmts in 
        let body = List.fold_left (fun body (mt, x, y) -> replace_implict_var (mt, x, y) body) body implicit_vars in
        ComponentStructure {cdcl with body = body }
    and rcdcl2 cdcl = map_place rewrite_component_dcl2 cdcl 

    and rewrite_term2 place = function
    | EmptyTerm -> EmptyTerm
    | Comments c -> Comments c
    | Stmt stmt -> Stmt stmt
    | Component cdcl -> Component (rcdcl2 cdcl)
    | Function fcdcl -> Function fcdcl
    | Typealias _ as t -> t
    | Typedef _ as t -> t
    | Derive _ as t -> t 
    and rterm2 term : term = map_place rewrite_term2 term

    (*Two passes of rewriting since some computation needs to gather information in all scope first
        - implict 2 explict needs to find all spawn in all scope
    *)
    and rewrite_program terms = 
        spawn_rewritings := []; (* TODO handle the state using a Make module *)
        let terms = List.map rterm terms in
        let apply_rewriting term (select, rewriter) = rewrite_expr_term select rewriter term in 
        let apply_all_rewriting term = List.fold_left apply_rewriting term !spawn_rewritings in
        let terms = List.map apply_all_rewriting terms in
        List.map rterm2 terms

    (*****************************************************)
    let displayed_pass_shortdescription = "Implicit have been removed and turned to explicit"
    let displayed_ast_name = "explicit IR"

    let show_ast = true

    let precondition program = program 
    let postcondition program = program 

    let apply_program = rewrite_program

end

(*
Issue:

int  i = 1;

component A () {
    j = i +1;
}

A(); //Ok i can be loaded

component Orchestrator () { // can not have implicit since it is a guardian container
    int i = 1;
    A () {} //i can not be loaded because atom id is not the same .....
}

choix 1: 
    - les implicits ne peuvent pas être capturé par d'autres binders (entre definition et instanciation) => A should be rewritten as A(i)
    Actuellement implementé
    - les implicicts peuvent être capturés par d'autres binders => form OK + load from scope
*)