open Core 
open Utils
open Core.Error
open Core.Builtin
open Easy_logging
open Fieldslib
open AstUtils

let logger = Logging.make_logger "_1_ compspec.frontend" Debug [];;

let fplace = (Error.forge_place "Frontend.Cook" 0 0) 
let auto_fplace smth = {place = fplace; value=smth}

(* The source calculus. *)
module S = Ast 
(* The target calculus. *)
module T = IR 

let places = Hashtbl.create 16

let fst3 (x,y,z) = x


(* Global state 
    Since each new binder create an unique variable, a variable is exactly binded once with a type other with its identity changes
*)

type gamma_t = (IR.expr_variable, IR.main_type) Hashtbl.t
let gamma = Hashtbl.create 64 

(* Environments map strings to atoms. *)
module Env = Map.Make(String)

(*
    A{
        method b
        C {
            method titi
        }
    }

    B {
        D {
            method d
        }
    }
    => 
    A -> A1
         b -> b2
         C -> C3
            titi -> titi4
    B -> B5
         D -> D6
            b -> b7
*)
type iota_entry = {
    name: Atom.atom AstUtils.placed; (* A -> A1*)
    inner: Atom.atom AstUtils.placed Env.t; (*b -> b2*)
    rec_inner: iota_entry AstUtils.placed Env.t; (* C -> C3
                                                        titi -> titi3
                                                *)
}

let fresh_iota_entry name = {
    name;
    inner = Env.empty;
    rec_inner = Env.empty;
}
(* iota denotes the structure of the program should be expose for subsequent passes *)
let iota = ref (fresh_iota_entry (auto_fplace (Atom.builtin "__default__")))


let register_gamma x t = Hashtbl.add gamma x t 
let register_gamma_builtin x = 
    Hashtbl.add gamma x (Builtin.type_of (Atom.hint x))

let print_gamma gamma = 
    Format.fprintf Format.std_formatter "Gamma {@[%a]}" (Error.pp_list "\n  -" (fun out (x,t) -> Format.fprintf out " %s -> %s " (Atom.to_string x) (T.show_main_type t))) (List.of_seq (Hashtbl.to_seq gamma))


(*********** Entity level environment - each entity create its own ************)
type entity_env = {
    components:     IR.component_variable Env.t; 
    exprs:          IR.expr_variable  Env.t; 
    instanciations: bool  Env.t; 
    this:           iota_entry; 
    implicits:      IR.component_variable Env.t;
    types:          IR.type_variable  Env.t} [@@deriving fields] 

let fresh_entity_env iota = {
    components              = Env.empty;
    exprs                   = Env.empty;
    instanciations          = Env.empty;
    this                    = iota;
    implicits               = Env.empty;
    types                   = Env.empty}

(*debug only*)
let print_entity_env env =
    let print_keys env = Env.iter (fun x _ -> Printf.printf "%s;" x) env in

    print_newline ();
    print_string "Env = {";

    List.iter (
        function (name, l) -> print_string ("\t"^name^"\n\t\t"); print_keys (l env);print_newline (); 
    ) [
        ("components", components);
        ("exprs", exprs); 
        ("implicits", implicits);
        ("types", types) ];
    
    print_string "}";
    print_newline ()

(* [bind env x] creates a fresh atom [a] and extends the environment [env]
   with a mapping of [x] to [a]. *)


(*********** Component level environment - each component create its own  ************)
(* Any entities (except subcomponent) share the "parent" component environment *)
type component_env = {
    name: Atom.atom; (* component name *)
    eventdef_from_labels: IR.typedef Env.t; 
} [@@deriving fields] 

let print_component_env env =
    let print_keys env = Env.iter (fun x _ -> Printf.printf "%s;" x) env in

    print_newline ();
    print_string ((Atom.value env.name)^" = {");

    List.iter (
        function (name, l) -> print_string ("\t"^name^"\n\t\t"); print_keys (l env);print_newline (); 
    ) [
        ("eventdef_from_labels", eventdef_from_labels);
    ];
    
    print_string "}";
    print_newline ()
let fresh_component_env () = {
    name                    = Atom.builtin "__default__";
    eventdef_from_labels    = Env.empty;
}

(* TODO FIXME can we do monadic computation *)
let combine (env1:component_env) (env2: component_env) = 
    if env1 = env2 then env1
    else begin
        Printf.printf "try combine %s %s\n" (Atom.to_string env1.name) (Atom.to_string env2.name);
        assert( env1.name = env2.name);
        
        let rec populate (key:string) (value:IR.typedef) env = 
            match Env.find_opt key env with
            | Some {AstUtils.place; _} -> Error.error (place@value.place) "labels %s defined multiple times" key
            | None -> Env.add key value env
        in 

        {
            name = env1.name;
            eventdef_from_labels = Env.fold populate env1.eventdef_from_labels env2.eventdef_from_labels;
        }
    end

let _terms_of_eventdef_from_labels env =
    List.map (function (edef:IR.typedef) -> {AstUtils.place=edef.place; value=T.Typedef edef})(List.map snd (List.of_seq (Env.to_seq env.eventdef_from_labels))) 
let _citems_of_eventdef_from_labels env =
    List.map (function (t:IR.term) -> {AstUtils.place=t.place; value=T.Term t}) (_terms_of_eventdef_from_labels env) 

(************************** Environement wrapper  ****************************)
let print_components_env (env:(IR.component_variable Env.t) Atom.VMap.t) =
    Format.fprintf Format.std_formatter "Components env = {@[<hv>%a@]}\n" (Error.pp_list "\n -" (fun out (x,_env) -> 
        Format.fprintf out " %s -> {@[%a@]}" (Atom.to_string x) (Error.pp_list ";" (fun out (x,_) -> Format.fprintf out "%s" x)) (List.of_seq (Env.to_seq _env))    
    )) (List.of_seq (Atom.VMap.to_seq env))

type env = {
    current: entity_env;
    component: component_env; (* NB: top-level is considered as a mock component *)
}

let print_env env = 
    print_component_env env.component;
    print_entity_env env.current

let (<<) (env0:env) (envs:env list) : env = {
    current = env0.current;
    component = List.fold_left combine env0.component (List.map (fun c -> c.component) envs)
}
let fresh_env iota = {
    current         = fresh_entity_env iota;
    component       = fresh_component_env ();
}



(* take an env and refresh the component scope *)
let refresh_component_env (env:env) name = 
    {env with component = { (fresh_component_env ()) with  name}}

let terms_of_eventdef_from_labels env = _terms_of_eventdef_from_labels env.component

let citems_of_eventdef_from_labels env = _citems_of_eventdef_from_labels env.component
(*****************************************************************************)
let bind_component env place name =
    if is_builtin_component name then
        error place "Component Keyword %s is reserved." name;

    try
        let inner = (Env.find name env.current.this.rec_inner).value in
        let a_name = inner.name.value in
        {env with 
            current = {
                env.current with 
                    components=Env.add name a_name env.current.components;
                instanciations= Env.add name true env.current.instanciations;
            };
            component = { (fresh_component_env ()) with  name = a_name}
        }, a_name 
    with Not_found -> Error.error place "Unbounded component"

let hydrate_compoent_env_from_iota env entry = List.fold_left 
    (fun env name -> fst (bind_component env fplace name)) 
    env 
    (List.map 
        (function (k, _) -> k) 
        (List.of_seq (Env.to_seq entry.rec_inner))
    ) 

let bind_expr env place x =
    if is_builtin_expr x then
        error place "Keyword %s is reserved." x;

    let a = Atom.fresh x in
    { env with current = {env.current with 
        exprs=Env.add x a env.current.exprs;
    }}, a

let register_expr env place ?create_instance:(create_instance=false) atom =
  if is_builtin_expr (Atom.hint atom) then
    error place "Keyword %s is reserved." (Atom.hint atom);

    {   env with current = {
            env.current with 
                exprs=Env.add (Atom.hint atom) atom env.current.exprs;
                instanciations= Env.add (Atom.hint atom) create_instance env.current.instanciations;
        }
    }

let bind_this env place x =
    try
        env, (Env.find x env.current.this.inner).value
    with Not_found -> raise (DeadbranchError "bind_this not in iota")

let bind_type env place x =
   if is_builtin_type x then
      error place "Type keyword %s is reserved." x;

  let a = Atom.fresh x in
  { env with current = {env.current with types=Env.add x a env.current.types}}, a

let bind_eventdef (env:env) place (label:Atom.atom) : env =
    let key = Atom.hint label in

    (* Sanity check *)
    begin
    match  Env.find_opt key env.component.eventdef_from_labels with 
    | None -> ()
    | Some {AstUtils.place=inner_place; _} ->
        Error.error (place@inner_place) "label %s is used multiple times" key
    end;


    let place = place @ Error.forge_place "cook/bind_evendef" 0 0 in
    let edef = { 
        AstUtils.place; 
        value = T.EventDef (label, [], ())
    } in
    { env with component = {env.component with eventdef_from_labels=Env.add key edef env.component.eventdef_from_labels}}


module StringMap = Map.Make(String)


(*****************************************************************************)
(** Pass 1 : cartography the structure
        i.e hydrate iota
*)

let rec _print_iota out (entry:iota_entry) = 
    Format.fprintf out "{name = %s;inner = {@;@[<v 3>%a@]@;<0 -3>}; rec_inner = {@;@[<v 3>%a@]@;<0 -3>}}" 
        (Atom.to_string entry.name.value) 
        (Error.pp_list "\n" (fun out (x,_) -> Format.fprintf out "%s;" x)) (List.of_seq (Env.to_seq entry.inner))
        (Error.pp_list "\n" _print_iota) (List.map (function x -> (snd x).value) (List.of_seq (Env.to_seq entry.rec_inner)))
let print_iota entry = _print_iota Format.std_formatter entry

let register_component place name = 
    if is_builtin_component name then
        error place "Component Keyword %s is reserved." name;

    Atom.fresh name
let register_this place x =
    Atom.fresh x

let rec cartography_component_item (entry:iota_entry) ({place; value}: S.component_item) : iota_entry = 
    (*
        FIXME WARNING
        method and state/port should have distinct name because only one env is used for all
    *)
    match value with
    | S.Term t -> cartography_term entry t
    | S.Method {value={name}} | S.Port {value={name;}} | S.State {value=StateDcl{name;}} | S.State {value=StateAlias{name;}}  -> begin
        match Env.find_opt name entry.inner with 
        | None -> { entry with
            inner = Env.add name {place; value=register_this place name} entry.inner
        } 
        | Some p -> Error.error (p.place@place) "multiple definitions %s in the same component" name
    end
and cartography_component_dcl ({place; value}: S.component_dcl) : iota_entry = 
(* We do not explore the body of a component *)
match value with
| S.ComponentStructure cdcl -> 
    let inner_entry = fresh_iota_entry ({place; value=register_component place cdcl.name}) in
    (* Remove contract since they shared the name of their method *)
    let citems = List.filter (function {value=S.Contract _} -> false | _ -> true) cdcl.body in
    List.fold_left cartography_component_item inner_entry citems 
| S.ComponentAssign cdcl -> fresh_iota_entry ({place; value=register_component place cdcl.name})  
and cartography_term (entry:iota_entry) ({place; value}: S.term) : iota_entry = 
match value with
| S.Component c -> begin 
    let inner_entry = cartography_component_dcl c in
    match Env.find_opt (Atom.value inner_entry.name.value) entry.rec_inner with
    | None -> { entry with
        rec_inner = Env.add (Atom.value inner_entry.name.value) {place; value=inner_entry} entry.rec_inner
    } 
    | Some p -> Error.error (p.place@place) "multiple definitions of component %s in the same scope" (Atom.value inner_entry.name.value)
end
| _ -> entry




(*****************************************************************************)
(*
        @return flag; flag = true if it is a component like (instanciation needed)
*)
let rec is_instance_var env place x : bool = 
    if is_builtin_expr x then
        false (* FIXME builtin flag_create_instance should be extracted from a DB *)
    else (
        try
            Env.find x env.current.instanciations
        with Not_found -> false 
    )
and is_instance_expr env (e:S.expr) : bool = 
match e.value with
| S.VarExpr x -> is_instance_var env e.place x
| _ -> false (* TODO *)

let rec _cook_var_expr (env:entity_env) place x : Atom.atom = 
    if is_builtin_expr x then
        Atom.builtin x (* FIXME builtin flag_create_instance should be extracted from a DB *)
    else (
        try
            Env.find x env.exprs
        with Not_found -> begin
            try 
                Env.find x env.implicits
            with Not_found -> 
                error place "Unbound variable: %s" x
        end
    )
and cook_var_expr (env:env) = _cook_var_expr env.current
and cook_var_type env place x : Atom.atom = 
    if Str.string_match (Str.regexp "^[A-Z].*") x 0 then
        cook_var_component env place x (* hack, FIXME add a specific type for container ???*)
    else begin
        if is_builtin_type x then
            Atom.builtin x
        else (
        try
            Env.find x env.current.types
        with Not_found ->
            error place "Unbound type variable: %s" x
        )
    end
and cook_var_component env place x = 
    if is_builtin_component x then
        Atom.builtin x
    else (
    try
        Env.find x env.current.components
    with Not_found ->
        error place "Unbound component variable: %s" x
    )
and cook_var_derive env place x = 
    if is_builtin_derivation x then
        Atom.builtin x
    else 
        error place "Unbound derivation: %s" x
and cook_var_this env place x = 
    try
        (Env.find x env.current.this.inner).value
    with Not_found ->
        error place "Unbound this variable: %s" x
(************************************ Types **********************************)
and cook_composed_type (env:env) place: S._composed_type -> env * T._composed_type = function
| (S.TActivationInfo mt as ct) | (S.TArray mt as ct) | (S.TList mt as ct) | (S.TOption mt as ct) | (S.TSet mt as ct) -> 
    let (env1:env), mt = cmtype env mt in
    (env << [env1]), (match ct with 
        | S.TArray _ -> T.TArray mt 
        | S.TActivationInfo _ -> T.TActivationInfo mt 
        | S.TList _ -> T.TList mt 
        | S.TOption _ -> T.TOption mt 
        | S.TSet _ -> T.TSet mt 
    )
| (S.TArrow (mt1, mt2) as ct) | (S.TDict (mt1, mt2) as ct) | (S.TResult (mt1, mt2) as ct) | (S.TUnion (mt1, mt2) as ct)-> 
    let env1, mt1 = cmtype env mt1 in
    let env2, mt2 = cmtype env mt2 in
    env << [env1; env2], (match ct with 
        | S.TArrow _ -> T.TArrow (mt1, mt2) 
        | S.TDict _ -> T.TDict (mt1, mt2) 
        | S.TResult _ -> T.TResult (mt1, mt2)
        | S.TUnion _ -> T.TUnion (mt1, mt2)
    )
| S.TBridge {in_type; out_type; protocol } -> 
    let env1, in_type = cmtype env in_type in
    let env2, out_type = cmtype env out_type in
    let env3, protocol = cmtype env protocol in
    env << [env1; env2; env3], T.TBridge { in_type; out_type; protocol }
| S.TFlatType ft -> env, T.TFlatType ft 
| S.TVar x -> 
    let y = cook_var_type env place x in
    env, T.TVar y
| S.TTuple mts -> 
    let envs, mts = List.split (List.map (cmtype env) mts) in
    env << envs, T.TTuple mts 
| S.TVPlace mt -> 
    let env1, mt = cmtype env mt in
    env << [env1], T.TVPlace mt
and cctype env ct: env * T.composed_type = map2_place (cook_composed_type env) ct

and cook_session_type env place: S._session_type -> env * T._session_type = function
| (S.STBranch entries as st0) | (S.STSelect entries as st0) ->
    let aux env (x, st, aconst_opt) = 
        let x = "label_"^x in
        let env1, y = bind_type env place x in
        (* Register an event def for this label *)
        let env2 : env = bind_eventdef env1 place y in

        let env3, st = cstype env2 st in
        match aconst_opt with
        | None -> env3, (y, st, None)
        | Some aconst -> 
            let env4, aconst = caconst env3 aconst in
            env4, (y, st, Some aconst) 
    in
    let new_env, entries = List.fold_left_map aux env entries in
    new_env, (match st0 with
        | S.STBranch _ -> T.STBranch entries            
        | S.STSelect _ -> T.STSelect entries            
    )
| S.STEnd  -> env, T.STEnd
| (S.STRecv (mt, st) as st0) | (S.STSend (mt, st) as st0)  -> 
    let env1, mt = cmtype env mt in
    let env2, st = cstype env st in 
    env << [env1; env2], (match st0 with 
        | S.STRecv _ -> T.STRecv (mt, st) 
        | S.STSend _ -> T.STSend (mt, st)
    )
| S.STVar x  -> begin
    let y = cook_var_type env place x in
    env, T.STVar y
end
| S.STRec (x, st) -> begin
    let _env, y = bind_type env place x in  
    let env1, st  = cstype _env st in
    env << [env1], T.STRec (y, st) (* NB: env1 is included in env2 *)
end
| S.STInline x ->  
    let y = cook_var_type env place x in  
    env, T.STInline y
and cstype env st: env * T.session_type = map2_place (cook_session_type env) st

and cook_component_type env place: S._component_type -> env * T._component_type = function
| S.CompTUid x -> 
    env, T.CompTUid (cook_var_type env place x)
and ccomptype env cmt : T.component_type = snd(map2_place (cook_component_type env) cmt)
and cook_expression = function (e:S.expr) -> snd (cexpr (fresh_env !iota) e)

and cook_mtype env place: S._main_type -> env * T._main_type = function
| S.CType ct -> 
    let env1, ct = cctype env ct in
    env << [env1], T.CType ct 
| S.SType st -> 
    (* Specific semantics of timer in constraint header
        first [timer x] bind x and set it to 0
        subsquent timer x reset x 
    *)
    let timers = S.timers_of_st st in 
    let inner_env = List.fold_left (fun env  name -> fst (bind_expr env place name)) env timers in

    let env1, st = cstype inner_env st in
    env << [env1], T.SType st
| S.CompType cmt -> 
    let cmt = ccomptype env cmt in
    env, T.CompType cmt 
| S.ConstrainedType (mt, aconst) ->
    let env1, mt = cmtype env mt in
    let env2, aconst = caconst env aconst in
    env << [env1; env2], T.ConstrainedType (mt, aconst)
and cmtype env mt : env * T.main_type = map2_place (cook_mtype env) mt

(******************************** Constraints ********************************)
and cook_constraint_header env place: S._constraint_header -> env * T._constraint_header = function
| S.UseMetadata (mt, x) ->
    let env1, mt = cmtype env mt in
    let new_env, y = bind_type env place x in
    new_env << [env1], T.UseMetadata (mt, y)
| S.SetTimer x ->
    (* alreay added when defining a session type *)
    env, T.SetTimer (cook_var_expr env place x)

and cconst env const : env * T.constraints = map2_place (cook_expr env) const

and caconst env (headers,const_opt): env * T.applied_constraint = 
    let (env1, new_headers) : (env * T.constraint_header list ) = List.fold_left_map (function env -> map2_place (cook_constraint_header env)) env headers in
    match const_opt with
    | Some const ->
        let env2, new_const = cconst env1 const in
        env << [env1; env2], (new_headers, Some new_const)
    | None -> env << [env1], (new_headers, None)

(************************************* Literals ******************************)
and cook_literal env place : S._literal -> env * T._literal = function
| S.BoolLit b -> env, T.BoolLit b
| S.FloatLit f -> env, T.FloatLit f 
| S.IntLit i -> env, T.IntLit i
| S.LabelLit l -> env, T.LabelLit l 
| S.StringLit s -> env, T.StringLit s
| S.VoidLit -> env, T.VoidLit

(** Activations *)
| S.ActivationInfo _ -> env, T.ActivationInfo () (* TODO *)
and cliteral env lit: env * T.literal = map2_place (cook_literal env) lit

(*
 bool parameter - create_instance_flag
*)
and cook_expr env place e : env * (T._expr * T.main_type) = 
    let fplace = (Error.forge_place "Frontend.Cook.cook_expr" 0 0) in
    let auto_place smth = {place = place; value=smth} in
    let auto_fplace smth = {place = fplace; value=smth} in
    let env, e =
        match e with 
        (* No binding done in an expression can be propagated outside this expression *)
        | S.VarExpr x -> (env, T.VarExpr (cook_var_expr env place x))
        | S.ImplicitVarExpr x -> (env, T.ImplicitVarExpr (cook_var_expr env place x))

        | S.AccessExpr ({place=p_t; value=S.This}, {place=p_v; value=S.VarExpr v}) -> env, T.AccessExpr (
            {place=p_t; value=T.This, auto_fplace T.EmptyMainType},
            {place=p_v; value= T.VarExpr (cook_var_this env p_v v), auto_fplace T.EmptyMainType}) 
        | S.AccessExpr ({place=_; value=S.This}, _) -> error place "Illformed [this] usage: should be this.<state_name/method_name>"
        (* Method call / or attribute access *)
        | S.AccessExpr (({value=VarExpr a} as e1), ({value=VarExpr x} as e2)) -> begin
            failwith "TODO COOOK"
            (*try begin
                let a = cook_var_expr env place a in
                let env1, e = cexpr env e1 in
                let mt_e1 = Hashtbl.find gamma a in
                match mt_e1.value with 
                | T.CType {value=T.TActivationInfo {value=T.CType {value=T.TVar cname }}} -> begin
                    try 
                        let target_env = Atom.VMap.find cname env.components_env in
                        let mock_entity_env = { (fresh_entity_env ()) with exprs = target_env } in (* FIXME add implicit *)
                        env << [env1], T.ActivationAccessExpr(
                            cname,
                            e, 
                            _cook_var_expr mock_entity_env place x
                        )
                    with Not_found -> error place "Unbound variable: %s in %s" x (Atom.to_string cname)
                end
                | r -> 
                    let env1, e1 = cexpr env e1 in
                    let env2, e2 = cexpr env e2 in
                    env << [env1; env2], T.AccessExpr (e1, e2)
            end with | Not_found -> error place "Variable %s not in  gamma" a
            *)
        end
        | S.AccessExpr (e1, e2) -> 
            let env1, e1 = cexpr env e1 in
            let env2, e2 = cexpr env e2 in
            env << [env1; env2], T.AccessExpr (e1, e2)
        | S.BinopExpr (e1, op, e2) ->
            let env1, e1 = cexpr env e1 in
            let env2, e2 = cexpr env e2 in
            
            env << [env1; env2], T.BinopExpr (e1, op, e2) 
        | S.LambdaExpr (x, mt, e) -> 
            let inner_env, y = bind_expr env place x in 
            let env2, e = cexpr inner_env e in 
            let env3, mt = cmtype env mt in

            register_gamma y mt;

            env << [env2; env3], T.LambdaExpr (y, mt, e)
        | S.LitExpr l -> 
            let env1, l = cliteral env l in
            env << [env1], T.LitExpr l
        | S.UnopExpr (op, e) -> 
            let env1, e = cexpr env e in
            env << [env1], T.UnopExpr (op, e) 
        | S.CallExpr (e1, es) when is_instance_expr env e1 -> 
            List.iter (function e -> if is_instance_expr env e then error place "constructor can not be aliased";) es;

            let env1, e1 = cexpr env e1 in
            let envs, es = List.split (List.map (cexpr env) es) in

            env << (env1::envs), T.NewExpr (e1, es)
        | S.PolyApp (e, mts) -> 
            let env1, e = cexpr env e in
            let envs, mts = List.split (List.map (cmtype env) mts) in

            env << (env1::envs), T.PolyApp (e, mts)
        | S.CallExpr (e1, es) -> 
            List.iter (function e -> if is_instance_expr env e then error place "constructor can not be aliased";) es;

            let env1, e1 = cexpr env e1 in
            let envs, es = List.split (List.map (cexpr env) es) in

            env << (env1::envs), T.CallExpr (e1, es)
        | S.This -> env, T.This
        | S.Spawn spawn -> begin 
            List.iter (function e -> if is_instance_expr env e then error place "constructor can not be aliased";) spawn.args;

            let env1, c = ccexpr env spawn.c in
            let env_args, args = List.split (List.map (cexpr env) spawn.args) in
            match spawn.at with 
                | None -> env << (env1::env_args), T.Spawn {c; args; at = None} 
                | Some at -> 
                    let env_at, at = cexpr env at in
                    env << (env1::env_at::env_args), T.Spawn {c; args; at = Some at}
        end
        | S.BoxCExpr ce -> 
            let env1, ce = ccexpr env ce in
            env << [env1], T.BoxCExpr ce 
        | S.OptionExpr None -> env, T.OptionExpr None 
        | S.OptionExpr (Some e) -> 
            let env1, e = cexpr env e in
            env << [env1], T.OptionExpr (Some e) 
        | S.ResultExpr (e1_opt, e2_opt) -> begin 
            match e1_opt, e2_opt with
            | Some e, None ->
                let env1, e = cexpr env e in 
                env << [env1], T.ResultExpr (Some e, None)
            | None, Some e ->
                let env1, e = cexpr env e in 
                env << [env1], T.ResultExpr (None, Some e)
            | _, _ -> raise (Error.PlacedDeadbranchError (place,"a result must be an error or an ok"))
        end
        | S.BlockExpr (b, es) -> 
            let envs, es = List.split (List.map (cexpr env) es) in
            env << envs, T.BlockExpr (b, es)
        | S.Block2Expr (b, es) -> 
            let es1, es2 = List.split es in
            let envs1, es1 = List.split (List.map (cexpr env) es1) in
            let envs2, es2 = List.split (List.map (cexpr env) es2) in
            let es = List.combine es1 es2 in
            env << (envs1@envs2), T.Block2Expr (b, es)
    in
    env, (e, auto_fplace T.EmptyMainType)
and cexpr env e: env * T.expr = map2_place (cook_expr env) e

and cook_stmt env place: S._stmt -> env * T._stmt = function
| S.EmptyStmt -> env, T.EmptyStmt
| S.AssignExpr (x, e) ->  
    let y = cook_var_expr env place x in
    let env1, e = cexpr env e in 
    env << [env1], T.AssignExpr (y, e)
| S.AssignThisExpr (x, e) ->
    let y = cook_var_this env place x in
    let env1, e = cexpr env e in 
    env << [env1], T.AssignThisExpr (y, e) 
| S.LetExpr (mt, x, e) ->
    if is_instance_expr env e then error place "constructor can not be aliased";

    let env1, mt = cmtype env mt in
    let new_env, y = bind_expr env place x in
    let env2, e = cexpr new_env e in

    register_gamma y mt;

    new_env << [env1; env2], T.LetExpr (mt, y, e)
| S.CommentsStmt c -> env, T.CommentsStmt c
| S.BreakStmt -> env, T.BreakStmt
| S.ContinueStmt -> env, T.ContinueStmt
| S.ExitStmt i -> env, T.ExitStmt i
| S.ForStmt (mt, x, e, stmt) ->
    if is_instance_expr env e then error place "constructor can not be aliased";

    (* [new env] applies to [stmt] only and [stmt_env] does not applies outside the for*)
    let env0, mt = cmtype env mt in
    let env1, e = cexpr env e in
    let inner_env, y = bind_expr env place x in
    let env2, stmt = cstmt inner_env stmt in

    register_gamma y mt;

    env << [env; env0; env1; inner_env; env2], T.ForStmt (mt, y, e, stmt)
| S.IfStmt (e, stmt1, stmt2_opt) -> begin
    let env1, e = cexpr env e in
    let env2, stmt1 = cstmt env stmt1 in
    match stmt2_opt with
    | None -> env << [env1; env2], T.IfStmt (e, stmt1, None)
    | Some stmt2 ->
        let env3, stmt2 = cstmt env stmt2 in
        env << [env1; env2; env3], T.IfStmt (e, stmt1, Some stmt2)
end
| S.MatchStmt (e, entries) ->
    let env1, e = cexpr env e in
    let tmp1, tmp2 = List.split (List.map (function (x,y) -> (cexpr env x, cstmt env y)) entries) in
    let envs1, exprs = List.split tmp1 in
    let envs2, stmts = List.split tmp2 in
    let entries = List.combine exprs stmts in

    env << (envs1@envs2), T.MatchStmt (e, entries)
| (S.ReturnStmt e as stmt0) | (S.ExpressionStmt e as stmt0) -> 
    let env1, e = cexpr env e in
    env << [env1], (match stmt0 with
        | S.ReturnStmt _ -> T.ReturnStmt e
        | S.ExpressionStmt _ -> T.ExpressionStmt e
    )
| S.BlockStmt stmts -> 
    let env1, stmts = List.fold_left_map cstmt env stmts in
    env << [env1], T.BlockStmt stmts
| S.GhostStmt stmt -> 
    let env1, stmt = cstmt env stmt in
    env << [env1], T.GhostStmt stmt
and cstmt env : S.stmt -> env * T.stmt = map2_place (cook_stmt env)

and cook_function env place : S._function_dcl -> env * T._function_dcl = 
let fplace = (Error.forge_place "Coook.cook_function" 0 0) in
let auto_place smth = {AstUtils.place = place; value=smth} in
let auto_fplace smth = {AstUtils.place = fplace; value=smth} in
function
| f -> 
    let new_env, name = bind_expr env place f.name in
    let inner_env, args = List.fold_left_map cparam env f.args in


    (* FIXME duplicated in cook_method*)
    let rec remove_empty_stmt = function
        | [] -> []
        | {AstUtils.place=_;value=S.EmptyStmt}::stmts -> remove_empty_stmt stmts
        | stmt::stmts -> stmt::(remove_empty_stmt stmts)
    in

    let env1, ret_type = cmtype env f.ret_type in
    let env2, body = List.fold_left_map cstmt inner_env (remove_empty_stmt f.abstract_impl) in

    let fct_sign = List.fold_right (fun t1 t2 -> auto_fplace (T.CType (auto_fplace(T.TArrow (t1, t2))))) (List.map (function (arg: T.param) -> fst arg.value) args) ret_type in 
    register_gamma name fct_sign;

    new_env << [inner_env; env1; env2], {
            ret_type = ret_type;
            name;
            args;
            body = body 
    } 
and cfdcl env: S.function_dcl -> env * T.function_dcl = map2_place (cook_function env)

(************************************ Component *****************************)
and cook_state env place : S._state -> env * T._state = function
| S.StateDcl sdcl ->
    let env1, type0 = cmtype env sdcl.type0 in 
    let new_env, y = bind_this env place sdcl.name in
    let ret_env, body = match sdcl.init_opt with
        | None -> new_env << [env1], None
        | Some e ->
            let _env, e = cexpr env e in
            new_env << [env1; _env], Some e
    in
    
    ret_env, T.StateDcl {
                ghost   = sdcl.ghost; 
                type0   = type0;
                name    = y;
                body = body}
| S.StateAlias _ -> failwith "cook: state alias not yet supported" (*TODO*)
and cstate env: S.state -> env * T.state = map2_place (cook_state env)


and cook_param env place (mt, x) : env * T._param = 
    let env1, mt = cmtype env mt in
    let new_env, y = bind_expr env place x in

    register_gamma y mt;
    
    new_env<<[env1], (mt, y)
and cparam env: S.param -> env * T.param = map2_place (cook_param env)

and cook_contract env place (contract:S._contract): env * T._contract =
    let fplace = (Error.forge_place "Frontend.Cook.cook_contract" 0 0) in
    let auto_place smth = {place = place; value=smth} in
    let auto_fplace smth = {place = fplace; value=smth} in

    let method_name = cook_var_this env place contract.method_name in
    let aux_binder env (mt, x, e) =
        let new_env, y = bind_expr env place x in
        let env1, mt = cmtype env mt in
        let env2, e = cexpr env e in 

        register_gamma y mt;

        new_env << [env1; env2], (mt, y, e)
    in
    let inner_env, pre_binders = List.fold_left_map aux_binder env contract.pre_binders in
   
    (* Goal: invariant should be added to ensures and to returns *)
    let env1, invariant = match contract.invariant with
        | None -> fresh_env !iota, None 
        | Some invariant -> 
            let env1, invariant = cexpr inner_env invariant in
            env1, Some invariant
    in
    let env2, ensures = match contract.ensures with
        | None -> fresh_env !iota, None 
        | Some ensures -> 
            let env2, ensures = cexpr inner_env ensures in
            env2, Some ensures
    in
    let env3, returns = match contract.returns with
        | None -> fresh_env !iota, None 
        | Some returns -> 
            let env3, returns = cexpr inner_env returns in
            env3, Some returns
    in

    let concat_opt (predicat_opt1:T.expr option) predicat_opt2 =
        match predicat_opt1, predicat_opt2 with 
        | None, None -> None
        | None, Some _ -> predicat_opt2
        | Some _, None -> predicat_opt1
        | Some p1, Some p2 -> Some {
            place = p1.place @ p2.place;
            value = T.BinopExpr (p1, And, p2), auto_fplace T.EmptyMainType 
        }
    in
    let ensures = concat_opt invariant ensures in
    let returns = concat_opt invariant returns in
    
    env << [env1; env2; env3], { method_name; 
            pre_binders; 
            ensures;
            returns}

and ccontract env: S.contract -> env * T.contract = map2_place (cook_contract env)


and cook_method0 env place (m: S._method0) : env * T._method0 = 
let fplace = (Error.forge_place "Coook.cook_method0" 0 0) in
let auto_place smth = {AstUtils.place = place; value=smth} in
let auto_fplace smth = {AstUtils.place = fplace; value=smth} in
    (* method name has been already binded when scanning the structure of the component *)
    let name = cook_var_this env place m.name in 
    let inner_env, args = List.fold_left_map cparam env m.args in

    let rec remove_empty_stmt = function
        | [] -> []
        | {AstUtils.place=_;value=S.EmptyStmt}::stmts -> remove_empty_stmt stmts
        | stmt::stmts -> stmt::(remove_empty_stmt stmts)
    in

    let env1, ret_type = cmtype env m.ret_type in
    let env2, body = List.fold_left_map cstmt inner_env (remove_empty_stmt m.abstract_impl) in

    let fct_sign = List.fold_right (fun t1 t2 -> auto_fplace (T.CType (auto_fplace(T.TArrow (t1, t2))))) (List.map (function (arg: T.param) -> fst arg.value) args) ret_type in 
    register_gamma name fct_sign;

    env << [inner_env; env1; env2], {
        ghost = m.ghost;
        ret_type = ret_type;
        name;
        args;
        contract_opt = None; (* Pairing between contract and method0 is done during a next pass, see. Core.PartialEval.ml *)
        body = body;
        on_destroy = m.on_destroy;
        on_startup = m.on_startup 
    } 
and cmethod0 env: S.method0 -> env * T.method0 = map2_place (cook_method0 env)

and cook_port env place (port:S._port) : env * (T._port * T.main_type)=
    let fplace = (Error.forge_place "Coook.cook_method0" 0 0) in
    let auto_fplace smth = {AstUtils.place = fplace; value=smth} in

    let new_env, name = bind_this env place port.name in
    let env1, input = cexpr env port.input in
    let env2, expecting_st = cmtype env port.expecting_st in 
    let env3, callback = cexpr env port.callback in 
    let env4, input_type = cmtype env port.input_type in
    new_env << [env1; env2; env3; env4], ({ name; input; expecting_st; callback}, input_type)
and cport env: S.port -> env * T.port = map2_place (cook_port env)

and cook_component_item env _ : S._component_item -> env * T._component_item list = function
| S.State s ->
    let new_env, new_s = cstate env s in
    new_env, [T.State new_s]
| S.Method m ->
    let new_env, new_m = cmethod0 env m in
    new_env, [T.Method new_m]
| S.Contract c -> 
    env, [T.Contract (snd( ccontract env c))]
| S.Port p ->
    let new_env, new_p = cport env p in
    new_env, [T.Port new_p]
| S.Term t ->
    let new_env, new_ts = cterm env t in
    new_env, List.map (fun t -> T.Term t) new_ts
| S.Include ce -> 
    let env1, ce = ccexpr env ce in
    env << [env1], [T.Include (ce)]
and ccitem env: S.component_item -> env * T.component_item list = map2_places (cook_component_item env)

and cook_component_dcl env place : S._component_dcl -> env * T._component_dcl = function
| S.ComponentStructure cdcl -> 
    (*
        Hyp: Exactly one component delcaration per component name at a given layer of the architure (checked befor cooking -> cf. iota cartography).
        Which means that A { B{ } } B {}, A.B and B will be binded to different unique atom.
        Stmt: decoupling of cook and cartograhpy => allows architecture loop i.e. A use B ref and B use A ref, has already attributed an Atom to the current componen t component.
    *)
    let name = cook_var_component env place cdcl.name in
    let new_env = 
        {env with 
        current = {env.current with 
            components  = Env.add cdcl.name name env.current.components;
        };
        component = { (fresh_component_env ()) with  name = name;
        }
    } in

    let this_entry = match Env.find_opt cdcl.name env.current.this.rec_inner with
    | Some e -> e.value
    | None -> raise (Error.PlacedDeadbranchError (place, Printf.sprintf "Iota entry not found for %s" cdcl.name))
    in

    let env = {env with 
        current = {env.current with 
            implicits   = Env.fold (fun k v map -> Env.add k v map) (Env.map (fun v -> v.value) env.current.this.inner) env.current.implicits ; (* add binders of parent components in implicit map, iterate over env.currentcomponents and successively add it into env.current.implicits (NB args order differ with List.fold_left) - in order to mask variable if with inner scope if needed *)
            this = this_entry 
        };
        component = new_env.component } in

    (* Check that there is at most one constructor/destructor per component *)
    let constructors = List.filter (function |{AstUtils.value=S.Method m} when m.value.on_startup -> true | _-> false) cdcl.body in 
    if List.length constructors > 1 then
        Error.error (List.flatten (List.map (function (item:S.component_item) -> item.place) constructors)) "multiple onstartup in component %s" cdcl.name;

    let destructors = List.filter (function |{AstUtils.value=S.Method m} when m.value.on_destroy -> true | _-> false) cdcl.body in 
    if List.length destructors > 1 then
        Error.error (List.flatten (List.map (function (item:S.component_item) -> item.place) destructors)) "multiple ondestroy in component %s" cdcl.name;

    let inner_env, args = List.fold_left_map cparam env cdcl.args in

    (* Prepare env for mutual binding between components and methods/states *)
    let inner_env = hydrate_compoent_env_from_iota inner_env this_entry in 

    let inner_env, body = List.fold_left_map ccitem inner_env cdcl.body in
    let body = List.flatten body in

    (* Add collected label events to body *)
    let collect_labelevents = citems_of_eventdef_from_labels inner_env in 
    let body = collect_labelevents @ body in

    new_env, T.ComponentStructure {target_name = (); name; args; body} 
| S.ComponentAssign cdcl -> 
    let new_env, name = bind_component env place cdcl.name in
    let env = {env with component = new_env.component } in

    let inner_env, args = List.fold_left_map cparam env cdcl.args in
    let inner_env, value = ccexpr inner_env cdcl.value in
    (* TODO what should i do with the inner_env + should i refresh the env ?? *)
    new_env, T.ComponentAssign {name; args; value }
and ccdcl env: S.component_dcl -> env * T.component_dcl = map2_place (cook_component_dcl env)

(********************** Manipulating component structure *********************)
and cook_component_expr env place ce : env * (T._component_expr * T.main_type)=
    let fplace = (Error.forge_place "Frontend.Cook.cook_expr" 0 0) in
    let auto_fplace smth = {place = fplace; value=smth} in

(* FIXME TODO component scope environment is not processed here  (yet) *)
    let env, ce = match ce with 
        | S.VarCExpr x ->
            let y = cook_var_component env place x in
            env, T.VarCExpr y
        | S.AppCExpr (ce1,ce2) -> env, T.AppCExpr (
            snd (ccexpr env ce1),
            snd (ccexpr env ce2)
        )
        | S.UnboxCExpr e -> 
            let cenv1, e = cexpr env e in
            env << [cenv1], T.UnboxCExpr e
        | S.AnyExpr e -> 
            let cenv1, e = cexpr env e in
            env << [cenv1], T.AnyExpr e
    in env, (ce, auto_fplace T.EmptyMainType)
and ccexpr env : S.component_expr -> env * T.component_expr = map2_place (cook_component_expr env)

(********************** Signatures *********************)

(************************************ Program *****************************)
and cook_term env place : S._term -> env * T._term list = 
let fplace = (Error.forge_place "Coook.cook_term" 0 0) in
    let auto_place smth = {AstUtils.place = place; value=smth} in
    let auto_fplace smth = {AstUtils.place = fplace; value=smth} in
function
| S.Comments c -> env, [T.Comments c]
| S.PPTerm _ -> raise (PlacedDeadbranchError (place, "No preprocessing term should remains when cooking the AST."))

| S.Stmt stmt ->
    let new_env, new_stmt = cstmt env stmt in
    new_env, [T.Stmt new_stmt]
| S.Function f -> 
    let new_env, new_f = cfdcl env f in
    new_env, [T.Function new_f]
| S.Component c ->
    let new_env, new_c = ccdcl env c in
    (* Restaure component env *)
    {new_env with component = env.component}, [T.Component new_c]

| S.Typealias (x, mt_opt) -> begin
    let new_env, y = bind_type env place x in
    (* No type constructor for alias *)
    match mt_opt with 
    | None -> new_env, [T.Typealias (y, None)]
    | Some mt -> 
        let cenv1, mt = cmtype env mt in
        new_env << [cenv1], [T.Typealias (y, Some mt)]
end
| Typedef {value= ProtocolDef (x, mt) as tdef; place} -> 
    let new_env1, y = bind_type env place x in
    (* We use resgister_expr y here in order to preserve atom identity between both the world of types and the world of values (type constructor for instance) *)
    let new_env2 = register_expr new_env1 place ~create_instance:false y in 
    let env3, mt = cmtype env mt in 

    (* p : () -> p *)
    let constructor_type = auto_fplace(T.CType(auto_fplace(T.TArrow(
        auto_fplace(T.CType(auto_fplace(T.TFlatType AstUtils.TVoid))),
        auto_fplace(T.CType(auto_fplace(T.TVar y)))
    )))) in
    register_gamma y constructor_type;

    new_env2 << [env3], [T.Typedef ({ place; value = 
    ProtocolDef (y, mt) })]

| Typedef {value= VPlaceDef (x, name) as tdef; place} -> begin 
    (* create a type x -> to be used as vplace<x> ...
    create a literal x 
    *)
    let new_env1, y = bind_type env place x in
    let new_env2 = register_expr new_env1 place ~create_instance:false y in
    
    let mt =  auto_place(T.CType(auto_place(T.TVPlace(auto_place(T.CType(auto_place(T.TVar y))))))) in 
    
    try
        new_env2, [
            T.Typedef {place; value=T.ClassicalDef (y, [], ())};
            T.Stmt(auto_place(T.LetExpr(
                    mt,  
                    y,
                    auto_place(T.LitExpr(auto_place(T.VPlace (Hashtbl.find places name))), mt) 
                ))
            )
        ]
    with Not_found -> Error.error place "vplace does not exists in configuration file"
end
| Typedef {value= ClassicalDef (x, args) as tdef; place} | Typedef {value= EventDef (x, args) as tdef; place} -> 
    let new_env1, y = bind_type env place x in
    (* Type constructor for typedef *)
    (* We use resgister_expr y here in order to preserve atom identity between both the world of types and the world of values (type constructor for instance) *)
    let new_env2 = register_expr new_env1 place ~create_instance:true y in
    let envs, args = List.split (List.map (cmtype env) args) in 

    (* t : args1 -> ... argsn -> t *)
    let constructor_type = List.fold_right (fun t1 t2 -> auto_fplace (T.CType (auto_fplace(T.TArrow (t1, t2))))) args (auto_fplace(T.CType(auto_fplace(T.TVar y)))) in 
    register_gamma y constructor_type;

    new_env2 << envs, [T.Typedef ({ place; value = 
    match tdef with 
        | ClassicalDef _ -> ClassicalDef (y, args, ())
        | EventDef _ -> EventDef (y, args, ())
    })]
| S.Derive derive ->
    let cenvs, cargs = List.split (List.map (ccexpr env) derive.cargs) in
    let tenvs, targs = List.split (List.map (cmtype env) derive.targs) in
    let eenvs, eargs = List.split (List.map (cexpr env) derive.eargs) in

    env << (cenvs@tenvs@eenvs),[ T.Derive {
        name = cook_var_derive env place derive.name;
        cargs;
        targs;
        eargs;
    }]
    
and cterm env: S.term -> env * T.term list = map2_places (cook_term env)

let cook_program _places terms =    
    let rec hydrate_places parent_name (p:IR.vplace) = 
        Hashtbl.add places (parent_name^(Atom.hint p.name)) p;
        let parent_name = match parent_name with
            | "" -> Atom.hint p.name
            | _ -> parent_name^"::"^(Atom.hint p.name)
        in
        List.iter (hydrate_places parent_name) p.children
    in
    List.iter (hydrate_places "") _places;

    iota := List.fold_left cartography_term !iota terms;


    let toplevel_env = {(fresh_env !iota) with component = {(fresh_component_env ()) with name = Atom.builtin "toplevel"}} in
    let toplevel_env = hydrate_compoent_env_from_iota toplevel_env !iota in 

    let toplevel_env = refresh_component_env toplevel_env (Atom.builtin "toplevel") in

    let toplevel_env,  program = (List.fold_left_map cterm toplevel_env terms) in 
    let program = List.flatten program in
    gamma, (terms_of_eventdef_from_labels toplevel_env) @ program