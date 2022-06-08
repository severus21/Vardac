open Core
open Core.Error
open Core.AstUtils
open Easy_logging

let logger = Logging.make_logger ("_1_ compspec.frontend") Debug [];;
(* variable : string => Atom.atom + check env *)
(* The source calculus. *)
module S = RawTarget 
(* The target calculus. *)
module T = Target 

(* Environments map strings to atoms. *)
module Env = Map.Make(String)

type env = unit
let fresh_env () = ()

let rec find_atom_citem place : string list -> Core.IR.component_item list -> Atom.atom = function 
| [] -> raise (Error.DeadbranchError "empty clpath") 
| name::clpath -> begin function
    | [] -> Error.perror place "atom not found %s" name
    | {value=Method m;}::_  when Atom.hint m.value.name = name -> 
        assert(clpath = []);
        m.value.name
    | {value=Term t;}::citems -> begin  
        try
            find_atom_term place clpath [t]
        with Error.DeadbranchError _ -> find_atom_citem place (name::clpath) citems 
    end
    | _::ts -> find_atom_citem place (name::clpath) ts
end
and find_atom_term place : string list -> Core.IR.term list -> Atom.atom = function 
| [] -> raise (Error.DeadbranchError "empty clpath") 
| name::clpath -> begin function
    | [] -> Error.perror place "atom not found %s" name
    | {value=Function f;}::_ when Atom.hint (f.value.name) = name -> 
        assert(clpath = []);
        f.value.name
    | {value=Component {value=ComponentAssign c;}}::_  when Atom.hint (c.name) = name -> 
        assert(clpath = []);
        c.name
    | {value=Component {value=ComponentStructure c;}}::_  when Atom.hint (c.name) = name -> 
        if clpath = [] then c.name
        else find_atom_citem place clpath c.body
    | {value=Typedef {value=ClassicalDef (inner_name, _,_);};}::_ when Atom.hint inner_name = name ->   
        assert(clpath = []);
        inner_name 
    | {value=Typedef {value=EventDef (inner_name, _,_);};}::_ when Atom.hint inner_name = name ->   
        assert(clpath = []);
        inner_name
    | {value=Typedef {value=ProtocolDef (inner_name, _);};}::_ when Atom.hint inner_name = name ->   
        assert(clpath = []);
        inner_name 
    | _::ts -> find_atom_term place (name::clpath) ts
end
and find_atom place (clpath:string) ast = 
    let clpath = String.split_on_char ':' clpath in
    find_atom_term place clpath ast

let cook_maindef place ast env (mdef:S.maindef) : env * T.maindef= 
    env, {
        name = mdef.name;
        bootstrap = 
            if mdef.bootstrap = "laststage" then Atom.builtin "laststage"
            else find_atom place mdef.bootstrap ast;
        entrypoint = 
            if mdef.entrypoint = "no_main" then Atom.builtin "no_main"
            else find_atom place mdef.entrypoint ast;
        _not_create_main = false;
    }

let rec cook_target ast env (target:S.target) : env * T.target =
    env, {
        place = target.place; 
        value = {
            T.name = target.value.name; 
            codegen = {
                runtime_plg = target.value.codegen.runtime_plg;
                language_plg = target.value.codegen.language_plg;
                interface_plg = target.value.codegen.interface_plg;
                mains = List.map snd (List.map (cook_maindef target.place ast env) target.value.codegen.mains);
            };
            user_defined = target.value.user_defined;
        }
    } 
                
let cook_targets (ir:Core.IR.program) (targets : S.targets) : T.targets =
   snd (List.fold_left_map (cook_target ir) (fresh_env ()) targets)  
