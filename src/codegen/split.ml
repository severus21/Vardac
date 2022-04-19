open Core
open AstUtils
open IRI
open Easy_logging
open Utils

let logger = Logging.make_logger "_1_ compspec" Debug [];;

let targets2ast : (string, IRI.program) Hashtbl.t = Hashtbl.create 10 
let targets2targets : (string, Target.target) Hashtbl.t = Hashtbl.create 10

(** State hydrated during this pass and exported to be used in topology.ml
TODO export it not just with module inclusion but using Functor
*)
let make_component2target () = 
    assert( Hashtbl.length targets2ast <> 0);

    let component2target : (Atom.atom, string) Hashtbl.t = Hashtbl.create 64 in

    let rec _explore_citem target place = function 
    | Term t -> explore_term target t
    | _ -> ()
    and explore_citem target = map0_place (_explore_citem target)
    and _explore_term target place = function 
    | Component {value = ComponentStructure cdcl} -> 
        Hashtbl.add component2target cdcl.name target; 
        List.iter (explore_citem target) cdcl.body
    | Component {value=ComponentAssign cdcl} -> 
        Hashtbl.add component2target cdcl.name target 
    | _ -> ()
    and explore_term target = map0_place (_explore_term target)
    in

    Hashtbl.iter (fun target -> List.iter (explore_term target)) targets2ast;

    component2target

let add_to_target target term = 
    try 
        let ast = Hashtbl.find targets2ast target in 
        Hashtbl.replace targets2ast target (term::ast)
    with | Not_found -> Error.error term.place "target %s is undefined" target 
let rec split_toplvl_term (term:IRI.term) : unit =
(* 
    - non component term are added to all targets
    - component description terms are to their target only
    - TODO remove unneeded term in targets or undefined ref of component
*)
match term.value with
| Component {value=ComponentStructure cstruct;_} -> add_to_target cstruct.target_name term
| _ -> 
    Seq.iter 
    (function target -> add_to_target target term) (Hashtbl.to_seq_keys targets2ast)

let split_program targets (terms: IRI.program) : (Target.target * IRI.program) Seq.t = 
    (* Initialize targets2aast with empty AST
    and targets2targets *)
    List.map (function (target:Target.target) -> Hashtbl.add targets2ast target.value.name []) targets;
    List.map (function (target:Target.target) -> Hashtbl.add targets2targets target.value.name target) targets;

    (* Split top-level terms per target*)
    List.iter split_toplvl_term terms;
    (* Correct terms order *)
    Seq.map (function (target, ast) -> 
        Hashtbl.replace targets2ast target (List.rev ast)
    ) (Hashtbl.to_seq targets2ast);

    (* Building the output list *)
    Seq.map (function (key, program) -> 
        try
            (Hashtbl.find targets2targets key, program)
        with | Not_found -> raise (Error.DeadbranchError "key in targets2ast and targets2targets must be the same")
    ) (Hashtbl.to_seq targets2ast)