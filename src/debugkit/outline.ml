open Core 
open Utils
open Error
open Builtin
open Easy_logging
open Fieldslib
open AstUtils

open IR

module S = IR
module T = IR


let logger = Logging.make_logger "vardad.Outline" Debug [];;


(* Environments map strings to atoms. *)
module Env = Atom.VMap

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
let iota_entry_toplevel = ref (fresh_iota_entry (auto_fplace (Atom.builtin "__default__")))

(* component atom -> iota_entry*)
let iota = Hashtbl.create 64

let rec hydrate_iota entry = 
    Env.iter (fun _ entry -> hydrate_iota entry.value) entry.rec_inner;

    if "__default__" <> Atom.value entry.name.value then
        Hashtbl.add iota entry.name.value entry

let order_by_name : (Env.key * 'a) list -> (Env.key * 'a) list =
    List.sort (fun (x,_) (y,_) -> String.compare (Atom.to_string x) (Atom.to_string y))

let rec _print_iota out (entry:iota_entry) = 
    Format.fprintf out "{name = %s;inner = {@;@[<v 3>%a@]@;<0 -3>}; rec_inner = {@;@[<v 3>%a@]@;<0 -3>}}" 
        (Atom.to_string entry.name.value) 
        (Error.pp_list "\n" (fun out (x,_) -> Format.fprintf out "%s;" (Atom.to_string x))) (order_by_name (Env.to_list entry.inner))
        (Error.pp_list "\n" _print_iota) (List.map (function x -> (snd x).value) (List.of_seq (Env.to_seq entry.rec_inner)))
let print_iota entry = _print_iota Format.std_formatter entry


    (** Pass 1 : cartography the structure -> equivalent to first pass on cook but not on Frontend ASt but on IR/IRI
            i.e hydrate iota
    *)

let rec _cartography_component_item (entry:iota_entry) place _ = function
    (*
        FIXME WARNING
        method and state/port should have distinct name because only one env is used for all
    *)
    | S.Term t -> cartography_term entry t
    | S.Method {value={on_startup; on_destroy}} when on_startup || on_destroy -> entry 
    | S.Method {value={name}} | S.Inport {value={name;},_} | S.Eport {value={name;},_} | S.Outport {value={name;},_} | S.State {value={name;}} -> begin
        match Env.find_opt name entry.inner with 
        | None -> { entry with
            inner = Env.add name {place; value=name} entry.inner
        } 
        | Some p -> 
            let name = Atom.fresh ((Atom.to_string name)^"_duplicated") in
            logger#warning "multiple definitions %s in the same component" (Atom.to_string name);
            { entry with inner = Env.add name {place; value=name} entry.inner }  
    end
    | S.Contract _ -> raise (PlacedDeadbranchError (place, "Contracts should have been paired with methods before!!"))
    | S.Include _ -> raise (PlacedDeadbranchError (place, "Include should have been resolvec and eliminated before calling the Cook pass (see. Resolve.ml)"))
and cartography_component_item entry = map0_place (map0_plgannot (_cartography_component_item entry))
and cartography_component_dcl ({place; value}: S.component_dcl) : iota_entry = 
(* We do not explore the body of a component *)
match value with
| S.ComponentStructure cdcl -> 
    let inner_entry = fresh_iota_entry ({place; value=cdcl.name}) in
    (* Remove contract since they shared the name of their method *)
    let citems = List.filter (function {value={v=S.Contract _}} -> false | _ -> true) cdcl.body in
    List.fold_left cartography_component_item inner_entry citems 
| S.ComponentAssign cdcl -> fresh_iota_entry ({place; value=cdcl.name})  
and _cartography_term (entry:iota_entry) place _ = function 
| S.Component c -> begin 
    let inner_entry = cartography_component_dcl c in
    match Env.find_opt inner_entry.name.value entry.rec_inner with
    | None -> { entry with
        rec_inner = Env.add inner_entry.name.value {place; value=inner_entry} entry.rec_inner
    } 
    | Some p -> Error.perror (p.place@place) "multiple definitions of component %s in the same scope" (Atom.to_string inner_entry.name.value)
end
| _ -> entry
and cartography_term entry = map0_place (map0_plgannot (_cartography_term entry))

let cartography_program program =
    iota_entry_toplevel := List.fold_left cartography_term !iota_entry_toplevel program;
    hydrate_iota !iota_entry_toplevel;
    Seq.iter print_iota (Hashtbl.to_seq_values iota);