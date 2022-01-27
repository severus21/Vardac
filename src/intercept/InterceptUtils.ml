open Core
open AstUtils
open IR
open Easy_logging

let logger = Logging.make_logger ("_1_ compspec.InterceptUtils") Debug [];;

let fplace = (Error.forge_place "InterceptUtils" 0 0) 
let auto_fplace smth = {place = fplace; value=smth}
include AstUtils2.Mtype.Make(struct let fplace = fplace end)



let failure_collector msg parent_opt place = 
    let parent = match parent_opt with | None -> "Toplevel" | Some p -> Atom.to_string p in
    Error.error place "%s. Parent = %s" msg parent
let failure_collector_e msg parent_opt env e = failure_collector msg parent_opt e.place 
let failure_collector_ce msg parent_opt place ce = failure_collector msg parent_opt place 

let get_schema program wanted_name= 
    let [schema] : component_structure list = 
        collect_term_program 
            (function | Component {value=ComponentStructure {name}} -> name = wanted_name | _ -> false) 
            (function place -> function | Component {value=ComponentStructure cstruct} -> [cstruct]) 
            program 
    in

   schema

let get_onstartup (schema : component_structure) : method0 option= 
    Option.map 
        (function {value=Method m} -> m) 
        (List.find_opt 
            (function | {value=Method m} -> m.value.on_startup | _ -> false) schema.body
        )

let mt_internals_of place intercepted_schemas = 
    assert(intercepted_schemas <> []);
    List.fold_left 
        (fun mt schema -> 
            mtype_of_ct (TUnion (mt, mtype_of_cvar schema))
        )
        (mtype_of_cvar (List.hd intercepted_schemas))
        (List.tl intercepted_schemas)
type onboard_info = {
    st_onboard: session_type; 
    p_onboard: Atom.atom; 
    b_onboard_mt: main_type
}

type interceptor_info = {
    from_ctx_elim: bool; (* Source: ctx API if true else low-level API*)

    (*** Hydrated by ctx API if from_ctx_elim  ***)
    name: Atom.atom;  
    base_interceptor_name: Atom.atom;
    
    (* Generated *)
    onboard_info: onboard_info; (* (st_onboard, p_onboard, b_onboard_mt) *)
    inout_bridges_info: (Atom.atom * Atom.atom * main_type) list; (* (b_out_1, b_in_1, mt) ... (b_out_n ... b_in_n) *)

    (* Computed *)
    intercepted_schemas: Atom.Set.t;

    (*** Not hydrated by ctx_elim ***)
    (*** Hydrated by intercept elim***)
    b_onboard_state: Atom.atom option; (* state holding the onboarding bridge *)
    inout_statebridges_info: ((Atom.atom * Atom.atom * main_type) list) option; (* schema state that holds the bridges *)
}