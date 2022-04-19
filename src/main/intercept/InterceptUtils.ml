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
let failure_collector_e2 msg parent_opt place ce = failure_collector msg parent_opt place 

let failure_collector_e3 msg parent_opt env place ce = failure_collector msg parent_opt place 

let get_schema program wanted_name= 
    let [place, schema] : (Error.place * component_structure) list = 
        collect_term_program 
            false
            (function | Component {value=ComponentStructure {name}} -> name = wanted_name | _ -> false) 
            (fun _ place -> function | Component {value=ComponentStructure cstruct} -> [place, cstruct]) 
            program 
    in

   place, schema

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
    b_onboard_mt: main_type
}
type sessions_info = {
    this_4external2internal: Atom.atom;
    this_4internal2external: Atom.atom;
    this_4external: Atom.atom;
    this_4internal: Atom.atom
} 

type interceptor_info = {
    from_ctx_elim: bool; (* Source: ctx API if true else low-level API*)

    (*** Hydrated by ctx API if from_ctx_elim  ***)
    name: Atom.atom;  
    base_interceptor_name: Atom.atom;
    base_interceptor_place: Error.place;
    
    (* Generated *)
    onboard_info: onboard_info; (* (st_onboard, p_onboard, b_onboard_mt) *)
    inout_bridges_info: (Atom.atom * Atom.atom * main_type) list; (* (b_out_1, b_in_1, mt) ... (b_out_n ... b_in_n) *)

    (* Computed *)
    intercepted_schemas: Atom.Set.t;

    (*** Not hydrated by ctx_elim ***)
    (*** Hydrated by intercept elim***)
    this_onboarded_activations: Atom.atom option;
    b_onboard_state: Atom.atom option; (* state holding the onboarding bridge *)
    inout_statebridges_info: ((Atom.atom * Atom.atom * main_type) list) option; (* schema state that holds the bridges *)
    sessions_info: sessions_info option
}

let st_onboard_of intercepted_schemas = 
    let intercepted_schemas = (Atom.Set.to_list intercepted_schemas) in

    let onboard_st_branch_of schema = 
        (* ('A': !tuple<activation_ref<A>, place>> ?bool.) *)
        schema, auto_fplace (STSend(
            mtype_of_ct (TTuple [
                mtype_of_ct (TActivationRef (mtype_of_cvar schema));
                mtype_of_ft (TPlace)
            ]),
            auto_fplace (STRecv (mtype_of_ft TBool, auto_fplace STEnd))
        )), None
    in
    auto_fplace (STBranch ( List.map onboard_st_branch_of intercepted_schemas))

let b_onboarf_mt_of interceptor_name intercepted_schemas st_onboard = 
    mtype_of_ct (TBridge {
        in_type = mt_internals_of fplace intercepted_schemas;
        out_type = mtype_of_cvar interceptor_name;
        protocol = st_onboard;
    })