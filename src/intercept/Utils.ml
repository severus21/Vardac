open Core
open AstUtils
open IR

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
            (function | {value=Method m} -> m.value.on_startup | _ -> false) base_interceptor.body
        )