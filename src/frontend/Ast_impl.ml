open Core
open AstUtils

(************************************* Base types ****************************)
type variable = string list

and blackbox_term = Impl.blackbox_term

and method_impl = {
        name: variable; 
        body: blackbox_term option; (* None => juste adding plg_annotations to the method *)
    }
and state_impl = {
        name: variable;
        body: blackbox_term;
    }
and function_impl = {
        name: variable;
        body: blackbox_term;
    }
and _component_item_impl = 
    | MethodImpl of method_impl 
    | StateImpl of state_impl 
    | ComponentHeadersImpl of blackbox_term
and component_item_impl = (_component_item_impl plg_annotated) placed

and component_impl = { target: string option; name: variable; body: component_item_impl list}
and type_impl = {name: variable; body: blackbox_term}
and _term =
    | ComponentImpl of component_impl
    | CurrentDefaultTarget of string 
    | TypeImpl of type_impl
    | FunctionImpl of function_impl
    | HeadersImpl of blackbox_term
    | DependenciesImpl of blackbox_term

and term = (_term plg_annotated) placed

and program = term list
[@@deriving show { with_path = false }]

let program_to_yojson _ = failwith "not implemented"
let program_of_yojson _ = failwith "not implemented"