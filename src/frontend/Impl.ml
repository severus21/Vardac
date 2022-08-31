open Core
open AstUtils

(************************************* Base types ****************************)
type variable = string list
[@@deriving show { with_path = false }, yojson]


type blackbox_body = 
| Text of string
| Varda of Ast.expr 

and _blackbox_term = {
    language: string option;
    body: blackbox_body list;
} 

and blackbox_term = _blackbox_term placed

(* TODO need to pair vairble in ret_type/... with their name in architeure*)
and method_impl = {
        (*ret_type: Ast.main_type;*)
        name: variable; 
        (*args: Ast.param list;*)
        body: blackbox_term option;
    }
and function_impl = {
        name: variable;
        body: blackbox_term;
    }
and state_impl = {
        name: variable;
        body: blackbox_term;
    }
and _component_item_impl = 
    | MethodImpl of method_impl 
    | StateImpl of state_impl 
    | ComponentHeadersImpl of blackbox_term
    | CItemRawImpl of blackbox_term
and component_item_impl = (_component_item_impl plg_annotated) placed

and component_impl = { target: string; name: variable; body: component_item_impl list}
and type_impl = {name: variable; body: blackbox_term}

and term_impl = {target: string; body: blackbox_term} 
and _term =
    | ComponentImpl of component_impl
    | CurrentDefaultTarget of string 
    | FunctionImpl of function_impl
    | TypeImpl of type_impl
    | HeadersImpl of term_impl 
    | DependenciesImpl of term_impl

and term = (_term plg_annotated) placed

and program = term list
[@@deriving show { with_path = false }]

let program_to_yojson _ = failwith "not implemented"
let program_of_yojson _ = failwith "not implemented"