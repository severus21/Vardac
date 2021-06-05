open Core
open Core.AstUtils

(************************************* Base types ****************************)
type variable = string

and blackbox_body = string

and _blackbox_term = {
    language: variable;
    body: blackbox_body;
}
and blackbox_term = _blackbox_term placed

and _component_item_impl = 
    | MethodImpl of {
        ret_type: Ast.main_type; 
        name: variable; 
        args: Ast.param list; 
        body: blackbox_term;
    }
    | StateImpl of {
        type0: Ast.main_type;
        name: variable;
        body: blackbox_term;
    } 
and component_item_impl = _component_item_impl placed

and component_impl = { name: variable; body: component_item_impl list}
and typedef_impl = {name: variable; body: blackbox_term}
and _term =
    | ComponentImpl of component_impl
    | TypedefImpl of typedef_impl

and term = _term placed

and program = term list
[@@deriving show { with_path = false }]