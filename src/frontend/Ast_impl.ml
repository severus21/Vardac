open Core
open AstUtils

include Impl_common

(************************************* Base types ****************************)
type variable = string list

and method_impl = {
        ret_type: Ast.main_type; 
        name: variable; 
        args: Ast.param list; 
        body: blackbox_term;
    }
and state_impl = {
        type0: Ast.main_type;
        name: variable;
        body: blackbox_term;
    }
and _component_item_impl = 
    | MethodImpl of method_impl 
    | StateImpl of state_impl 
and component_item_impl = _component_item_impl placed

and component_impl = { name: variable; body: component_item_impl list}
and type_impl = {name: variable; body: blackbox_term}
and _term =
    | ComponentImpl of component_impl
    | TypedefImpl of type_impl

and term = _term placed

and program = term list
[@@deriving show { with_path = false }]