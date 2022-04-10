open Core
open AstUtils

(************************************* Base types ****************************)
type variable = string list

and blackbox_term = Impl.blackbox_term

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

and component_impl = { target: string option; name: variable; body: component_item_impl list}
and type_impl = {name: variable; body: blackbox_term}
and _term =
    | ComponentImpl of component_impl
    | CurrentDefaultTarget of string 
    | TypeImpl of type_impl
    | HeadersImpl of blackbox_term

and term = _term placed

and program = term list
[@@deriving show { with_path = false }]