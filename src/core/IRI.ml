(* IR extended with blackbox implementation for type, methods and states *)

type iri_state_dcl_body = 
| InitExpr of IR_common.expr
| InitBB of Impl_common.blackbox_term

and iri_custom_method0_body = 
| AbstractImpl of IR_common.stmt
| BBImpl of Impl_common.blackbox_term

and iri_typedef_body = 
| AbstractTypedef of IR_common.main_type
| BBTypedef of Impl_common.blackbox_term 
[@@deriving show { with_path = false }]

module Params : (
    IR_template.IRParams with   type _state_dcl_body = iri_state_dcl_body and 
                                type _custom_method0_body = iri_custom_method0_body and 
                                type _typedef_body = iri_typedef_body
) = struct
    type _state_dcl_body = iri_state_dcl_body 
    and _custom_method0_body = iri_custom_method0_body 
    and _typedef_body = iri_typedef_body
    [@@deriving show { with_path = false }]
end

include IR_common
module IRI = IR_template.Make(Params) 
include IRI