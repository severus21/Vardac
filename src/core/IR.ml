type ir_target_name = unit 
and ir_state_dcl_body = IR_common.expr option 
and ir_custom_method0_body = IR_common.stmt list 
and ir_typealias_body = IR_common.main_type option
and ir_typedef_body = unit
[@@deriving show { with_path = false }]


module Params : (
    IR_template.IRParams with   
        type target_name = ir_target_name and
        type _state_dcl_body = ir_state_dcl_body and 
        type _custom_method0_body = ir_custom_method0_body and
        type _typealias_body = ir_typealias_body and
        type _typedef_body = ir_typedef_body
) = struct
    type target_name = ir_target_name 
    and _state_dcl_body = ir_state_dcl_body
    and _custom_method0_body = ir_custom_method0_body
    and _typealias_body = ir_typealias_body
    and _typedef_body = ir_typedef_body
    [@@deriving show { with_path = false }]
end

include IR_common
module IR = IR_template.Make(Params) 
include IR