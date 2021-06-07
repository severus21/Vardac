module Params : (
    IR_template.IRParams with   type _state_dcl_body = IR_common.expr option and 
                                type _custom_method0_body = IR_common.stmt option and
                                type _typedef_body = IR_common.main_type option
) = struct
    type _state_dcl_body = IR_common.expr option

    and _custom_method0_body = IR_common.stmt option
    and _typedef_body = IR_common.main_type option
    [@@deriving show { with_path = false }]
end

include IR_common
module IR = IR_template.Make(Params) 
include IR