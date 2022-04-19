open AstUtils
open IR_common

module IRCParams : (AstUtils.IRParams with type Variable.t = Atom.t)= struct
    module Variable = Atom
end 

type ir_target_name = 
    | UserDefined (* i.e. defined in *.impl *)
    | SameAs of Atom.atom (* 
    B.target_name = SameAs A means that when A load from *.impl B will be completed as well 
    SameAs can not be chained -> PairImpl issue a "Target should have been assign to .."
    *)
and ir_state_dcl_body = expr option 
and ir_custom_method0_body = stmt list 
and ir_typealias_body = main_type option
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
    module Variable = Atom
    type target_name = ir_target_name 
    and _state_dcl_body = ir_state_dcl_body
    and _custom_method0_body = ir_custom_method0_body
    and _typealias_body = ir_typealias_body
    and _typedef_body = ir_typedef_body
    [@@deriving show { with_path = false }]

    let collect_type_state_dcl_body 
        parent_opt already_binded selector collector 
    = function
        | Some e -> collect_type_expr parent_opt already_binded selector collector e
        | None _ -> already_binded, [], []
    let rewrite_type_state_dcl_body 
        selector rewriter
    = Option.map (rewrite_type_expr selector rewriter)
    let rewrite_expr_state_dcl_body 
        selector rewriter
    = 
        (* TODO FIXME rewrite type0*)
        Option.map (rewrite_expr_expr selector rewriter)

    let collect_expr_state_dcl_body 
        parent_opt already_binded selector collector 
    = function
    | Some e -> collect_expr_expr parent_opt already_binded selector collector e
    | None _ -> already_binded, [], []

    let collect_cexpr_state_dcl_body 
        parent_opt already_binded selector collector   
    = function
        | Some e -> already_binded, IR_common.collect_cexpr_expr parent_opt already_binded selector collector e, []
        | None _ -> already_binded, [], []

    let collect_cexpr_custom_method0_body 
        parent_opt already_binded selector collector 
    = function body ->  already_binded, List.flatten (List.map (collect_cexpr_stmt parent_opt already_binded selector collector) body), []

    let collect_type_custom_method0_body 
        parent_opt already_binded selector collector 
    = 
        List.fold_left_map (fun set stmt ->         
            let env, a,b  = collect_type_stmt parent_opt set selector collector stmt in
            env, (a,b)
        ) already_binded
    let rewrite_type_custom_method0_body 
        selector rewriter
    = List.map (rewrite_type_stmt selector rewriter)
    let rewrite_expr_custom_method0_body 
        selector rewriter
    = List.map (rewrite_expr_stmt selector rewriter)
    let rewrite_exprstmts_custom_method0_body rewrite_exprstmts_stmt parent_opt exclude_stmt selector rewriter = function body ->
    List.flatten (List.map (rewrite_exprstmts_stmt parent_opt exclude_stmt selector rewriter) body)
    let rewrite_stmt_custom_method0_body
    rewrite_stmt_stmt recurse selector rewriter = function body ->
    List.flatten (List.map (rewrite_stmt_stmt recurse selector rewriter) body)

    let collect_expr_custom_method0_body 
        parent_opt already_binded selector collector
    = 
        List.fold_left_map (fun already_binded stmt ->         
            let env, a,b  = collect_expr_stmt parent_opt already_binded selector collector stmt in
            env, (a,b)
        ) already_binded

    let collect_stmt_custom_method0_body 
        parent_opt selector collector 
    = function body ->  
                List.flatten (List.map (collect_stmt_stmt parent_opt  selector collector) body)

    let rename_state_dcl_body rename_expr renaming = Option.map (rename_expr renaming)
    let rename_custom_method0_body rename_stmt renaming = List.map (rename_stmt renaming) 
    let rename_typealias_body rename_main_type renaming = 
    Option.map (rename_main_type renaming) 

end

include IR_common
module IR = IR_template.Make(Params) 
include IR
include IR.IRUtils

