(* IR extended with blackbox implementation for type, methods and states *)
type iri_target_name = string

and iri_state_dcl_body = 
| InitExpr of IR_common.expr
| InitBB of Impl_common.blackbox_term

and iri_custom_method0_body = 
| AbstractImpl of IR_common.stmt list
| BBImpl of Impl_common.blackbox_term

and iri_custom_function_body = iri_custom_method0_body

and iri_typealias_body = 
| AbstractTypealias of IR_common.main_type
| BBTypealias of Impl_common.blackbox_term 
and iri_typedef_body = Impl_common.blackbox_term option 
[@@deriving show { with_path = false }]

module Params : (
    IR_template.IRParams with   
        type target_name = iri_target_name and
        type _state_dcl_body = iri_state_dcl_body and 
        type _custom_method0_body = iri_custom_method0_body and 
        type _custom_function_body = iri_custom_function_body and 
        type _typealias_body = iri_typealias_body and
        type _typedef_body = iri_typedef_body
) = struct
    type target_name = iri_target_name
    and _state_dcl_body = iri_state_dcl_body 
    and _custom_method0_body = iri_custom_method0_body 
    and _custom_function_body = iri_custom_function_body 
    and _typealias_body = iri_typealias_body
    and _typedef_body = iri_typedef_body
    [@@deriving show { with_path = false }]
end

include IR_common
module IRI = IR_template.Make(Params) 
include IRI

(* TODO integrate replace_ in IR_template/IR_common *)
let replace_place replace_value {AstUtils.place; value} = 
    {AstUtils.place; value=replace_value value}

(* Common *)
let rec _type_replace_ctype to_be_replaced by = function
| ct when ct = to_be_replaced -> by
| TArrow (mt1, mt2) -> TArrow (
    type_replace_main_type to_be_replaced by mt1,
    type_replace_main_type to_be_replaced by mt2
)
| TVar _ as ct -> ct
| TFlatType _ as ct -> ct
| TDict (mt1, mt2) -> TDict (
    type_replace_main_type to_be_replaced by mt1,
    type_replace_main_type to_be_replaced by mt2
)
| TList mt -> TList (
    type_replace_main_type to_be_replaced by mt
)
| TOption mt -> TOption (
    type_replace_main_type to_be_replaced by mt
)
| TResult (mt1, mt2) -> TResult (
    type_replace_main_type to_be_replaced by mt1,
    type_replace_main_type to_be_replaced by mt2
)
| TSet mt -> TSet (
    type_replace_main_type to_be_replaced by mt
)
| TTuple mts -> TTuple (List.map (function mt -> type_replace_main_type to_be_replaced by mt) mts )
| TBridge {in_type; out_type; protocol} -> 
    TBridge {
        in_type = type_replace_main_type to_be_replaced by in_type; 
        out_type = type_replace_main_type to_be_replaced by out_type; 
        protocol = type_replace_main_type to_be_replaced by protocol
    }
| TRaw _ as mt -> mt
and type_replace_ctype to_be_replaced by : composed_type -> composed_type = replace_place (_type_replace_ctype to_be_replaced by)

and _type_replace_stype to_be_replaced by = function
| STEnd -> STEnd
| STVar _ as st -> st
| STBranch entries -> 
    let entries = List.map (function (x, st, aconst_opt) -> 
        x,
        type_replace_stype  to_be_replaced by st,
        Option.map (type_replace_applied_constraint to_be_replaced by) aconst_opt
    ) entries in
    STBranch entries 
| STSelect entries -> 
    let entries = List.map (function (x, st, aconst_opt) -> 
        x,
        type_replace_stype  to_be_replaced by st,
        Option.map (type_replace_applied_constraint to_be_replaced by) aconst_opt
    ) entries in
    STSelect entries 
| STRecv (mt, st) -> STRecv (
    type_replace_main_type  to_be_replaced by mt,
    type_replace_stype  to_be_replaced by st
)
| STSend (mt, st) -> STSend (
    type_replace_main_type  to_be_replaced by mt,
    type_replace_stype  to_be_replaced by st
)
| STRec _ as st -> st
| STInline _ as st -> st
and type_replace_stype to_be_replaced by = replace_place (_type_replace_stype to_be_replaced by)

and _type_replace_main_type to_be_replaced by : _main_type -> _main_type = function
| CType ct -> CType (type_replace_ctype to_be_replaced by ct)
| SType st -> SType (type_replace_stype to_be_replaced by st)
| CompType cmpt -> CompType cmpt
| ConstrainedType (mt, aconst) -> ConstrainedType (
    type_replace_main_type to_be_replaced by mt,
    type_replace_applied_constraint to_be_replaced by aconst
)
| main_type -> main_type
and type_replace_main_type to_be_replaced by = replace_place (_type_replace_main_type to_be_replaced by)

and _type_replace_stmt to_be_replaced by = function
| LetExpr (mt, x, e) -> LetExpr (type_replace_main_type to_be_replaced by mt, x, e)
| stmt -> stmt
and type_replace_stmt to_be_replaced by = replace_place (_type_replace_stmt to_be_replaced by)


and _type_replace_constraint_header to_be_replaced by = function
| UseGlobal (mt, x) -> UseGlobal (type_replace_main_type to_be_replaced by mt, x)
| UseMetadata (mt, x) -> UseMetadata (type_replace_main_type to_be_replaced by mt, x) 
and type_replace_constraint_header to_be_replaced by = replace_place (_type_replace_constraint_header to_be_replaced by)

and type_replace_applied_constraint to_be_replaced by : applied_constraint -> applied_constraint = function
(headers, const) -> (
    (List.map (function header -> type_replace_constraint_header to_be_replaced by header) headers),
    const
)

and _type_replace_param to_be_replaced by = function
(mt, x) -> (type_replace_main_type to_be_replaced by mt, x)
and type_replace_param to_be_replaced by = replace_place (_type_replace_param to_be_replaced by)

(** IRI **)
and type_replace__state_dcl_body to_be_replaced by = function
sdcl -> sdcl

and type_replace__custom_method0_body to_be_replaced by = function
| AbstractImpl stmts -> AbstractImpl (List.map (type_replace_stmt to_be_replaced by) stmts)  
| BBImpl _ as mb -> mb 

(** Template **)
and _type_replace_state to_be_replaced by = function
| StateDcl sdcl -> StateDcl {sdcl with
    type0 = type_replace_main_type to_be_replaced by sdcl.type0;
    body = type_replace__state_dcl_body to_be_replaced by sdcl.body 
}
| StateAlias _ -> failwith "not support in IRI replace"
and type_replace_state to_be_replaced by = replace_place (_type_replace_state to_be_replaced by)

and _type_replace_method to_be_replaced by = function
| CustomMethod m ->  CustomMethod {m with
    ret_type = type_replace_main_type to_be_replaced by m.ret_type;
    args = List.map (type_replace_param to_be_replaced by) m.args;
    body = type_replace__custom_method0_body to_be_replaced by m.body;
    contract_opt = Option.map (type_replace_contract to_be_replaced by) m.contract_opt
}
| OnStartup m -> OnStartup (type_replace_method to_be_replaced by m)
| OnDestroy m -> OnDestroy (type_replace_method to_be_replaced by m)

and type_replace_method to_be_replaced by = replace_place (_type_replace_method to_be_replaced by)

and _type_replace_contract to_be_replaced by c = 
{ c with pre_binders = List.map (function (mt, x, e) -> 
        type_replace_main_type to_be_replaced by mt,
        x,
        e        
    ) c.pre_binders
}
and type_replace_contract to_be_replaced by = replace_place (_type_replace_contract to_be_replaced by)

and _type_replace_port to_be_replaced by p =
{ p with expecting_st = type_replace_main_type to_be_replaced by p.expecting_st
}
and type_replace_port to_be_replaced by = replace_place (_type_replace_port to_be_replaced by)

and _type_replace_component_item to_be_replaced by = function
| State s -> State (type_replace_state to_be_replaced by s)
| Method m -> Method (type_replace_method to_be_replaced by m)
| Contract c -> Contract (type_replace_contract to_be_replaced by c)   
| Port p -> Port (type_replace_port to_be_replaced by p)
| Term t -> Term (type_replace_term to_be_replaced by t)
| Include ce -> Include ce
and type_replace_component_item to_be_replaced by = replace_place (_type_replace_component_item to_be_replaced by)

and _type_replace_component_dcl to_be_replaced by = function
| ComponentStructure cdcl -> ComponentStructure {cdcl with 
    args = List.map (type_replace_param to_be_replaced by) cdcl.args;
    body = List.map (type_replace_component_item to_be_replaced by) cdcl.body
}
| ComponentAssign cdcl -> ComponentAssign {cdcl with 
args = List.map (type_replace_param to_be_replaced by) cdcl.args
}
and type_replace_component_dcl to_be_replaced by = replace_place (_type_replace_component_dcl to_be_replaced by)

and type_replace__typealias_body to_be_replaced by = function
| AbstractTypealias mt -> AbstractTypealias (type_replace_main_type to_be_replaced by mt) 
| BBTypealias _ as tb -> tb 

and _type_replace_term to_be_replaced by = function
| Stmt stmt -> Stmt (type_replace_stmt to_be_replaced by stmt)
| Component cdcl -> Component (type_replace_component_dcl to_be_replaced by cdcl)
| Typealias (x, body) -> Typealias (x, type_replace__typealias_body to_be_replaced by body)
| Typedef {value= ClassicalDef (x, args, body) as tdef; place} | Typedef {value= EventDef (x, args, body) as tdef; place} -> 
    let args = List.map (type_replace_main_type to_be_replaced by) args in 

    Typedef ({ place; value = 
        match tdef with
        | ClassicalDef _ -> ClassicalDef (x, args, body)
        | EventDef _ -> EventDef (x, args, body)
    })
| term -> term
and type_replace_term to_be_replaced by = replace_place (_type_replace_term to_be_replaced by)