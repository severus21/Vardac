open AstUtils
open IR_common

(* IR extended with blackbox implementation for type, methods and states *)
type iri_target_name = string

and blackbox_body = 
| Text of string
| Varda of IR.expr 

and _blackbox_term = {
    language: string option;
    body: blackbox_body list;
} 

and blackbox_term = _blackbox_term placed

and iri_state_dcl_body = 
| InitExpr of expr
| InitBB of blackbox_term
| NoInit

and iri_custom_method0_body = 
| AbstractImpl of stmt list
| BBImpl of blackbox_term

and iri_typealias_body = 
| AbstractTypealias of main_type
| BBTypealias of blackbox_term 
and iri_typedef_body = blackbox_term option 
[@@deriving show { with_path = false }]


(*
    Type
    Make(param), pb 2 make
    ...

    Method 1
    IRCAtom IRCDebreijn IRCString
    IR_templateAtom IR_templateDebre IR....

*)


module Params : (
    IR_template.IRParams with   
        type target_name = iri_target_name and
        type _state_dcl_body = iri_state_dcl_body and 
        type _custom_method0_body = iri_custom_method0_body and 
        type _typealias_body = iri_typealias_body and
        type _typedef_body = iri_typedef_body
) = struct
    module Variable = Atom
    type target_name = iri_target_name
    and _state_dcl_body = iri_state_dcl_body 
    and _custom_method0_body = iri_custom_method0_body 
    and _typealias_body = iri_typealias_body
    and _typedef_body = iri_typedef_body
    [@@deriving show { with_path = false }]

    let collect_type_state_dcl_body 
        parent_opt already_binded selector collector 
    = function
        | InitExpr e -> collect_type_expr parent_opt already_binded selector collector e
        | InitBB _ | NoInit -> already_binded, [], []
    let rewrite_type_state_dcl_body 
        selector rewriter
    = function 
        | InitExpr e -> InitExpr (rewrite_type_expr selector rewriter e)
        | (InitBB _ as e) | (NoInit as e) -> e
    let rewrite_expr_state_dcl_body 
        selector rewriter
    = function 
        | InitExpr e -> InitExpr (rewrite_expr_expr selector rewriter e)
        | (InitBB _ as e) | (NoInit as e) -> e
        (* TODO FIXME rewrite type0*)

    let collect_expr_state_dcl_body 
        parent_opt already_binded selector collector 
    = function
        | InitExpr e -> collect_expr_expr parent_opt already_binded selector collector e
        | InitBB _ | NoInit -> already_binded, [], []

    let collect_cexpr_state_dcl_body 
        parent_opt already_binded selector collector   
    = function
        | InitExpr e -> already_binded, IR_common.collect_cexpr_expr parent_opt already_binded selector collector e, []
        | InitBB _ | NoInit -> already_binded, [], []

    let collect_cexpr_custom_method0_body 
        parent_opt already_binded selector collector 
    = function 
        | AbstractImpl body ->  already_binded, List.flatten (List.map (collect_cexpr_stmt parent_opt already_binded selector collector) body), []
        | (BBImpl _ ) -> already_binded, [], []

    let collect_type_custom_method0_body 
        parent_opt already_binded selector collector 
    = function 
        | AbstractImpl body ->
            List.fold_left_map (fun set stmt ->         
                let env, a,b  = collect_type_stmt parent_opt set selector collector stmt in
                env, (a,b)
            ) already_binded body
        | (BBImpl _ ) -> already_binded, []
    let rewrite_type_custom_method0_body 
        selector rewriter
    = function
        | AbstractImpl stmts -> 
            AbstractImpl (List.map (rewrite_type_stmt selector rewriter) stmts)
        | (BBImpl _ as x) -> x
    let rewrite_expr_custom_method0_body 
        selector rewriter
    = function
        | AbstractImpl stmts -> 
            AbstractImpl (
                List.map (rewrite_expr_stmt selector rewriter) stmts)
        | (BBImpl _ as x) -> x
    let rewrite_exprstmts_custom_method0_body rewrite_exprstmts_stmt parent_opt exclude_stmt selector rewriter = function 
        | AbstractImpl body ->
            AbstractImpl (
                List.flatten (List.map (rewrite_exprstmts_stmt parent_opt exclude_stmt selector rewriter) body))
        | (BBImpl _ as x) -> x
    let rewrite_stmt_custom_method0_body
    rewrite_stmt_stmt recurse selector rewriter = function 
        | AbstractImpl body ->
            AbstractImpl (
                List.flatten (List.map (rewrite_stmt_stmt recurse selector rewriter) body))
        | (BBImpl _ as x) -> x

    let collect_expr_custom_method0_body 
        parent_opt already_binded selector collector
    = function 
        | AbstractImpl stmts ->
            List.fold_left_map (fun already_binded stmt ->         
                let env, a,b  = collect_expr_stmt parent_opt already_binded selector collector stmt in
                env, (a,b)
            ) already_binded stmts
        | BBImpl _ -> already_binded, []

    let collect_stmt_custom_method0_body 
        parent_opt selector collector 
    = function  
        | AbstractImpl body ->
            List.flatten (List.map (collect_stmt_stmt parent_opt  selector collector) body)
        | BBImpl _ -> []

    let rename_state_dcl_body rename_expr renaming = function
        | InitExpr e -> InitExpr (rename_expr renaming e)
        | (InitBB _) as x | (NoInit as x) -> x
    let rename_custom_method0_body rename_stmt renaming = function 
        | AbstractImpl stmts -> AbstractImpl (List.map (rename_stmt renaming) stmts) 
        | (BBImpl _) as x -> x

    let rename_typealias_body rename_main_type renaming = function 
        | AbstractTypealias mt -> AbstractTypealias (rename_main_type renaming mt) 
        | BBTypealias _ as x -> x
end

(*include IR_common*)
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
| LetStmt (mt, x, e) -> LetStmt (type_replace_main_type to_be_replaced by mt, x, e)
| stmt -> stmt
and type_replace_stmt to_be_replaced by = replace_place (_type_replace_stmt to_be_replaced by)


and _type_replace_constraint_header to_be_replaced by = function
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
and type_replace_state to_be_replaced by = replace_place (_type_replace_state to_be_replaced by)

and _type_replace_method to_be_replaced by (m:_method0)= 
    {
        m with
        ret_type = type_replace_main_type to_be_replaced by m.ret_type;
        args = List.map (type_replace_param to_be_replaced by) m.args;
        body = type_replace__custom_method0_body to_be_replaced by m.body;
        contract_opt = Option.map (type_replace_contract to_be_replaced by) m.contract_opt
    }

and type_replace_method to_be_replaced by = replace_place (_type_replace_method to_be_replaced by)

and _type_replace_contract to_be_replaced by c = 
{ c with pre_binders = List.map (function (mt, x, e) -> 
        type_replace_main_type to_be_replaced by mt,
        x,
        e        
    ) c.pre_binders
}
and type_replace_contract to_be_replaced by = replace_place (_type_replace_contract to_be_replaced by)

and _type_replace_port to_be_replaced by (p, mt_p) =
{ p with expecting_st = type_replace_main_type to_be_replaced by p.expecting_st
}, type_replace_main_type to_be_replaced by mt_p 
and type_replace_port to_be_replaced by = replace_place (_type_replace_port to_be_replaced by)

and _type_replace_component_item to_be_replaced by = function
| State s -> State (type_replace_state to_be_replaced by s)
| Method m -> Method (type_replace_method to_be_replaced by m)
| Contract c -> Contract (type_replace_contract to_be_replaced by c)   
|Inport p ->Inport (type_replace_port to_be_replaced by p)
| Term t -> Term (type_replace_term to_be_replaced by t)
| Include ce -> Include ce
and type_replace_component_item to_be_replaced by = replace_place (_type_replace_component_item to_be_replaced by)

and _type_replace_component_dcl to_be_replaced by = function
| ComponentStructure cdcl -> ComponentStructure {cdcl with 
    body = List.map (type_replace_component_item to_be_replaced by) cdcl.body
}
| ComponentAssign _ as cdcl -> cdcl
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

    