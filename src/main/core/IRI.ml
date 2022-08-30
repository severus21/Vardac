open AstUtils
open IR_common
open CommonUtils

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
and iri_component_headers = string list
[@@deriving show { with_path = false }, yojson]




let collect_type_bbbody flag_tcvar parent_opt already_binded selector collector = function
    | Text str -> already_binded, [], [] 
    | Varda e -> collect_type_expr flag_tcvar parent_opt already_binded selector collector e
let rec collect_type_bbterm_ flag_tcvar parent_opt already_binded selector collector place {language; body} =
    List.fold_left (fun (already_binded, collected_elts, tvars) bbbody -> 
        let _, collected_elts1, tvars1 = collect_type_bbbody flag_tcvar parent_opt already_binded selector collector bbbody in
        already_binded, collected_elts@collected_elts1, tvars@tvars1
    ) (already_binded, [], []) body
and collect_type_bbterm flag_tcvar parent_opt already_binded selector collector = map0_place (collect_type_bbterm_ flag_tcvar parent_opt already_binded selector collector)

let rewrite_type_bbbody rewrite_type_expr selector rewriter = function
    | Text str -> Text str
    | Varda e -> Varda (rewrite_type_expr selector rewriter e)
let rec rewrite_type_bbterm_ rewrite_type_expr selector rewriter place {language; body} =
    {language;
    body = List.map (rewrite_type_bbbody rewrite_type_expr selector rewriter) body}
and rewrite_type_bbterm rewrite_type_expr selector collector = map_place (rewrite_type_bbterm_ rewrite_type_expr selector collector)

let collect_expr_bbbody parent_opt already_binded selector collector = function
    | Text str -> already_binded, [], [] 
    | Varda e -> collect_expr_expr parent_opt already_binded selector collector e
let rec collect_expr_bbterm_ parent_opt already_binded selector collector place {language; body} =
    List.fold_left (fun (already_binded, collected_elts, tvars) bbbody -> 
        let _, collected_elts1, tvars1 = collect_expr_bbbody parent_opt already_binded selector collector bbbody in
        already_binded, collected_elts@collected_elts1, tvars@tvars1
    ) (already_binded, [], []) body
and collect_expr_bbterm parent_opt already_binded selector collector = map0_place (collect_expr_bbterm_ parent_opt already_binded selector collector)

let rewrite_expr_bbbody parent_opt selector rewriter = function
    | Text str -> Text str
    | Varda e -> Varda (rewrite_expr_expr parent_opt selector rewriter e)
let rec rewrite_expr_bbterm_ parent_opt selector rewriter place {language; body} =
    {language;
    body = List.map (rewrite_expr_bbbody parent_opt selector rewriter) body}
and rewrite_expr_bbterm parent_opt selector collector = map_place (rewrite_expr_bbterm_ parent_opt selector collector)

let collect_stmt_bbbody parent_opt selector collector = function
    | Text _ -> [] 
    | Varda _ -> [] 
let rec collect_stmt_bbterm_ parent_opt selector collector place {language; body} =
    List.flatten (List.map (function bbbody -> 
        collect_stmt_bbbody parent_opt selector collector bbbody
    )  body)
and collect_stmt_bbterm parent_opt selector collector = map0_place (collect_stmt_bbterm_ parent_opt selector collector)

let rewrite_stmt_bbbody recurse selector rewriter = function
    | Text str -> Text str
    | Varda e -> Varda e
let rec rewrite_stmt_bbterm_ recurse selector rewriter place {language; body} =
    {language;
    body = List.map (rewrite_stmt_bbbody recurse selector rewriter) body}
and rewrite_stmt_bbterm recurse selector collector = map_place (rewrite_stmt_bbterm_ recurse selector collector)

let rewrite_exprstmts_bbbody parent_opt exclude_stmt selector rewriter = function
    | Text str -> Text str
    | Varda e -> Varda e
let rec rewrite_exprstmts_bbterm_ parent_opt exclude_stmt selector rewriter place {language; body} =
    {language;
    body = List.map (rewrite_exprstmts_bbbody parent_opt exclude_stmt selector rewriter) body}
and rewrite_exprstmts_bbterm parent_opt exclude_stmt selector collector = map_place (rewrite_exprstmts_bbterm_ parent_opt exclude_stmt selector collector)

let collect_cexpr_bbbody parent_opt already_binded selector collector = function
    | Text str -> [] 
    | Varda e -> collect_cexpr_expr parent_opt already_binded selector collector e
let rec collect_cexpr_bbterm_ parent_opt already_binded selector collector place {language; body} =
    List.fold_left (fun collected_elts bbbody -> 
        let collected_elts1 = collect_cexpr_bbbody parent_opt already_binded selector collector bbbody in
        collected_elts@collected_elts1
    ) [] body
and collect_cexpr_bbterm parent_opt already_binded selector collector = map0_place (collect_cexpr_bbterm_ parent_opt already_binded selector collector)

let rewrite_cexpr_bbbody selector rewriter = function
    | Text str -> Text str
    | Varda e -> Varda e
let rec rewrite_cexpr_bbterm_ selector rewriter place {language; body} =
    {language;
    body = List.map (rewrite_cexpr_bbbody selector rewriter) body}
and rewrite_cexpr_bbterm selector collector = map_place (rewrite_cexpr_bbterm_ selector collector)

let rename_bbbody flag_rename_attribute flag_rename_type renaming = function
    | Text str -> Text str
    | Varda e -> Varda (rename_expr ~flag_rename_attribute:flag_rename_attribute flag_rename_type renaming e)

let rec rename_bbterm_ flag_rename_attribute flag_rename_type renaming place {language; body} =
    {language;
    body = List.map (rename_bbbody flag_rename_attribute flag_rename_type renaming) body}
and rename_bbterm flag_rename_attribute flag_rename_type renaming = map_place (rename_bbterm_ flag_rename_attribute flag_rename_type renaming)

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
        type _typedef_body = iri_typedef_body and
        type component_headers = iri_component_headers
) = struct
    module Variable = Atom
    type target_name = iri_target_name
    and _state_dcl_body = iri_state_dcl_body 
    and _custom_method0_body = iri_custom_method0_body 
    and _typealias_body = iri_typealias_body
    and _typedef_body = iri_typedef_body
    and component_headers = iri_component_headers
    [@@deriving show { with_path = false }, yojson]

    let collect_type_state_dcl_body 
        flag_tcvar parent_opt already_binded selector collector 
    = function
        | InitExpr e -> collect_type_expr flag_tcvar parent_opt already_binded selector collector e
        | InitBB bbterm -> collect_type_bbterm flag_tcvar parent_opt already_binded selector collector bbterm
        | NoInit -> already_binded, [], []
    let rewrite_type_state_dcl_body 
        selector rewriter
    = function 
        | InitExpr e -> InitExpr (rewrite_type_expr selector rewriter e)
        | InitBB bbterm -> InitBB (rewrite_type_bbterm rewrite_type_expr selector rewriter bbterm) 
        | (NoInit as e) -> e
    let rewrite_expr_state_dcl_body 
        parent_opt selector rewriter
    = function 
        | InitExpr e -> InitExpr (rewrite_expr_expr parent_opt selector rewriter e)
        | InitBB bbterm -> InitBB (rewrite_expr_bbterm parent_opt selector rewriter bbterm) 
        | (NoInit as e) -> e
        (* TODO FIXME rewrite type0*)

    let collect_expr_state_dcl_body 
        parent_opt already_binded selector collector 
    = function
        | InitExpr e -> collect_expr_expr parent_opt already_binded selector collector e
        | InitBB bbterm -> collect_expr_bbterm parent_opt already_binded selector collector bbterm 
        | NoInit -> already_binded, [], []

    let collect_cexpr_state_dcl_body 
        parent_opt already_binded selector collector   
    = function
        | InitExpr e -> already_binded, collect_cexpr_expr parent_opt already_binded selector collector e, []
        | InitBB bbterm -> already_binded, collect_cexpr_bbterm parent_opt already_binded selector collector bbterm, []
        | NoInit -> already_binded, [], []

    let collect_cexpr_custom_method0_body 
        parent_opt already_binded selector collector 
    = function 
        | AbstractImpl body ->  already_binded, List.flatten (List.map (collect_cexpr_stmt parent_opt already_binded selector collector) body), []
        | BBImpl bbterm -> already_binded, collect_cexpr_bbterm parent_opt already_binded selector collector bbterm, []

    let collect_type_custom_method0_body 
        flag_tcvar parent_opt already_binded selector collector 
    = function 
        | AbstractImpl body ->
            List.fold_left_map (fun set stmt ->         
                let env, a,b  = collect_type_stmt flag_tcvar parent_opt set selector collector stmt in
                env, (a,b)
            ) already_binded body
        | BBImpl bbterm -> 
            let env, a, b = collect_type_bbterm flag_tcvar parent_opt already_binded selector collector bbterm in
            env, [ (a,b) ]
    let rewrite_type_custom_method0_body 
        selector rewriter
    = function
        | AbstractImpl stmts -> 
            AbstractImpl (List.map (rewrite_type_stmt selector rewriter) stmts)
        | BBImpl bbterm -> BBImpl (rewrite_type_bbterm rewrite_type_expr selector rewriter bbterm)
    let rewrite_expr_custom_method0_body 
        parent_opt selector rewriter
    = function
        | AbstractImpl stmts -> 
            AbstractImpl (
                List.map (rewrite_expr_stmt parent_opt selector rewriter) stmts)
        | BBImpl bbterm -> BBImpl (rewrite_expr_bbterm parent_opt selector rewriter bbterm) 
    let rewrite_exprstmts_custom_method0_body rewrite_exprstmts_stmt parent_opt exclude_stmt selector rewriter = function 
        | AbstractImpl body ->
            AbstractImpl (
                List.flatten (List.map (rewrite_exprstmts_stmt parent_opt exclude_stmt selector rewriter) body))
        | BBImpl bbterm -> BBImpl (rewrite_exprstmts_bbterm parent_opt exclude_stmt selector rewriter bbterm)
    let rewrite_stmt_custom_method0_body
    rewrite_stmt_stmt recurse parent_opt selector rewriter = function 
        | AbstractImpl body ->
            AbstractImpl (
                List.flatten (List.map (rewrite_stmt_stmt recurse parent_opt selector rewriter) body))
        | BBImpl bbterm -> BBImpl (rewrite_stmt_bbterm recurse selector rewriter bbterm)

    let collect_expr_custom_method0_body 
        parent_opt already_binded selector collector
    = function 
        | AbstractImpl stmts ->
            List.fold_left_map (fun already_binded stmt ->         
                let env, a,b  = collect_expr_stmt parent_opt already_binded selector collector stmt in
                env, (a,b)
            ) already_binded stmts
        | BBImpl bbterm -> 
            let env, a ,b = collect_expr_bbterm parent_opt already_binded selector collector bbterm in
            env, [a,b]

    let collect_stmt_custom_method0_body 
        parent_opt selector collector 
    = function  
        | AbstractImpl body ->
            List.flatten (List.map (collect_stmt_stmt parent_opt  selector collector) body)
        | BBImpl bbterm -> collect_stmt_bbterm parent_opt selector collector bbterm

    let rename_state_dcl_body flag_rename_attribute flag_rename_type renaming = function
        | InitExpr e -> InitExpr (rename_expr ~flag_rename_attribute:flag_rename_attribute flag_rename_type renaming e)
        | InitBB bbterm -> InitBB (rename_bbterm flag_rename_attribute flag_rename_type renaming bbterm)
        | (NoInit as x) -> x
    let rename_custom_method0_body flag_rename_attribute flag_rename_type renaming = function 
        | AbstractImpl stmts -> AbstractImpl (List.map (rename_stmt ~flag_rename_attribute:flag_rename_attribute flag_rename_type renaming) stmts) 
        | BBImpl bbterm -> BBImpl (rename_bbterm flag_rename_attribute flag_rename_type renaming bbterm)

    let rename_typealias_body flag_rename_attribute flag_rename_type renaming = function 
        | AbstractTypealias mt -> AbstractTypealias (rename_main_type renaming mt) 
        | BBTypealias bbterm -> BBTypealias (rename_bbterm flag_rename_attribute flag_rename_type renaming bbterm)



    let rewrite_type_typealias_body rewrite_type_expr rewrite_type_mtype selector rewriter = function 
    | BBTypealias bbterm -> BBTypealias (rewrite_type_bbterm rewrite_type_expr selector rewriter bbterm)
    | AbstractTypealias mt -> AbstractTypealias (rewrite_type_mtype selector rewriter mt)

    let rewrite_type_typedef_body rewrite_type_expr selector rewriter = Option.map (rewrite_type_bbterm rewrite_type_expr selector rewriter)
    let collect_type_typealias_body flag_tcvar parent_opt already_binded selector collector = function
    | BBTypealias bbterm -> collect_type_bbterm flag_tcvar parent_opt already_binded selector collector bbterm
    | AbstractTypealias mt -> collect_type_mtype ~flag_tcvar:flag_tcvar parent_opt already_binded selector collector mt
    let collect_type_typedef_body flag_tcvar parent_opt already_binded selector collector = function
    | None -> already_binded, [], []
    | Some bbterm -> collect_type_bbterm flag_tcvar parent_opt already_binded selector collector bbterm
end

(*include IR_common*)
module IRI = IR_template.Make(Params) 
include IRI
include IRI.IRUtils
