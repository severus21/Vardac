open Ast
open Core
open Core.AstUtils
open Easy_logging

let logger = Logging.make_logger ("_1_ compspec.plg.Java") Debug [];;

module AnnotationOrder = struct
  type t = annotation
  let compare = compare
end
module DecoratorOrder = struct
  type t = decorator
  let compare = compare
end
module SAnnotation = Set.Make(AnnotationOrder)
module SDecorator = Set.Make(DecoratorOrder)

let deduplicate_annotations annots = 
    SAnnotation.elements (SAnnotation.of_list annots)

let deduplicate_decorators dcs = 
SDecorator.elements (SDecorator.of_list dcs)

let rec clean_place clean_value ({ AstUtils.place ; AstUtils.value}: 'a AstUtils.placed) = 
    let value = clean_value place value in
    {AstUtils.place; AstUtils.value}


let clean_return_type stmts ret_type = 
    match ret_type.value with
    | TAtomic "Void" ->
        let rec search_return flag stmt = 
            match stmt.value with
            | BlockStmt stmts -> List.fold_left search_return flag stmts
            | IfStmt (_, stmt1, stmt2_opt) ->
                (search_return flag stmt1) ||
                (match Option.map (search_return flag) stmt2_opt with | Some f -> f | None -> flag)
            | ReturnStmt _ -> true
            | _ -> flag
        in

        let flag = List.fold_left search_return false stmts in
        if flag then ret_type else {ret_type with value = TAtomic "void"}
    | _ -> ret_type

let rec clean_body_v place : _body -> _body = function 
| ClassOrInterfaceDeclaration cid ->
    ClassOrInterfaceDeclaration { cid with 
        body = List.map citem cid.body
    }
| MethodDeclaration m0 ->
    MethodDeclaration { m0 with
        ret_type = Option.map (clean_return_type m0.body) m0.ret_type
    }
| tmp -> tmp
and clean_body place {annotations; decorators; v} = { 
    annotations = deduplicate_annotations annotations;
    decorators = deduplicate_decorators decorators;
    v = clean_body_v place v;
}
and cbody b = clean_place clean_body b 
    
and clean_item place = function
| Body b -> Body (cbody b) 
| item -> item 
and citem item =  clean_place clean_item item 

let clean_program program = List.map citem program 