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

let rec clean_body_v place : _body -> _body = function 
| ClassOrInterfaceDeclaration cid ->
    let body = List.map citem cid.body in

    (* Toplevel stmt -> FieldDeclaration *)
    let transform_citem = function
    | {place = p1; value=Stmt {place=p2; value= NamedExpr(t, x, e_opt)}} -> 
        {   
            place = p1;
            value=Body {
                place = p2;
                value = {
                    annotations = [Visibility Public];
                    decorators = [];
                    v = FieldDeclaration {
                        type0 = t;
                        name = x;
                        body = e_opt
                    }
                }
            }
        }
    | item -> item
    in

    ClassOrInterfaceDeclaration { cid with 
        body = List.map transform_citem body
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