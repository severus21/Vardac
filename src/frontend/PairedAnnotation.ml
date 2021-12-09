open Ast
open Easy_logging
open Core
open AstUtils
let logger = Logging.make_logger "_1_ compspec.Frontend" Debug [];;

let rec apair_component_item place : _component_item -> _component_item = function 
| Term t -> Term (a_term t)
| citem -> citem
and a_component_item citem : component_item = map_place (apair_component_item) citem

and apair_citems (acc_annotations : annotation list) : component_item list -> component_item list = function
| [] -> []
| [{place; value=Term {value=Annotation _}}] -> Error.error place "This annotation is linked to no term"
| {value=Term {value=Annotation a1}} :: {value=Term {value=Annotation a2}} :: ts ->
    apair_citems (a2::a1::acc_annotations) ts
| {value=Term {value=Annotation a}} :: {place=p_tcomp; value=Term {place=p_comp; value=Component {place = p_struct; value=ComponentStructure comp} }} :: ts ->
    {place = p_tcomp; value = Term {
        place = p_comp; 
        value=Component {place=p_struct;
            value= ComponentStructure {comp with annotations = List.rev (a::acc_annotations)}}
    }} :: (apair_citems [] ts)
| {value=Term{value=Annotation a}} :: {place; value=Method m} :: ts ->
    {place; value = Method {
        m with value = {m.value with 
            annotations = List.rev (a::acc_annotations)
        }
    }} :: (apair_citems [] ts)
| {place; value=Term {value=Annotation a}} :: _ :: ts -> Error.error place "Annotations are only supported for components or methods"
| t::ts -> t::(apair_citems [] ts)

and apair_component_dcl place : _component_dcl -> _component_dcl = function  
| ComponentStructure cdcl ->
    (* Recursive apairing in components *)
    let body = List.map a_component_item cdcl.body in

    (* Do the top-level appairing *)
    ComponentStructure {cdcl with body = apair_citems [] cdcl.body }
| citem -> citem

and a_component_dcl cdcl: component_dcl = map_place (apair_component_dcl ) cdcl

(************************************ Program *****************************)
and apair_term place : _term -> _term = function
| Component comp -> Component (a_component_dcl comp)
| t -> t
and a_term t : term = map_place (apair_term) t

and apair_terms acc_annotations : term list -> term list = function
| [] -> []
| [{place; value=Annotation a}] -> Error.error place "This annotation is linked to no term"
| {value=Annotation a1} :: {value=Annotation a2} :: ts ->
    apair_terms (a2::a1::acc_annotations) ts
| {value=Annotation a} :: {place=p_comp; value=Component {place=p_struct; value = ComponentStructure cdcl}} :: ts ->
    {place = p_comp; value=Component { place = p_struct; value= ComponentStructure {cdcl with annotations = List.rev (a::acc_annotations)}}} :: (apair_terms [] ts)
| {place; value=Annotation a} :: _ :: ts -> Error.error place "Annotations are only supported for components"
| t::ts -> t::(apair_terms [] ts)

and apair_program program = 
    (* Recursive apairing in components *)
    let program = List.map a_term program in

    (* Do the top-level appairing *)
    apair_terms [] program