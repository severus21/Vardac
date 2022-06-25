%start<Ast_impl.program> entry

%{
open Ast
open Ast_impl
open Core.AstUtils
open Core.Label
open Core

%}

%%

(* -------------------------------------------------------------------------- *)

(* A toplevel phrase is just a term. *)

entry:
  t = list(any_term) EOF
    { t }
(* -------------------------------------------------------------------------- *)

blackbox_body:
| b = BLACKBOX_BODY 
    { 
        let body = String.trim b in
        let varda_regexp = Str.regexp "{{%\\|%}}" in
        let tokens = Str.full_split varda_regexp body in

        (* debug only *)
        (*List.map (function 
            | Str.Text t -> Printf.printf "Text <%s>\n" t
            | Str.Delim t -> Printf.printf "Delim <%s>\n" t
        ) tokens;*)

        let rec aux = function
            | [] -> []
            | (Str.Delim "{{%"):: (Str.Text code):: (Str.Delim "%}}")::xs -> 
                Printf.printf "\n\n>> %s" code;
                let (pos:Lexing.position) = fst $loc in
                let e = Parse.parse_expr pos.pos_fname code in
                (* TODO update loc number *)

                (Impl.Varda e)::(aux xs) 
            | (Str.Text code) :: xs-> (Impl.Text code)::(aux xs)
            | _ -> Error.perror [$loc] "Blackbox term is ill-formed!"
        in
        aux tokens
    }

any_var:
| x = right_flexible_list(DOUBLE_COLON, LID)
    { x }

any_blackbox_term_:
| body = blackbox_body
    { {Impl.language=None; body;} } (* language is set in a subsequent pass based on target *)
| language = LID COLON body = blackbox_body (* language is used for syntax coloring only *)
    { {Impl.language=Some language; body;} }
%inline any_blackbox_term:
  t = placed(any_blackbox_term_)
    {t}

any_function_impl:
| IMPL FUNCTION name=any_var body = any_blackbox_term 
    { {name; body}:function_impl }

any_type_impl:
| IMPL TYPE name=any_var body = any_blackbox_term 
    { {name; body} }

any_core_method_impl_:
| IMPL METHOD name=any_var body = any_blackbox_term 
{ MethodImpl {name; body = Some(body)}}
| IMPL METHOD name=any_var SEMICOLON 
{ MethodImpl {name; body = None}}
%inline any_core_method_impl:
  t = placed(any_core_method_impl_)
    {t}

any_state_impl_:
| type0=any_type name=any_var body = any_blackbox_term 
{ StateImpl {type0; name; body}}
%inline any_state_impl:
  t = placed(any_state_impl_)
    {t}

_any_component_item_impl:
| m = any_core_method_impl 
    { {place = [$loc]@m.place; value = { plg_annotations = []; v = m.value }} }
| s = any_state_impl
    { {place = [$loc]@s.place; value = { plg_annotations = []; v = s.value }} }
| HEADERS body = any_blackbox_term
    { {place=[$loc]; value={ plg_annotations = []; v =ComponentHeadersImpl body }}}

any_component_item_impl:
| annots=list(PLG_ANNOT) item=_any_component_item_impl(* replace UID *) 
    { {item with 
        value = { item.value with plg_annotations = annots@item.value.plg_annotations }} }

any_component_impl:
| IMPL COMPONENT name=right_flexible_list(DOUBLE_COLON, UID) LCURLYBRACKET body=flexible_sequence(any_component_item_impl) RCURLYBRACKET 
    { {name; body; target = None} }
| IMPL COMPONENT name=right_flexible_list(DOUBLE_COLON, UID) TARGET target=LID LCURLYBRACKET body=flexible_sequence(any_component_item_impl) RCURLYBRACKET 
    { {name; body; target = Some(target)} }

any_term__:
| TARGET target = LID SEMICOLON
    { CurrentDefaultTarget target }
| IMPL HEADERS body = any_blackbox_term
    { HeadersImpl body }
| IMPL DEPENDENCIES body = any_blackbox_term
    { DependenciesImpl body }
| c=any_component_impl
    { ComponentImpl c }
| f_impl = any_function_impl
    { FunctionImpl f_impl }
| t_impl = any_type_impl
    { TypeImpl t_impl }

any_term_:
| annots=list(PLG_ANNOT) t=any_term__
    { { plg_annotations = annots; v = t} }
| t = any_term__
    { { plg_annotations = []; v = t} }
%inline any_term:
    t = placed(any_term_)
    { t }

(* -------------------------------------------------------------------------- *)