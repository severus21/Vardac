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

blacbox_body:
| b = BLACKBOX_BODY 
    { b }

any_blackbox_term_:
| language = LID body = blacbox_body
    { {language; body} }
%inline any_blackbox_term:
  t = placed(any_blackbox_term_)
    {t}

any_type_impl:
| IMPL TYPE name=LID body = any_blackbox_term 
    { {name; body} }

any_core_method_impl_:
| ret_type=any_type name=LID LPAREN args=right_flexible_list(COMMA, any_param) RPAREN body = any_blackbox_term 
{ MethodImpl {ret_type; name; args; body}}
%inline any_core_method_impl:
  t = placed(any_core_method_impl_)
    {t}

any_state_impl_:
| type0=any_type name=LID body = any_blackbox_term 
{ StateImpl {type0; name; body}}
%inline any_state_impl:
  t = placed(any_state_impl_)
    {t}

any_component_item_impl:
| m = any_core_method_impl 
    { m }
| s = any_state_impl
    { s }

any_component_impl:
| IMPL COMPONENT name=UID LCURLYBRACKET body=flexible_sequence(any_component_item_impl) RCURLYBRACKET 
    { {name; body} }

any_term_:
| c=any_component_impl
    { ComponentImpl c }
| t_impl = any_type_impl
    { TypedefImpl t_impl }
%inline any_term:
    t = placed(any_term_)
    { t }

(* -------------------------------------------------------------------------- *)