open AstUtils

type blackbox_body = 
| Text of string
| Varda of string

and _blackbox_term = {
    language: string option;
    body: string; (*blackbox_body list;*)
} 

and blackbox_term = _blackbox_term placed
[@@deriving show { with_path = false }]