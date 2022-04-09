open AstUtils

type _blackbox_term = {
    language: string option;
    body: string;
} 

and blackbox_term = _blackbox_term placed
[@@deriving show { with_path = false }]