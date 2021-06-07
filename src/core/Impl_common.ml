open AstUtils

type _blackbox_term = {
    language: string;
    body: string;
    template: bool
} 

and blackbox_term = _blackbox_term placed
[@@deriving show { with_path = false }]