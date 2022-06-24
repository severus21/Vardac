%start<Ast.plg_annotation list> entry

%{

open Core.AstUtils
open Ast

%}

%%

entry:
  t = list(any_annot) EOF
    { t }

_any_annot:
| OVERRIDE
    { AOverride }
| EXTENDS x=UID
    { AExtends x}
| IMPLEMENTS x=UID
    { AImplements x}

%public %inline any_annot:
  t = placed(_any_annot)
    {t}