open IR
open AstUtils

let aid_of place e = 
    let auto_place smth = {place; value=smth} in

    auto_place (CallExpr (
        auto_place (VarExpr (Atom.builtin "activationid"), auto_place EmptyMainType),
        [
            e
        ]
    ), auto_place EmptyMainType)


(* FIXME limitaiton - static maybe add some dynamic reflexive capabilities *)
let schema_of cexpr = match fst cexpr.value with
| VarCExpr x -> x
| _ -> failwith "Other kind of component Expr not yet supported by schema_of"

let schema_to_label place schema = 
    let auto_place smth = {place; value=smth} in
    auto_place (LitExpr (auto_place (StringLit (Atom.to_string schema))), auto_place EmptyMainType)