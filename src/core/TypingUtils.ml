open IR
open AstUtils

(* Dans type inference*)
(*context already gamma do we need more yes*)
type context = {
    ectx : main_type Atom.VMap.t; (* Typing context for expressions *) 
    tctx : main_type Atom.VMap.t; (* Contexts of types*)
    cctx : main_type Atom.VMap.t; (* Typing context for components *)
    self : component_variable option;
}


let fresh_context () =
    {
        ectx = Atom.VMap.empty;
        tctx = Atom.VMap.empty;
        cctx = Atom.VMap.empty;
        self = None 
    }
let typeof_var_expr ctx x : main_type =
    if Atom.is_builtin x then
        Builtin.type_of (Atom.hint x)
    else
        try
            Atom.VMap.find x ctx.ectx
        with Not_found -> failwith (Printf.sprintf "notfound type of expr %s" (Atom.to_string x))

let typeof_var_cexpr ctx x : main_type =
    try
        Atom.VMap.find x ctx.cctx
    with Not_found -> failwith (Printf.sprintf "notfound type of cexpr %s" (Atom.to_string x))


let register_expr_type ctx x mt = {
    ctx with 
        ectx = Atom.VMap.add x mt ctx.ectx 
}

let register_cexpr_type ctx x mt = {
    ctx with 
        cctx = Atom.VMap.add x mt ctx.cctx 
}

(*let typeof_constructor = 
    let fplace = (Error.forge_place "TypeUtils.typeof_constructor" 0 0) in
    let auto_fplace smth = {place = fplace; value=smth} in
    let ctypeof x = auto_fplace (CType(auto_fplace x)) in
    function
| [mt] -> mt
| mt1::mts -> TArrow (mt1, typeof_constructor mts)*)



let fct_sign argmts ret_type = 
    let fplace = (Error.forge_place "TypeUtils.register_def_type" 0 0) in
    let auto_fplace smth = {place = fplace; value=smth} in

    List.fold_right (fun t1 t2 -> auto_fplace (CType (auto_fplace(TArrow (t1, t2))))) argmts ret_type 

let register_def_type ctx = 
    let fplace = (Error.forge_place "TypeUtils.register_def_type" 0 0) in
    let auto_fplace smth = {place = fplace; value=smth} in
    let ctypeof x = auto_fplace (CType(auto_fplace x)) in
    
function
| ClassicalDef (x, mts, ()) | EventDef (x, mts, ()) ->
    {
        ctx with
        tctx = Atom.VMap.add x (ctypeof(TTuple mts)) ctx.tctx; (*FIXME support other things than tuple*)
        ectx = Atom.VMap.add x (fct_sign mts (ctypeof (TVar x))) ctx.ectx (* register constructor *)
    }
| ProtocolDef (x, mt) -> 
    {
        ctx with
        tctx = Atom.VMap.add x mt ctx.tctx; (*FIXME*)
        ectx = Atom.VMap.add x mt ctx.ectx (* register protocol object *)
    }

let register_self ctx x = { ctx with self = Some x }
let register_type ctx x mt = {
    ctx with tctx = Atom.VMap.add x mt ctx.tctx
}