open IR

(* Dans type inference*)
(*context already gamma do we need more yes*)
type context = {
    ectx : main_type Atom.VMap.t; (* Typing context for expressions *) 
    tctx : unit; (* Contexts of types*)
    cctx : main_type Atom.VMap.t; (* Typing context for components *)
    self : component_variable option;
}


let fresh_context () =
    {
        ectx = Atom.VMap.empty;
        tctx = ();
        cctx = Atom.VMap.empty;
        self = None 
    }
let typeof_var_expr ctx x : main_type =
    Atom.VMap.find x ctx.ectx

let typeof_var_cexpr ctx x : main_type =
    Atom.VMap.find x ctx.cctx


let register_expr_type ctx x mt = {
    ctx with 
        ectx = Atom.VMap.add x mt ctx.ectx 
}

let register_cexpr_type ctx x mt = {
    ctx with 
        cctx = Atom.VMap.add x mt ctx.cctx 
}

let register_self ctx x = { ctx with self = Some x }