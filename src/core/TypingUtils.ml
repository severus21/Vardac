open IR

(* Dans type inference*)
(*context already gamma do we need more yes*)
type context = {
    ectx : (expr_variable, main_type) Hashtbl.t; (* Typing context for expressions *) 
    tctx : unit; (* Contexts of types*)
    cctx : (component_variable, main_type) Hashtbl.t; (* Typing context for components *)
    self : component_variable option;
}
let fresh_context () =
    {
        ectx = Hashtbl.create 64;
        tctx = ();
        cctx = Hashtbl.create 64;
        self = None 
    }