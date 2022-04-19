open AstUtils

type maindef = {
    (* main name *)
    name: string; 
    (* init component - toplevel is magic bootstrap name *)
    bootstrap: Atom.atom;
    entrypoint: Atom.atom;
}
and codegen_info = {
    runtime_plg: string; 
    language_plg: string;
    mains: maindef list    
}
and _target = {
    name:string; 
    codegen: codegen_info
} 
and target = _target placed
and targets = target list
[@@deriving show { with_path = false }]

(** 
    @param x - component name 
    @return true if it is a guardian *)
let is_guardian (targets: targets) x = 
    List.exists (function target -> 
        List.exists (function mdef -> mdef.bootstrap = x) target.value.codegen.mains  
    ) targets 
