open AstUtils

type maindef = {
    (* main name *)
    name: string; 
    (* init component - toplevel is magic bootstrap name *)
    bootstrap: Atom.atom;
    entrypoint: Atom.atom;
    _not_create_main: bool; (* inner used only *)
}
and codegen_info = {
    runtime_plg: string; 
    language_plg: string;
    interface_plg: string;
    mains: maindef list    
}
and _target = {
    name:string; 
    codegen: codegen_info;
    user_defined: string;
    compiler: string Collections.StringMap.t [@to_yojson SerializationUtils.stringmap_to_yojson] [@of_yojson SerializationUtils.stringmap_of_yojson]
} 
and target = _target placed
and targets = target list
[@@deriving show { with_path = false }, yojson]

(** 
    @param x - component name 
    @return true if it is a guardian *)
let is_guardian (targets: targets) x = 
    List.exists (function target -> 
        List.exists (function mdef -> mdef.bootstrap = x) target.value.codegen.mains  
    ) targets 
