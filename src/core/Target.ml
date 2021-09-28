open AstUtils

type maindef = {
    (* main name *)
    name: string; 
    (* init component - toplevel is magic component name *)
    component: string;
    entrypoint: string;
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

