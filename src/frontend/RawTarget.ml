open Core.AstUtils

type  maindef = {
    name: string; 
    bootstrap: string;
    entrypoint: string;
}
and codegen_info = {
    runtime_plg: string; 
    language_plg: string;
    interface_plg: string;
    mains: maindef list    
}
and _target = {
    name:string; 
    codegen: codegen_info}     
and target = _target placed
and targets = target list
[@@deriving show { with_path = false }]

