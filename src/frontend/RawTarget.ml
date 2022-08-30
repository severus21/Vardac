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
    codegen: codegen_info;
    user_defined: string;
    compiler: string Core.Collections.StringMap.t [@to_yojson Core.SerializationUtils.stringmap_to_yojson] [@of_yojson Core.SerializationUtils.stringmap_of_yojson]
}     
and target = _target placed
and targets = target list
[@@deriving show { with_path = false }, yojson]

