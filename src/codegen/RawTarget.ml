open Core.AstUtils

type  codegen_info= {runtime_plg: string; language_plg: string}
and _target = Target of {name:string; codegen: codegen_info}     
and target = _target placed
and targets = target list
[@@deriving show { with_path = false }]

