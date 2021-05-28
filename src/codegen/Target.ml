(* variable : string => Atom.atom *)
open Core
open Core.AstUtils

type codegen_info = RawTarget.codegen_info = {runtime_plg: string; language_plg: string}
and _target = Target of {name:Atom.atom; codegen: codegen_info} 
and target = _target placed
and targets = target list
[@@deriving show { with_path = false }]

