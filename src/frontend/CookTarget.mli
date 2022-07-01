(** This module translates [RawTarget] into [Target] namely it is in charge of transforming string to atom (to unify notations with IR).
    - ensuring that each name (entrypoint, bootstrap) is properly bound by an IR binder + replace it by the corresponding atom 
*)

module Make : functor () -> sig
    val cook_targets : Core.IR.program -> RawTarget.targets -> Core.Target.targets
end