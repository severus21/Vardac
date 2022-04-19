(** This module read target file, parse it and convert the yaml representation to [RawTarget] AST. It checks that:
    - target names are defined exactly once
    - per target:
        - main names are defined exaclty once
        - [no_main] entrypoint is used at most once
        - [laststage] boostrap is used at most once
*)

val parse_targets : string -> RawTarget.targets