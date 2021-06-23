(* Split a program, an IRI AST, to a set of programs - one program per target.
   The split is done at top-level
   - non component term are added to all program 
        - TODO FIXME for now, this can add deadcode
        - TODO FIXME for now, this can add not working code if the component is used inside the other toplevel code or inside some components (e.g. types parametrized by components)
   - components are added according to their target attribute
*)
val split_program : Target.targets -> IRI.program -> (Target.target * IRI.program) Seq.t 