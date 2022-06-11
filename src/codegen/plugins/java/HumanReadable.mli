(**
    Should be the last pass before outputing
    since we remove the properties guarantee by atom unique ID    
    for a:atom, if atom.hint is unique in its scope (i.e. ctx) then atom become a builtin 
*)

module Make : functor (Arg : sig 
    val filename : string 
    val component_names : Core.Atom.Set.t
end) -> sig 
    include AstCompilationPass.Pass
end
