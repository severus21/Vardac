module Make : functor () -> sig 
    val gtransform_program : Core.IRI.program -> Core.IRI.program
end