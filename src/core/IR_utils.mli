open IR 

val find_lca_program : Atom.Set.t -> program -> Atom.atom option
val insert_in_terms : term list -> term list -> term list
val insert_terms_into_lca : (Atom.atom option) list -> term list -> program -> program