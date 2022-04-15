open IR 

val free_tvars_component_item : Atom.Set.t ->
    _component_item AstUtils.placed ->
    Atom.Set.t * type_variable list
val free_tvars_program : Atom.Set.t ->
    term list -> Atom.Set.t * type_variable list

val free_vars_component_item : Atom.Set.t ->
    _component_item AstUtils.placed ->
    Atom.Set.t *
    (main_type * expr_variable) list
val free_vars_component_dcl : Atom.Set.t ->
    _component_dcl AstUtils.placed ->
    Atom.Set.t *
    (main_type * expr_variable) list
val free_vars_program : Atom.Set.t ->
    term list ->
    Atom.Set.t *
    (main_type * expr_variable) list

val collect_stype_program : Atom.Set.t ->
    (_session_type -> bool) ->
    (Atom.atom option ->
     Atom.Set.t -> session_type -> 'a list) ->
    'b ->
    term list ->
    Atom.Set.t * 'a list * type_variable list
val rewrite_stype_program : (_session_type -> bool) ->
    (_session_type -> _session_type) ->
    program -> program

val rewrite_type_component_item : (_main_type -> bool) ->
    (_main_type -> _main_type) ->
    _component_item AstUtils.placed ->
    _component_item AstUtils.placed

val collect_type_program : Atom.Set.t ->
    (_main_type -> bool) ->
    (Atom.atom option ->
     Atom.Set.t -> main_type -> 'a list) ->
    term list ->
    Atom.Set.t * 'a list * type_variable list
val rewrite_type_program : (_main_type -> bool) ->
    (_main_type -> _main_type) ->
    program -> program

val collect_expr_component_dcl : Atom.atom option ->
    Atom.Set.t ->
    (_expr -> bool) ->
    (Atom.atom option ->
     Variable.Set.t -> expr -> 'a list) ->
    _component_dcl AstUtils.placed ->
    Atom.Set.t * 'a list *
    (main_type * expr_variable) list
val collect_expr_program : Atom.Set.t ->
    (_expr -> bool) ->
    (Atom.atom option ->
     Variable.Set.t -> expr -> 'a list) ->
    term list ->
    Atom.Set.t * 'a list *
    (main_type * expr_variable) list
val rewrite_expr_component_item : (_expr -> bool) ->
    (main_type -> _expr -> _expr) ->
    _component_item AstUtils.placed ->
    _component_item AstUtils.placed
val rewrite_expr_term : (_expr -> bool) ->
    (main_type -> _expr -> _expr) ->
    term -> term
val rewrite_expr_program : (_expr -> bool) ->
    (main_type -> _expr -> _expr) ->
    program -> program

val collect_cexpr_program : (_component_expr -> bool) ->
    (Atom.atom option ->
     Error.place -> _component_expr -> 'a list) ->
    term list -> 'a list

val collect_stmt_program : (_stmt -> bool) ->
    (Atom.atom option ->
     Error.place -> _stmt -> 'a list) ->
    term list -> 'a list
val rewrite_stmt_program : bool ->
    (_stmt -> bool) ->
    (Error.place -> _stmt -> _stmt list) ->
    term list -> term list

val rewrite_exprstmts_stmt : component_variable option ->
    (_stmt -> bool) ->
    (_expr -> bool) ->
    (component_variable option ->
     main_type ->
     _expr ->
     stmt list * (_expr * main_type)) ->
    stmt -> stmt list
val rewrite_exprstmts_program : (_stmt -> bool) ->
    (_expr -> bool) ->
    (component_variable option ->
     main_type ->
     _expr ->
     stmt list * (_expr * main_type)) ->
    term list -> term list

val rewrite_citem_program : (_component_item -> bool) ->
    (Error.place ->
     _component_item -> _component_item list) ->
    term list -> term list

val rewrite_component_program : (component_structure -> bool) ->
    (Error.place ->
     component_structure -> component_structure list) ->
    term list -> term list

val collect_term_program : bool ->
    (_term -> bool) ->
    (Atom.atom list ->
    Error.place -> _term -> 'a list) ->
    term list -> 
    'a list
val rewrite_term_program : (_term -> bool) ->
    (Error.place -> _term -> _term list) ->
    term list -> term list

val rewrite_scopeterm_program : (term -> bool) ->
    (term list -> term list) ->
    term list -> term list

val rename_stmt : (Atom.atom -> Atom.atom) ->
    stmt -> stmt
val rename_component_item : (Atom.atom -> Atom.atom) ->
    _component_item AstUtils.placed ->
    _component_item AstUtils.placed

val find_lca_program : Atom.Set.t -> program -> Atom.atom option
val insert_in_terms : term list -> term list -> term list
val insert_terms_into_lca : (Atom.atom option) list -> term list -> program -> program