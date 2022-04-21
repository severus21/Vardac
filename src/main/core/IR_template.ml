open AstUtils
open IR_common

(************************************* Base types ****************************)
open Label

(* Abstract types *)
module type IRParams = sig
    type target_name
    type _state_dcl_body
    type _custom_method0_body
    type _typealias_body
    type _typedef_body
    val show_target_name :  target_name -> Ppx_deriving_runtime.string
    val show__typealias_body :  _typealias_body -> Ppx_deriving_runtime.string
    val show__typedef_body :  _typedef_body -> Ppx_deriving_runtime.string
    val show__state_dcl_body :  _state_dcl_body -> Ppx_deriving_runtime.string
    val show__custom_method0_body : _custom_method0_body ->  Ppx_deriving_runtime.string
    val pp_target_name : Ppx_deriving_runtime.Format.formatter -> target_name -> Ppx_deriving_runtime.unit
    val pp__typealias_body : Ppx_deriving_runtime.Format.formatter -> _typealias_body -> Ppx_deriving_runtime.unit
    val pp__typedef_body : Ppx_deriving_runtime.Format.formatter -> _typedef_body -> Ppx_deriving_runtime.unit
    val pp__state_dcl_body : Ppx_deriving_runtime.Format.formatter -> _state_dcl_body -> Ppx_deriving_runtime.unit
    val pp__custom_method0_body : Ppx_deriving_runtime.Format.formatter -> _custom_method0_body -> Ppx_deriving_runtime.unit

    val collect_type_state_dcl_body : 
    Atom.atom option ->
    Atom.Set.t ->
    (_main_type -> bool) ->
    (Atom.atom option -> Atom.Set.t -> main_type -> 'a list) ->
    _state_dcl_body ->
    Atom.Set.t * 'a list * type_variable list


    val collect_expr_state_dcl_body :  
    Atom.atom option ->
            Atom.Set.t ->
            (_expr -> bool) ->
            (Atom.atom option ->
            Atom.Set.t -> expr -> 'a list) ->
              _state_dcl_body ->
            Atom.Set.t * 'a list *
            (main_type * expr_variable) list
    val rewrite_type_state_dcl_body : 
    (_main_type -> bool) ->
    ( _main_type -> _main_type) ->
    _state_dcl_body -> _state_dcl_body
    val rewrite_expr_state_dcl_body : 
    (_expr -> bool) ->
    ( main_type -> _expr -> _expr) ->
    _state_dcl_body -> _state_dcl_body


    val collect_cexpr_state_dcl_body :  
        Atom.atom option ->
        Atom.Set.t ->
        (_component_expr -> bool) ->
        (Atom.atom option -> Atom.Set.t -> Error.place -> _component_expr -> 'a list) ->
        _state_dcl_body ->
        Atom.Set.t * 'a list * (main_type * expr_variable) list

    val collect_type_custom_method0_body : 
        Atom.atom option ->
        Atom.Set.t ->
        (_main_type -> bool) ->
        (Atom.atom option -> Atom.Set.t -> _main_type AstUtils.placed -> 'a list) ->
        _custom_method0_body -> 
        Atom.Set.t * ('a list * type_variable list) list
    val rewrite_type_custom_method0_body : 
    (_main_type -> bool) ->
    ( _main_type -> _main_type) ->
    _custom_method0_body -> _custom_method0_body 
    val rewrite_stmt_custom_method0_body : 
    (
        bool ->
        (_stmt -> bool) ->
        ( Error.place -> _stmt -> _stmt list) ->
        stmt -> stmt list
    ) ->
        bool ->
    (_stmt -> bool) ->
    ( Error.place -> _stmt -> _stmt list) ->
    _custom_method0_body -> _custom_method0_body 
    val rewrite_expr_custom_method0_body :
    (_expr -> bool) ->
    ( main_type -> _expr -> _expr) ->
    _custom_method0_body -> _custom_method0_body 
    val rewrite_exprstmts_custom_method0_body :
    (
        component_variable option ->
            (_stmt -> bool) ->
            (_expr -> bool) ->
            (component_variable option ->
            main_type ->
            _expr ->
            stmt list * (_expr * main_type)) ->
            stmt -> stmt list
    ) ->
    component_variable option ->
    (_stmt -> bool) ->
    (_expr -> bool) ->
    (component_variable option -> main_type -> _expr -> stmt list * (_expr * main_type)) ->
    _custom_method0_body -> _custom_method0_body 


    val collect_cexpr_custom_method0_body :  
        Atom.atom option ->
        Atom.Set.t ->
        (_component_expr -> bool) ->
        (Atom.atom option -> Atom.Set.t -> Error.place -> _component_expr -> 'a list) ->
        _custom_method0_body -> 
        Atom.Set.t * 'a list * (main_type * expr_variable) list

    val collect_expr_custom_method0_body :  
    Atom.atom option ->
            Atom.Set.t ->
            (_expr -> bool) ->
            (Atom.atom option ->
            Atom.Set.t -> expr -> 'a list) ->
              _custom_method0_body ->
            Atom.Set.t * ('a list *
            (main_type * expr_variable) list) list

    val collect_stmt_custom_method0_body :  
    Atom.atom option ->
    (_stmt -> bool) ->
    (Atom.atom option -> Error.place -> _stmt -> 'a list) ->
    _custom_method0_body -> 'a list

    val rename_state_dcl_body : 
    (( Atom.atom -> Atom.atom) ->
        expr -> expr) ->
    (Atom.atom -> Atom.atom) ->
    _state_dcl_body -> _state_dcl_body
    val rename_custom_method0_body : 
    (( Atom.atom -> Atom.atom) ->
        stmt -> stmt) ->
    (Atom.atom -> Atom.atom) ->
    _custom_method0_body -> _custom_method0_body
    val rename_typealias_body : 
    (( Atom.atom -> Atom.atom) ->
        main_type -> main_type) ->
    (Atom.atom -> Atom.atom) ->
    _typealias_body -> _typealias_body
end

module Make (Params : IRParams) = struct
    type target_name = Params.target_name
    type _state_dcl_body = Params._state_dcl_body
    type _custom_method0_body = Params._custom_method0_body
    type _typealias_body = Params._typealias_body
    type _typedef_body = Params._typedef_body
    let show_target_name = Params.show_target_name
    let show__typealias_body = Params.show__typealias_body
    let show__typedef_body = Params.show__typedef_body
    let show__state_dcl_body = Params.show__state_dcl_body
    let show__custom_method0_body = Params.show__custom_method0_body
    let pp_target_name = Params.pp_target_name
    let pp__typealias_body = Params.pp__typealias_body
    let pp__typedef_body = Params.pp__typedef_body
    let pp__state_dcl_body = Params.pp__state_dcl_body
    let pp__custom_method0_body = Params.pp__custom_method0_body

    let collect_type_state_dcl_body x = Params.collect_type_state_dcl_body x
    let rewrite_type_state_dcl_body x = Params.rewrite_type_state_dcl_body x 
    let rewrite_expr_state_dcl_body x = Params.rewrite_expr_state_dcl_body x 

    let collect_expr_state_dcl_body x = Params.collect_expr_state_dcl_body x 

    let collect_cexpr_state_dcl_body x = Params.collect_cexpr_state_dcl_body x 

    let collect_type_custom_method0_body x = Params.collect_type_custom_method0_body x
    let rewrite_type_custom_method0_body x = Params.rewrite_type_custom_method0_body x
    let rewrite_stmt_custom_method0_body rewrite_stmt_stmt = Params.rewrite_stmt_custom_method0_body rewrite_stmt_stmt
    let rewrite_expr_custom_method0_body x = Params.rewrite_expr_custom_method0_body x 
    let collect_expr_custom_method0_body x  = Params.collect_expr_custom_method0_body x 
    let rewrite_exprstmts_custom_method0_body rewrite_exprstmts_stmt  = Params.rewrite_exprstmts_custom_method0_body rewrite_exprstmts_stmt

    let collect_cexpr_custom_method0_body x = Params.collect_cexpr_custom_method0_body x 
    let collect_stmt_custom_method0_body x = Params.collect_stmt_custom_method0_body x 

    let rename_state_dcl_body rename_expr = Params.rename_state_dcl_body rename_expr
    let rename_custom_method0_body rename_stmt = Params.rename_custom_method0_body rename_stmt
    let rename_typealias_body rename_main_type = Params.rename_typealias_body rename_main_type

    (************************************ Component *****************************)
    include AstUtils
    include IR_common

    type _state = 
        | StateDcl of  {
            ghost: bool; 
            type0: main_type; 
            name: component_variable; 
            body: _state_dcl_body
        }
    and state = _state placed


    and _method0 = {
        annotations: method_annotation list;
        ghost: bool; 
        ret_type: main_type; 
        name: component_variable; 
        args: param list; 
        body: _custom_method0_body; 
        contract_opt: contract option;
        on_destroy: bool;
        on_startup: bool;
    }

    and method0 = _method0 placed


    and _component_item =  
        | Contract of contract 
        | Method of method0 
        | State of state 

        (** Inter-component composition*)
        | Inport of port 
        | Outport of outport 

        (** Sub-components *)
        | Term of term    

        (* Reusing component architecture *)
        (* Syntaxic sugar/component manipulation 
            include Y;
            include Y(args); where arg could be statically known or not
            include mylist[0];
        *)
        | Include of component_expr
    and component_item = _component_item placed

    and component_structure = {
        target_name: target_name; 
        annotations: component_annotation list;
        name: component_variable; 
        body: component_item list}

    and _component_dcl = 
        | ComponentAssign of {
            name: component_variable; 
            value: component_expr
        }
        | ComponentStructure of component_structure

    and component_dcl = _component_dcl placed

  (********************** Signatures *********************)

  (* TODO 

  and field_sig =
    | SF of variable * ctype(*field x: t*)
    | SFInvariant of variable * ctype * term (* where t2 is invariant*)
  and method_sig =
    | SM of variable * ctype list  (*method g arg1 argn*)
  and contract_sig =                                    
    | SC of variable * term option * term option (* ensure t1 returns t2*)
    | SContractWith of variable * variable * term * term option * term option (*x with y= t1 ensures ...*) 
  and port_sig =
    | SP of variable * variable  (*port x on c*)
  and signature_item =
    | SField of field_sig
    | SMethod of method_sig             
    | SContract of contract_sig              
    | SPort of port_sig
    | SigDcl of signature_dcl    
    | CArgExpr of term                             
  and signature_dcl = 
    | CSig of variable * pattern_expr list * signature_item list  (*name, args, body*)
  *)
    
    (************************************ Program *****************************)
    and _function_dcl = {
        name: expr_variable;
        targs: expr_variable list; (* generic type parameters *)
        ret_type: main_type;
        args: param list;
        body: _custom_method0_body;
    }
    and function_dcl = _function_dcl placed

    and _typedef = (* Two kind for type *) 
    | ClassicalDef of type_variable * main_type list * _typedef_body
    | EventDef of type_variable * main_type list * _typedef_body
    | ProtocolDef of type_variable * main_type
    | VPlaceDef of type_variable
    and typedef = _typedef placed

    and derivation = { (* Used to rewrite the ast *)
        name: type_variable;
        cargs: component_expr list;
        targs: main_type list; 
        eargs: expr list;    
    } 

    and _term =
        | EmptyTerm
        | Comments of comments

        (* Dynamic part*)
        | Stmt of stmt

        (** Structure part*)
        | Component of component_dcl
        | Function of function_dcl

        (* Static part*)
        (*TODO | SignatureDcl of signature_dcl*)   
        | Typealias of type_variable * _typealias_body
        | Typedef of typedef 
        | Derive of derivation (* Used to rewrite the ast *)
    and term = _term placed

    and program = term list

    (* The following annotation requests the automatic generation of a [show_]
        function for each of the types defined above.*)
    [@@deriving show { with_path = false }]

    type 'a sig_expr_collector = (Atom.atom option -> Atom.Set.t -> expr -> 'a list)
    
    module type IR_utils_sig = sig 
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
            Atom.Set.t -> expr -> 'a list) ->
            _component_dcl AstUtils.placed ->
            Atom.Set.t * 'a list *
            (main_type * expr_variable) list
        val collect_expr_program : Atom.Set.t ->
            (_expr -> bool) ->
            'a sig_expr_collector ->
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

        val collect_cexpr_program : 
            Atom.Set.t ->
            (_component_expr -> bool) ->
            (Atom.atom option -> Atom.Set.t -> Error.place -> _component_expr -> 'a list) ->
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
    end


    module IRUtils : IR_utils_sig = struct
        
        (******************************************************************)

        let rec collect_expr_contract_ parent_opt already_binded selector collector place _contract = 
            let inner_already_binded = List.fold_left (fun already_binded (mt, x, e) ->
                Atom.Set.add x already_binded
            ) already_binded _contract.pre_binders in
            let res = List.map (function (_, _, e) -> collect_expr_expr parent_opt already_binded selector collector e) _contract.pre_binders in
            let collected_elts1 = List.flatten (List.map (function (_,x,_) -> x) res) in
            let fvars1 = List.flatten (List.map (function (_,_,x) -> x) res) in

            let _, collected_elts2, fvars2 = 
            match _contract.ensures with
            | None -> already_binded, [], []
            | Some ensures -> collect_expr_expr parent_opt already_binded selector collector ensures 
            in

            let _, collected_elts3, fvars3 = 
            match _contract.returns with
            | None -> already_binded, [], []
            | Some returns -> collect_expr_expr parent_opt already_binded selector collector returns 
            in

            already_binded, collected_elts1@collected_elts2@collected_elts3, fvars1@fvars2@fvars3

        and collect_expr_contract parent_opt (already_binded:Atom.Set.t) selector collector c = 
            map0_place (collect_expr_contract_ parent_opt already_binded selector collector) c 
        and collect_expr_port_ parent_opt (already_binded:Atom.Set.t) selector collector place ((_port, _):_port * main_type ) =
            let _, collected_elts1, fvars1 = collect_expr_mtype parent_opt already_binded selector collector _port.expecting_st in
            let _, collected_elts2, fvars2 = collect_expr_expr parent_opt already_binded  selector collector _port.callback in
            already_binded, collected_elts1@collected_elts2, fvars1@fvars2
        and collect_expr_port parent_opt (already_binded:Atom.Set.t) selector collector p = 
            map0_place (collect_expr_port_ parent_opt already_binded selector collector) p

        and collect_expr_outport_ parent_opt (already_binded:Atom.Set.t) selector collector place ((_outport, _):_outport * main_type ) =
            already_binded, [],[] 
        and collect_expr_outport parent_opt (already_binded:Atom.Set.t) selector collector p = 
            map0_place (collect_expr_outport_ parent_opt already_binded selector collector) p

        and collect_expr_state_ parent_opt (already_binded:Atom.Set.t) selector collector place = function 
        | StateDcl sdcl -> 
            let _, collected_elts1, fvars1 = collect_expr_mtype parent_opt already_binded selector collector sdcl.type0 in
            let _, collected_elts2, fvars2 = collect_expr_state_dcl_body parent_opt already_binded selector collector sdcl.body
            in

            already_binded, collected_elts1@collected_elts2, fvars1@fvars2
        and collect_expr_state parent_opt (already_binded:Atom.Set.t) selector collector s = 
            map0_place (collect_expr_state_ parent_opt already_binded selector collector) s 

        and collect_expr_function_dcl_ parent_opt (already_binded:Atom.Set.t) selector collector place (m:_function_dcl) =
            let _, collected_elts1, fvars1 = collect_expr_mtype parent_opt already_binded selector collector m.ret_type in
            let _, collected_elts2, fvars2 = List.fold_left (fun (set, collected_elts0, fvars0) {value=mt, x} -> 
                let _, collected_elts, fvars = collect_expr_mtype parent_opt set selector collector mt in
                set, collected_elts0@collected_elts, fvars0@fvars
            ) (already_binded, [], []) m.args in

            let already_binded = Atom.Set.add m.name already_binded in (*rec support*)
            let already_binded = List.fold_left (fun set {value=_,x} -> Atom.Set.add x set) already_binded m.args in
            let _, res = collect_expr_custom_method0_body parent_opt already_binded selector collector m.body in
            let collected_elts3 = List.flatten (List.map fst res) in
            let fvars3 = List.flatten (List.map snd res) in

            already_binded, collected_elts1@collected_elts2@collected_elts3, fvars1@fvars2@fvars3
        and collect_expr_function_dcl parent_opt (already_binded:Atom.Set.t) selector collector (fdcl:function_dcl) = 
            map0_place (collect_expr_function_dcl_ parent_opt already_binded selector collector) fdcl

        and collect_expr_method0_ parent_opt (already_binded:Atom.Set.t) selector collector place (m:_method0) =
            let _, collected_elts1, fvars1 = collect_expr_function_dcl_ parent_opt already_binded selector collector place ({
                name        = m.name;
                targs       = [];
                ret_type    = m.ret_type;
                args        = m.args;
                body        = m.body;
            }:_function_dcl) in 
            let _, collected_elts4, fvars4 = match m.contract_opt with
                | Some c -> collect_expr_contract parent_opt already_binded selector collector c
                | None -> already_binded, [],[]
            in
            already_binded, collected_elts1@collected_elts4, fvars1@fvars4
        and collect_expr_method0 parent_opt (already_binded:Atom.Set.t) selector collector m = 
            map0_place (collect_expr_method0_ parent_opt already_binded selector collector) m 
        and collect_expr_component_item_ parent_opt (already_binded:Atom.Set.t) selector (collector: 'a sig_expr_collector) place = function 
            | Contract c -> collect_expr_contract parent_opt already_binded selector collector c
            | Method m -> collect_expr_method0 parent_opt already_binded selector collector m
            | State s -> collect_expr_state parent_opt already_binded selector collector s 
            | Inport p  -> collect_expr_port parent_opt already_binded selector collector p
            | Outport p  -> collect_expr_outport parent_opt already_binded selector collector p
            | Term t -> collect_expr_term  parent_opt already_binded selector collector t    
        and collect_expr_component_item parent_opt (already_binded:Atom.Set.t) selector collector citem =              
            map0_place (collect_expr_component_item_ parent_opt already_binded selector collector) citem

        and free_vars_component_item already_binded citem = 
            let already_binded, _, fvars = collect_expr_component_item None  already_binded (function e -> false) (fun parent_opt env e -> []) citem in
            already_binded, Utils.deduplicate snd fvars 

        and collect_expr_component_dcl_ parent_opt (already_binded:Atom.Set.t) selector (collector: 'a sig_expr_collector) place =
        function 
        | ComponentAssign {name; value} ->
            (* TODO write collect expr_cexpr
            let _, collected_elts, fvars = collect_expr_cexpr parent_opt already_binded selector collector in 
            (Atom.Set.add name already_binded), collected_elts, fvars
            *)
            already_binded, [], []
        | ComponentStructure cdcl ->
            let parent_opt = Some cdcl.name in
            (* FIXME TODO do i need to propagate field/method name binding ???*)

            (* Shallow scan because fields and methods could be recursive *)
            let already_binded = List.fold_left (
                fun already_binded citem -> 
                    match citem.value with
                    | Contract _ -> already_binded
                    | Method m -> Atom.Set.add m.value.name already_binded
                    | State s -> 
                        Atom.Set.add(match s.value with 
                        | StateDcl s -> s.name
                    ) already_binded
                    |Inport p -> Atom.Set.add (fst p.value).name already_binded
                    | Outport p -> Atom.Set.add (fst p.value).name already_binded
                    | Term t -> already_binded
            ) already_binded cdcl.body in

            let _, res = List.fold_left_map (fun already_binded citem -> 
                let env, a,b = collect_expr_component_item parent_opt already_binded selector collector citem in
                env, (a,b)    
            ) already_binded cdcl.body in
            let collected_elts = List.flatten (List.map fst res) in
            let fvars = List.flatten (List.map snd res) in
            already_binded, collected_elts, fvars
        and collect_expr_component_dcl parent_opt (already_binded:Atom.Set.t) selector collector  cdcl = 
            map0_place (collect_expr_component_dcl_ parent_opt already_binded selector collector ) cdcl

        and free_vars_component_dcl already_binded cdcl = 
            let already_binded, _, fvars = collect_expr_component_dcl None  already_binded (function e -> false) (fun parent_opt env e -> []) cdcl in
            already_binded, Utils.deduplicate snd fvars

        and collect_expr_typedef_ parent_opt (already_binded:Atom.Set.t) selector (collector: 'a sig_expr_collector) place = function 
        (* already binded left unchanged since it is type binder *)
        | ClassicalDef  (x, targs, body) -> already_binded, [], []
        | EventDef (x, targs, body) -> already_binded, [], []
        | ProtocolDef (x, mt) -> collect_expr_mtype parent_opt already_binded selector collector mt
        | VPlaceDef x -> already_binded, [], []
        and collect_expr_typedef parent_opt (already_binded:Atom.Set.t) selector collector tdef= 
            map0_place (collect_expr_typedef_ parent_opt already_binded selector collector) tdef

        and collect_expr_derivation parent_opt (already_binded:Atom.Set.t) selector (collector: 'a sig_expr_collector) place (derive : derivation) =
            let _, tmp1 = [], [] in (*List.fold_left_map (collect_expr_cexpr parent_opt already_binded selector collector) derive.cargs  in*)
            let _, tmp2 = List.fold_left_map (fun already_binded truc -> 
                let env, a,b = collect_expr_mtype parent_opt already_binded selector collector truc in
                env, (a,b)    
            ) already_binded derive.targs in
            let _, tmp3 = List.fold_left_map (fun already_binded truc -> 
                let env, a,b =  collect_expr_expr parent_opt already_binded selector collector truc in
                env, (a,b)    
            ) already_binded derive.eargs  in
            let res = tmp1@tmp2@tmp3 in

            let collected_elts = List.flatten (List.map fst res) in
            let fvars = List.flatten (List.map snd res) in
            already_binded, collected_elts, fvars

        and collect_expr_term_ parent_opt (already_binded:Atom.Set.t) selector (collector: 'a sig_expr_collector) place = 
        function 
            | EmptyTerm | Comments _ -> already_binded, [], []
            | Stmt stmt -> collect_expr_stmt parent_opt already_binded selector collector stmt
            | Component cdcl -> collect_expr_component_dcl parent_opt already_binded selector collector cdcl
            | Function fdcl -> collect_expr_function_dcl parent_opt already_binded selector collector fdcl
            | Typealias _ -> already_binded, [], [] (* type binder but not an expr binder so already_binded is left unchanged*)
            | Typedef typedef -> collect_expr_typedef parent_opt already_binded selector collector typedef
            | Derive derive ->  collect_expr_derivation parent_opt already_binded selector collector place derive 
        and collect_expr_term parent_opt (already_binded:Atom.Set.t) selector (collector:'a sig_expr_collector) t = 
            map0_place (collect_expr_term_ parent_opt already_binded selector collector) t

        let rec collect_expr_program already_binded selector collector program = 
            let _, res = List.fold_left_map (fun already_binded term -> 
                let env, a,b = collect_expr_term None already_binded selector collector term in
                env, (a,b)    
            ) already_binded program in
            let collected_elts = List.flatten (List.map fst res) in
            let fvars = List.flatten (List.map snd res) in
            already_binded, collected_elts, fvars

        and free_vars_term already_binded citem = 
            let already_binded, _, fvars = collect_expr_term None  already_binded (function e -> false) (fun parent_opt env e -> []) citem in
            already_binded, Utils.deduplicate snd fvars 


        and free_vars_program already_binded program = 
            let already_binded, _, fvars = collect_expr_program  already_binded (function e -> false) (fun parent_opt env e -> []) program in
            already_binded, Utils.deduplicate snd fvars 


        (******************************************************************)
        let rec collect_cexpr_contract_ parent_opt already_binded selector collector place _contract = 
            let collected_elts2 = 
            match _contract.ensures with
            | None -> []
            | Some ensures -> collect_cexpr_expr parent_opt  already_binded selector collector ensures 
            in

            let collected_elts3 = 
            match _contract.returns with
            | None -> []
            | Some returns -> collect_cexpr_expr parent_opt  already_binded selector collector returns 
            in

            collected_elts2@collected_elts3

        and collect_cexpr_contract parent_opt already_binded selector collector c = 
            map0_place (collect_cexpr_contract_ parent_opt already_binded selector collector) c 

        and collect_cexpr_inport_ parent_opt already_binded selector collector place ((_inport, _):_port*'a) =
            let collected_elts1 = collect_cexpr_expr parent_opt already_binded selector collector _inport.callback in
            collected_elts1
        and collect_cexpr_inport parent_opt already_binded selector collector p = 
            map0_place (collect_cexpr_inport_ parent_opt already_binded selector collector) p

        and collect_cexpr_outport_ parent_opt already_binded selector collector place ((_outport, _):_outport*'a) =
            []
        and collect_cexpr_outport parent_opt already_binded selector collector p = 
            map0_place (collect_cexpr_outport_ parent_opt already_binded selector collector) p

        and collect_cexpr_state_ parent_opt already_binded selector collector place = function 
        | StateDcl sdcl -> begin
            let _, collected_elts, _ = collect_cexpr_state_dcl_body parent_opt already_binded selector collector sdcl.body in
            collected_elts
        end
        and collect_cexpr_state parent_opt already_binded selector collector s = 
            map0_place (collect_cexpr_state_ parent_opt  already_binded selector collector) s 

        and collect_cexpr_function_dcl_ parent_opt already_binded selector collector place (m:_function_dcl) =
            let _, collected_elts, _ = collect_cexpr_custom_method0_body parent_opt already_binded selector collector m.body in
            collected_elts

        and collect_cexpr_function_dcl parent_opt already_binded selector collector fdcl = 
            map0_place (collect_cexpr_function_dcl_ parent_opt already_binded selector collector) fdcl

        and collect_cexpr_method0_ parent_opt already_binded(selector:_component_expr->bool) collector place (m:_method0) =
            let collected_elts1 = collect_cexpr_function_dcl_ parent_opt already_binded selector collector place ({
                name        = m.name;
                targs       = [];
                ret_type    = m.ret_type;
                args        = m.args;
                body        = m.body;
            }:_function_dcl) in 
            let collected_elts4 = match m.contract_opt with
                | Some c -> collect_cexpr_contract parent_opt already_binded selector collector c
                | None -> []
            in
            collected_elts1@collected_elts4
        and collect_cexpr_method0 parent_opt already_binded selector collector m = 
            map0_place (collect_cexpr_method0_ parent_opt  already_binded selector collector) m 
        and collect_cexpr_component_item_ parent_opt already_binded selector collector place = function 
            | Contract c -> collect_cexpr_contract parent_opt  already_binded selector collector c
            | Method m -> collect_cexpr_method0 parent_opt  already_binded selector collector m
            | State s -> collect_cexpr_state parent_opt  already_binded selector collector s 
            | Inport p  -> collect_cexpr_inport parent_opt  already_binded selector collector p
            | Outport p  -> collect_cexpr_outport parent_opt  already_binded selector collector p
            | Term t -> collect_cexpr_term  parent_opt  already_binded selector collector t    
        and collect_cexpr_component_item parent_opt already_binded selector collector citem =              
            map0_place (collect_cexpr_component_item_ parent_opt already_binded selector collector) citem

        and collect_cexpr_component_dcl_ parent_opt already_binded selector collector place = function 
        | ComponentStructure cdcl ->
            let parent_opt = Some cdcl.name in

            List.flatten (List.map (collect_cexpr_component_item parent_opt  already_binded selector collector) cdcl.body)
        and collect_cexpr_component_dcl parent_opt already_binded selector collector cdcl = 
            map0_place (collect_cexpr_component_dcl_ parent_opt  already_binded selector collector ) cdcl
        and collect_cexpr_typedef_ parent_opt already_binded selector collector place = function 
        (* already binded left unchanged since it is type binder *)
        | ClassicalDef  (x, targs, body) -> []
        | EventDef (x, targs, body) -> []
        | ProtocolDef (x, mt) -> []
        | VPlaceDef x -> []
        and collect_cexpr_typedef parent_opt already_binded selector collector tdef= 
            map0_place (collect_cexpr_typedef_ parent_opt  already_binded selector collector) tdef

        and collect_cexpr_derivation parent_opt already_binded selector collector place derive =
            let tmp1 = List.flatten (List.map (collect_cexpr_cexpr parent_opt  already_binded selector collector) derive.cargs) in
            let tmp3 = List.flatten (List.map (collect_cexpr_expr parent_opt  already_binded selector collector)  derive.eargs) in
            tmp1@tmp3

        and collect_cexpr_term_ parent_opt already_binded selector collector place = function 
            | EmptyTerm | Comments _ -> []
            | Stmt stmt -> collect_cexpr_stmt parent_opt  already_binded selector collector stmt
            | Component cdcl -> collect_cexpr_component_dcl parent_opt  already_binded selector collector cdcl
            | Function fdcl -> collect_cexpr_function_dcl parent_opt  already_binded selector collector fdcl
            | Typealias _ -> [] (* type binder but not an cexpr binder so  is left unchanged*)
            | Typedef typedef -> collect_cexpr_typedef parent_opt  already_binded selector collector typedef
            | Derive derive ->  collect_cexpr_derivation parent_opt  already_binded selector collector place derive 
        and collect_cexpr_term parent_opt already_binded selector collector t = 
            map0_place (collect_cexpr_term_ parent_opt  already_binded selector collector) t

        and collect_cexpr_program  already_binded selector collector program = 
            List.flatten (List.map (collect_cexpr_term None  already_binded selector collector) program)

        (******************************************************************)
        let rec collect_stmt_contract_ parent_opt selector collector place _contract = []
        and collect_stmt_contract parent_opt selector collector c = 
            map0_place (collect_stmt_contract_ parent_opt  selector collector) c 

        and collect_stmt_inport_ parent_opt selector collector place (_port, _) = []
        and collect_stmt_inport parent_opt selector collector p = 
            map0_place (collect_stmt_inport_ parent_opt  selector collector) p

        and collect_stmt_outport_ parent_opt selector collector place (_port, _) = []
        and collect_stmt_outport parent_opt selector collector p = 
            map0_place (collect_stmt_outport_ parent_opt  selector collector) p

        and collect_stmt_state_ parent_opt selector collector place s = [] 
        and collect_stmt_state parent_opt selector collector s = 
            map0_place (collect_stmt_state_ parent_opt  selector collector) s 

        and collect_stmt_function_dcl_ parent_opt selector collector place (m:_function_dcl) =
            collect_stmt_custom_method0_body parent_opt selector collector m.body

        and collect_stmt_function_dcl parent_opt selector collector fdcl = 
            map0_place (collect_stmt_function_dcl_ parent_opt  selector collector) fdcl

        and collect_stmt_method0_ parent_opt selector collector place (m:_method0) =
            let collected_elts1 = collect_stmt_function_dcl_ parent_opt  selector collector place ({
                name        = m.name;
                targs       = [];
                ret_type    = m.ret_type;
                args        = m.args;
                body        = m.body;
            }:_function_dcl) in 
            let collected_elts4 = match m.contract_opt with
                | Some c -> collect_stmt_contract parent_opt  selector collector c
                | None -> []
            in
            collected_elts1@collected_elts4
        and collect_stmt_method0 parent_opt selector collector m = 
            map0_place (collect_stmt_method0_ parent_opt  selector collector) m 
        and collect_stmt_component_item_ parent_opt selector collector place = function 
            | Contract c    -> collect_stmt_contract parent_opt selector collector c
            | Method m      -> collect_stmt_method0 parent_opt  selector collector m
            | State s       -> collect_stmt_state parent_opt selector collector s 
            | Inport p      -> collect_stmt_inport parent_opt selector collector p
            | Outport p     -> collect_stmt_outport parent_opt selector collector p
            | Term t        -> collect_stmt_term parent_opt  selector collector t    
        and collect_stmt_component_item parent_opt selector collector citem =              
            map0_place (collect_stmt_component_item_ parent_opt  selector collector) citem

        and collect_stmt_component_dcl_ parent_opt selector collector place = function 
        | ComponentStructure cdcl ->
            let parent_opt = Some cdcl.name in

            List.flatten (List.map (collect_stmt_component_item parent_opt  selector collector) cdcl.body)
        | ComponentAssign _ -> []
        and collect_stmt_component_dcl parent_opt selector collector cdcl = 
            map0_place (collect_stmt_component_dcl_ parent_opt  selector collector ) cdcl
        and collect_stmt_typedef_ parent_opt selector collector place = function 
        (* already binded left unchanged since it is type binder *)
        | ClassicalDef  (x, targs, body) -> []
        | EventDef (x, targs, body) -> []
        | ProtocolDef (x, mt) -> []
        | VPlaceDef x -> []
        and collect_stmt_typedef parent_opt selector collector tdef= 
            map0_place (collect_stmt_typedef_ parent_opt  selector collector) tdef

        and collect_stmt_derivation parent_opt selector collector place derive = []

        and collect_stmt_term_ parent_opt selector collector place = function 
            | EmptyTerm | Comments _ -> []
            | Stmt stmt -> collect_stmt_stmt parent_opt  selector collector stmt
            | Component cdcl -> collect_stmt_component_dcl parent_opt  selector collector cdcl
            | Function fdcl -> collect_stmt_function_dcl parent_opt  selector collector fdcl
            | Typealias _ -> [] (* type binder but not an stmt binder so  is left unchanged*)
            | Typedef typedef -> collect_stmt_typedef parent_opt  selector collector typedef
            | Derive derive ->  collect_stmt_derivation parent_opt  selector collector place derive 
        and collect_stmt_term parent_opt selector collector t = 
            map0_place (collect_stmt_term_ parent_opt  selector collector) t

        and collect_stmt_program  selector collector program = 
            List.flatten (List.map (collect_stmt_term None  selector collector) program)
        (******************************************************************)


        let rec collect_type_contract_ parent_opt (already_binded:Atom.Set.t) selector collector place _contract = 
            let inner_already_binded = List.fold_left (fun already_binded (mt, x, e) ->
                Atom.Set.add x already_binded
            ) already_binded _contract.pre_binders in
            let res = List.map (function (_, _, e) -> collect_type_expr parent_opt already_binded selector collector e) _contract.pre_binders in
            let collected_elts1 = List.flatten (List.map (function (_,x,_) -> x) res) in
            let fvars1 = List.flatten (List.map (function (_,_,x) -> x) res) in

            let _, collected_elts2, fvars2 = 
            match _contract.ensures with
            | None -> already_binded, [], []
            | Some ensures -> collect_type_expr parent_opt already_binded selector collector ensures 
            in

            let _, collected_elts3, fvars3 = 
            match _contract.returns with
            | None -> already_binded, [], []
            | Some returns -> collect_type_expr parent_opt already_binded selector collector returns 
            in

            already_binded, collected_elts1@collected_elts2@collected_elts3, fvars1@fvars2@fvars3

        and collect_type_contract parent_opt (already_binded:Atom.Set.t) selector collector c = 
            map0_place (collect_type_contract_ parent_opt already_binded selector collector) c 
        and collect_type_port_ parent_opt (already_binded:Atom.Set.t) selector collector place ((_port, _):_port*'a) =
            let _, collected_elts1, fvars1 = collect_type_mtype parent_opt already_binded selector collector _port.expecting_st in
            let _, collected_elts2, fvars2 = collect_type_expr parent_opt already_binded  selector collector _port.callback in
            already_binded, collected_elts1@collected_elts2, fvars1@fvars2
        and collect_type_port parent_opt (already_binded:Atom.Set.t) selector collector p = 
            map0_place (collect_type_port_ parent_opt already_binded selector collector) p

        and collect_type_outport_ parent_opt (already_binded:Atom.Set.t) selector collector place (_outport, _) =
            already_binded, [], [] 
        and collect_type_outport parent_opt (already_binded:Atom.Set.t) selector collector p = 
            map0_place (collect_type_outport_ parent_opt already_binded selector collector) p

        and collect_type_state_ parent_opt (already_binded:Atom.Set.t) selector collector place = function 
        | StateDcl sdcl -> 
            let _, collected_elts1, fvars1 = collect_type_mtype parent_opt already_binded selector collector sdcl.type0 in
            let _, collected_elts2, fvars2 = collect_type_state_dcl_body parent_opt already_binded selector collector sdcl.body in

            already_binded, collected_elts1@collected_elts2, fvars1@fvars2
        and collect_type_state parent_opt (already_binded:Atom.Set.t) selector collector s = 
            map0_place (collect_type_state_ parent_opt already_binded selector collector) s 

        and collect_type_function_dcl_ parent_opt (already_binded:Atom.Set.t) selector collector place (m:_function_dcl) =
            (* A function can not bind type new type - except internally if there this is a generic function *)
            let already_binded_generic_tvars = Atom.Set.of_seq (List.to_seq m.targs) in
            let inner_already_binded = Atom.Set.union already_binded already_binded_generic_tvars in

            let _, collected_elts1, ftvars1 = collect_type_mtype parent_opt inner_already_binded selector collector m.ret_type in
            let _, collected_elts2, ftvars2 = List.fold_left (fun (set, collected_elts0, ftvars0) {value=mt, x} -> 
                let _, collected_elts, ftvars = collect_type_mtype parent_opt set selector collector mt in
                set, collected_elts0@collected_elts, ftvars0@ftvars
            ) (inner_already_binded, [], []) m.args in

            let _, res = collect_type_custom_method0_body parent_opt inner_already_binded selector collector m.body  in
            let collected_elts3 = List.flatten (List.map fst res) in
            let ftvars3 = List.flatten (List.map snd res) in

            if List.exists (function x -> Atom.value x = "Interceptor") (ftvars1@ftvars2@ftvars3) then
                logger#error "%s" (Atom.to_string m.name);

            already_binded, collected_elts1@collected_elts2@collected_elts3, ftvars1@ftvars2@ftvars3
        and collect_type_function_dcl parent_opt (already_binded:Atom.Set.t) selector collector fdcl = 
            map0_place (collect_type_function_dcl_ parent_opt already_binded selector collector) fdcl

        and collect_type_method0_ parent_opt (already_binded:Atom.Set.t) selector collector place (m:_method0) =
            let _, collected_elts1, fvars1 = collect_type_function_dcl_ parent_opt already_binded selector collector place {
                name        = m.name;
                targs       = [];
                ret_type    = m.ret_type;
                args        = m.args;
                body        = m.body;
            } in 
            let _, collected_elts4, fvars4 = match m.contract_opt with
                | Some c -> collect_type_contract parent_opt already_binded selector collector c
                | None -> already_binded, [],[]
            in
            already_binded, collected_elts1@collected_elts4, fvars1@fvars4
        and collect_type_method0 parent_opt (already_binded:Atom.Set.t) selector collector m = 
            map0_place (collect_type_method0_ parent_opt already_binded selector collector) m 
        and collect_type_component_item_ parent_opt (already_binded:Atom.Set.t) selector collector place = function 
            | Contract c -> collect_type_contract parent_opt already_binded selector collector c
            | Method m -> collect_type_method0 parent_opt already_binded selector collector m
            | State s -> collect_type_state parent_opt already_binded selector collector s 
            | Inport p  -> collect_type_port parent_opt already_binded selector collector p
            | Outport p  -> collect_type_outport parent_opt already_binded selector collector p
            | Term t -> collect_type_term  parent_opt already_binded selector collector t    
        and collect_type_component_item parent_opt (already_binded:Atom.Set.t) selector collector citem =              
            map0_place (collect_type_component_item_ parent_opt already_binded selector collector) citem

        and free_tvars_component_item already_binded citem = 
            let already_binded, _, ftvars = collect_type_component_item None  already_binded (function e -> false) (fun parent_opt env e -> []) citem in
            already_binded, Utils.deduplicate Fun.id ftvars 

        and collect_type_component_dcl_ parent_opt (already_binded:Atom.Set.t) selector collector place = function 
        | ComponentAssign {name; value} ->
            (*
            TODO ?? if so do the same for component structure
            let already_binded = Atom.Set.add name already_binded in
            *)
            let _, collected_elts, ftvars = collect_type_cexpr parent_opt already_binded selector collector value in
            already_binded, collected_elts, ftvars 
        | ComponentStructure cdcl ->
            let parent_opt = Some cdcl.name in
            (* FIXME TODO do i need to propagate field/method name binding ???*)

            (* Shallow scan because because component type def could be recursive *)
            let already_binded = List.fold_left (
                fun already_binded citem -> 
                    match citem.value with
                    | Contract _ -> already_binded
                    | Method m -> already_binded
                    | State s -> already_binded
                    | Inport p -> already_binded
                    | Outport p -> already_binded
                    | Term {value=Component {value=ComponentStructure {name}}} -> Atom.Set.add name already_binded
                    | Term _ -> already_binded
            ) already_binded cdcl.body in

            let _, res = List.fold_left_map (fun already_binded citem -> 
                let env, a,b = collect_type_component_item parent_opt already_binded selector collector citem in
                env, (a,b)    
            ) already_binded cdcl.body in
            let collected_elts = List.flatten (List.map fst res) in
            let fvars = List.flatten (List.map snd res) in
            already_binded, collected_elts, fvars
        and collect_type_component_dcl parent_opt (already_binded:Atom.Set.t) selector collector cdcl = 
            map0_place (collect_type_component_dcl_ parent_opt already_binded selector collector ) cdcl

        and free_tvars_component_dcl already_binded cdcl = 
            let already_binded, _, ftvars = collect_type_component_dcl None  already_binded (function e -> false) (fun parent_opt env e -> []) cdcl in
            already_binded, Utils.deduplicate Fun.id ftvars 

        and collect_type_typedef_ parent_opt (already_binded:Atom.Set.t) selector collector place = 
            let collect_mtypes mtypes = 
                List.fold_left (fun (acc0, acc1) mtype -> 
                    let _, collected_elts, ftvars = collect_type_mtype parent_opt already_binded selector collector mtype in
                    collected_elts@acc0, ftvars@acc1
                ) ([], []) mtypes 
            in

            function 
            (* already binded left unchanged since it is type binder *)
            | ClassicalDef  (x, targs, body) | EventDef (x, targs, body) ->
                let collected_elts, ftvars = collect_mtypes targs in
                Atom.Set.add x already_binded, collected_elts, ftvars 
            | ProtocolDef (x, mt) -> 
                let _, collected_elts, ftvars = collect_type_mtype parent_opt already_binded selector collector mt in
                Atom.Set.add x already_binded, collected_elts, ftvars
            | VPlaceDef x ->
                Atom.Set.add x already_binded, [], []
        and collect_type_typedef parent_opt (already_binded:Atom.Set.t) selector collector tdef= 
            map0_place (collect_type_typedef_ parent_opt already_binded selector collector) tdef

        and collect_type_derivation parent_opt (already_binded:Atom.Set.t) selector collector place derive =
            let _, tmp1 = List.fold_left_map (fun already_binded ce ->          
                let env, a, b = collect_type_cexpr parent_opt already_binded selector collector ce in
                env, (a,b)
            ) already_binded derive.cargs  in
            let _, tmp2 = List.fold_left_map (fun already_binded truc -> 
                let env, a,b = collect_type_mtype parent_opt already_binded selector collector truc in
                env, (a,b)    
            ) already_binded derive.targs in
            let _, tmp3 = List.fold_left_map (fun already_binded truc -> 
                let env, a,b =  collect_type_expr parent_opt already_binded selector collector truc in
                env, (a,b)    
            ) already_binded derive.eargs  in
            let res = tmp1@tmp2@tmp3 in

            let collected_elts = List.flatten (List.map fst res) in
            let fvars = List.flatten (List.map snd res) in
            already_binded, collected_elts, fvars

        (*
            recursive = true means that even if a term is selected, its sub-terms could be selector also. 
                = false - when a term is selected, sub-terms are ignored
        *)
        and collect_type_term_ parent_opt (already_binded:Atom.Set.t) selector collector place = function 
            | EmptyTerm | Comments _ -> already_binded, [], []
            | Stmt stmt -> collect_type_stmt parent_opt already_binded selector collector stmt
            | Component cdcl -> collect_type_component_dcl parent_opt already_binded selector collector cdcl
            | Function fdcl -> collect_type_function_dcl parent_opt already_binded selector collector fdcl
            | Typealias _ -> already_binded, [], [] (* type binder but not an expr binder so already_binded is left unchanged*)
            | Typedef typedef -> collect_type_typedef parent_opt already_binded selector collector typedef
            | Derive derive ->  collect_type_derivation parent_opt already_binded selector collector place derive 
        and collect_type_term parent_opt (already_binded:Atom.Set.t) selector collector t = 
            map0_place (collect_type_term_ parent_opt already_binded selector collector) t

        and collect_type_program already_binded selector collector program = 
            (* Shallow scan because because component type def could be recursive *)
            let already_binded = List.fold_left (
                fun already_binded term -> 
                    match term.value with
                    | Component {value=ComponentStructure {name}} -> Atom.Set.add name already_binded
                    | _ -> already_binded
            ) already_binded program in

            let _, res = List.fold_left_map (fun already_binded term -> 
                let env, a,b = collect_type_term None already_binded selector collector term in
                env, (a,b)    
            ) already_binded program in
            let collected_elts = List.flatten (List.map fst res) in
            let fvars = List.flatten (List.map snd res) in
            already_binded, collected_elts, fvars

        and free_tvars_term already_binded citem = 
            let already_binded, _, ftvars = collect_type_term None  already_binded (function e -> false) (fun parent_opt env e -> []) citem in
            already_binded, Utils.deduplicate Fun.id ftvars 


        and free_tvars_program already_binded program = 
            let already_binded, _, ftvars = collect_type_program  already_binded (function e -> false) (fun parent_opt env e -> []) program in
            already_binded, Utils.deduplicate Fun.id ftvars 

        (*****************************************************************)

        let collect_stype_program already_binded selector collector program = 
            collect_type_program already_binded 
            (function | SType st -> true | _-> false) 
            (fun parent_opt already_binded {place; value=SType st} -> 
                let _, elts, _ = collect_stype_stype parent_opt already_binded selector collector st in
                elts
            ) 

        (*****************************************************************)

        let rec rewrite_type_contract_ selector rewriter place _contract =
            let rewrite_expr = rewrite_type_expr selector rewriter in
            let rewrite_mtype = rewrite_type_mtype selector rewriter in
            {_contract with  
                pre_binders = List.map (function (mt, x, e) -> (
                    rewrite_mtype mt, x, rewrite_expr e)) _contract.pre_binders;
                ensures = Option.map rewrite_expr _contract.ensures;
                returns = Option.map rewrite_expr _contract.returns;
            }
        and rewrite_type_contract selector rewriter = map_place (rewrite_type_contract_ selector rewriter) 

        and rewrite_type_port_  selector rewriter place ((_port, mt): _port*main_type) =
            let rewrite_expr = rewrite_type_expr selector rewriter in
            let rewrite_mtype = rewrite_type_mtype selector rewriter in
            ({ _port with
                expecting_st  = rewrite_mtype _port.expecting_st;
                callback = rewrite_expr _port.callback; 
            }, mt)
            
        and rewrite_type_port selector rewriter = map_place (rewrite_type_port_ selector rewriter) 

        and rewrite_type_outport_  selector rewriter place (({name}, mt): _outport * main_type) =
            ({name}, mt)
            
        and rewrite_type_outport selector rewriter = map_place (rewrite_type_outport_ selector rewriter) 

        and rewrite_type_state_  selector rewriter place = function 
        | StateDcl sdcl -> StateDcl {
            sdcl with 
                type0 = rewrite_type_mtype selector rewriter sdcl.type0;
                body = rewrite_type_state_dcl_body selector rewriter sdcl.body;
        }
        and rewrite_type_state selector rewriter = map_place (rewrite_type_state_ selector rewriter) 

        and rewrite_type_function_dcl_  selector rewriter place (m:_function_dcl) =
            let rewrite_expr = rewrite_type_expr selector rewriter in
            let rewrite_mtype = rewrite_type_mtype selector rewriter in
            { m with 
                ret_type = rewrite_mtype m.ret_type;
                args = List.map (function {place; value=(mt,x)} -> 
                    {place; value=rewrite_mtype mt, x}
                ) m.args;
                body = rewrite_type_custom_method0_body selector rewriter m.body 
            }
        and rewrite_type_function_dcl selector rewriter = map_place (rewrite_type_function_dcl_ selector rewriter) 

        and rewrite_type_method0_  selector rewriter place (m:_method0) =
            let rewrite_expr = rewrite_type_expr selector rewriter in
            let rewrite_mtype = rewrite_type_mtype selector rewriter in
            { m with 
                ret_type = rewrite_mtype m.ret_type;
                args = List.map (function {place; value=(mt,x)} -> 
                    {place; value=rewrite_mtype mt, x}
                ) m.args;
                body = rewrite_type_custom_method0_body selector rewriter m.body;
                contract_opt = Option.map (rewrite_type_contract selector rewriter) m.contract_opt;
            }
        and rewrite_type_method0 selector rewriter = map_place (rewrite_type_method0_ selector rewriter) 

        and rewrite_type_component_item_  selector rewriter place = function 
            | Contract c -> Contract (rewrite_type_contract selector rewriter c)
            | Method m -> Method (rewrite_type_method0 selector rewriter m)
            | State s -> State (rewrite_type_state selector rewriter s )
            | Inport p  ->Inport (rewrite_type_port selector rewriter p)
            | Outport p  -> Outport (rewrite_type_outport selector rewriter p)
            | Term t -> Term (rewrite_type_term selector rewriter t)
        and rewrite_type_component_item selector rewriter = map_place (rewrite_type_component_item_ selector rewriter) 

        and rewrite_type_component_dcl_  selector rewriter place = function 
        | ComponentStructure cdcl -> 
            ComponentStructure { cdcl with body = List.map (rewrite_type_component_item selector rewriter) cdcl.body}
        and rewrite_type_component_dcl selector rewriter = map_place (rewrite_type_component_dcl_ selector rewriter) 

        and rewrite_type_term_ selector rewriter place = function 
        | EmptyTerm -> EmptyTerm 
        | Comments c -> Comments c
        | Stmt stmt -> Stmt (rewrite_type_stmt selector rewriter stmt)
        | Component cdcl -> Component (rewrite_type_component_dcl selector rewriter cdcl)
        | Function fdcl -> Function (rewrite_type_function_dcl selector rewriter fdcl)
        | (Typealias _ as t) |(Typedef _ as t) -> t
        | Derive derive -> Derive { derive with eargs = List.map (rewrite_type_expr selector rewriter) derive.eargs}
        and rewrite_type_term selector rewriter = map_place (rewrite_type_term_ selector rewriter) 
        and rewrite_type_program selector rewriter (program : program) : program = List.map (rewrite_type_term selector rewriter) program


        (*****************************************************************)

        let rewrite_stype_program selector rewriter program = 
            let fplace = (Error.forge_place "rewrite_stype_program" 0 0) in
            let auto_fplace smth = {place = fplace; value=smth} in
            rewrite_type_program 
            (function _ -> true) 
            (function mt -> (rewrite_stype_mtype selector rewriter (auto_fplace mt)).value) program

        (*****************************************************************)

        let rec rewrite_expr_contract_ selector rewriter place _contract =
            {_contract with  
                pre_binders = List.map (function (mt, x, e) -> (mt, x, rewrite_expr_expr selector rewriter e)) _contract.pre_binders; (*TODO replace for mt*)
                ensures = Option.map (rewrite_expr_expr selector rewriter) _contract.ensures;
                returns = Option.map (rewrite_expr_expr selector rewriter) _contract.returns;
            }
        and rewrite_expr_contract selector rewriter = map_place (rewrite_expr_contract_ selector rewriter) 

        and rewrite_expr_port_  selector rewriter place ((_port, mt): _port * main_type) =
            ({ _port with
                (* TODO on rewirte_expr_mtype => expecting_st = rewrite_expr_mtype selector rewriter expecting_st;*)
                callback = rewrite_expr_expr selector rewriter _port.callback; 
            }, mt)
            
        and rewrite_expr_port selector rewriter = map_place (rewrite_expr_port_ selector rewriter) 

        and rewrite_expr_outport_  selector rewriter place (({name}, mt): _outport * main_type) =
            ({name}, mt)
            
        and rewrite_expr_outport selector rewriter = map_place (rewrite_expr_outport_ selector rewriter) 

        and rewrite_expr_state_  selector rewriter place = function 
        | StateDcl sdcl -> StateDcl {
            sdcl with body = rewrite_expr_state_dcl_body selector rewriter sdcl.body;
        }
        and rewrite_expr_state selector rewriter = map_place (rewrite_expr_state_ selector rewriter) 

        and rewrite_expr_function_dcl_  selector rewriter place (m:_function_dcl) =
            { m with body = rewrite_expr_custom_method0_body selector rewriter m.body }
        and rewrite_expr_function_dcl selector rewriter = map_place (rewrite_expr_function_dcl_ selector rewriter) 

        and rewrite_expr_method0_  selector rewriter place (m:_method0) =
            { m with 
                body = rewrite_expr_custom_method0_body selector rewriter m.body;
                contract_opt = Option.map (rewrite_expr_contract selector rewriter) m.contract_opt;
            }
        and rewrite_expr_method0 selector rewriter = map_place (rewrite_expr_method0_ selector rewriter) 

        and rewrite_expr_component_item_  selector rewriter place = function 
            | Contract c -> Contract (rewrite_expr_contract selector rewriter c)
            | Method m -> Method (rewrite_expr_method0 selector rewriter m)
            | State s -> State (rewrite_expr_state selector rewriter s )
            |Inport p  ->Inport (rewrite_expr_port selector rewriter p)
            | Outport p  -> Outport (rewrite_expr_outport selector rewriter p)
            | Term t -> Term (rewrite_expr_term selector rewriter t)
        and rewrite_expr_component_item selector rewriter = map_place (rewrite_expr_component_item_ selector rewriter) 

        and rewrite_expr_component_dcl_  selector rewriter place = function 
        | ComponentStructure cdcl -> 
            ComponentStructure { cdcl with body = List.map (rewrite_expr_component_item selector rewriter) cdcl.body}
        and rewrite_expr_component_dcl selector rewriter = map_place (rewrite_expr_component_dcl_ selector rewriter) 

        and rewrite_expr_term_ selector rewriter place = function 
        | EmptyTerm -> EmptyTerm 
        | Comments c -> Comments c
        | Stmt stmt -> Stmt (rewrite_expr_stmt selector rewriter stmt)
        | Component cdcl -> Component (rewrite_expr_component_dcl selector rewriter cdcl)
        | Function fdcl -> Function (rewrite_expr_function_dcl selector rewriter fdcl)
        | (Typealias _ as t) |(Typedef _ as t) -> t
        | Derive derive -> Derive { derive with eargs = List.map (rewrite_expr_expr selector rewriter) derive.eargs}
        and rewrite_expr_term selector rewriter = map_place (rewrite_expr_term_ selector rewriter) 
        and rewrite_expr_program selector rewriter (program : program) : program = List.map (rewrite_expr_term selector rewriter) program

        let make x_to_replace ((replaceby_x_opt, replaceby_e_opt)as replaceby) = 
            let selector = function |VarExpr x when x = x_to_replace -> true | _ -> false in
            let rewriter e _ = match replaceby_x_opt with | Some x -> VarExpr x | None -> Option.get replaceby_e_opt in
            selector, rewriter
        let replace_expr_component_item x_to_replace replaceby = 
            let selector, rewriter = make x_to_replace replaceby in
            rewrite_expr_component_item selector rewriter

        (******************************************)

        let rec collect_term_component_item_ recursive parents selector collector place = function 
        | Term t -> collect_term_term recursive (List.rev parents) selector collector t
        | citem -> []
        and collect_term_component_item recursive parents selector collector = map0_place (collect_term_component_item_ recursive parents selector collector) 

        and collect_term_component_dcl_  recursive parents selector collector place = function 
        | ComponentStructure cdcl -> List.flatten (List.map (collect_term_component_item recursive (cdcl.name::parents) selector collector) cdcl.body)
        | x -> [] 
        and collect_term_component_dcl recursive parents selector collector = map0_place (collect_term_component_dcl_ recursive parents selector collector) 

        and collect_term_term_ recursive parents selector collector place = function 
        | t when selector t  -> 
            collector (List.rev parents) place t 
            @ (
                (* sub-terms *)
                if recursive then 
                    match t with 
                    | Component cdcl -> collect_term_component_dcl recursive parents selector collector cdcl
                    | _ -> []
                else []
            )
        | Component cdcl -> collect_term_component_dcl recursive parents selector collector cdcl
        | t -> [] 
        and collect_term_term recursive parents selector collector = map0_place (collect_term_term_ recursive parents selector collector) 

        (*
            recursive = true means that even if a term is selected, its sub-terms could be selector also. 
                = false - when a term is selected, sub-terms are ignored
        *)
        and collect_term_program recursive (selector : _term -> bool) (collector : Atom.atom list -> Error.place -> _term -> 'a list) program = List.flatten (List.map (collect_term_term recursive [] selector collector) program )
        (******************************************)
        let rec rewrite_term_component_item_  selector rewriter place = function 
        | Term t -> List.map (function x -> Term x) (rewrite_term_term selector rewriter t)
        | citem -> [citem]
        and rewrite_term_component_item selector rewriter = map_places (rewrite_term_component_item_ selector rewriter) 

        and rewrite_term_component_dcl_  selector rewriter place = function 
        | ComponentStructure cdcl -> ComponentStructure { cdcl with body = List.flatten (List.map (rewrite_term_component_item selector rewriter) cdcl.body)}
        | x -> x
        and rewrite_term_component_dcl selector rewriter = map_place (rewrite_term_component_dcl_ selector rewriter) 

        and rewrite_term_term_ selector rewriter place = function 
        | t when selector t -> rewriter place t
        | Component cdcl -> [Component (rewrite_term_component_dcl selector rewriter cdcl)]
        | t -> [ t ]
        and rewrite_term_term selector rewriter = map_places (rewrite_term_term_ selector rewriter) 

        and rewrite_term_program (selector : _term -> bool) (rewriter : Error.place -> _term -> _term list) program = List.flatten (List.map (rewrite_term_term selector rewriter) program )

        (******************************************)
        let rec rewrite_citem_component_item_  selector rewriter place = function 
        | t when selector t -> rewriter place t
        | Term t -> List.map (function x -> Term x) (rewrite_citem_term selector rewriter t)
        | citem -> [citem]
        and rewrite_citem_component_item selector rewriter = map_places (rewrite_citem_component_item_ selector rewriter) 

        and rewrite_citem_component_dcl_  selector rewriter place = function 
        | ComponentStructure cdcl -> ComponentStructure { cdcl with body = List.flatten (List.map (rewrite_citem_component_item selector rewriter) cdcl.body)}
        | x -> x
        and rewrite_citem_component_dcl selector rewriter = map_place (rewrite_citem_component_dcl_ selector rewriter) 

        and rewrite_citem_term_ selector rewriter place = function 
        | Component cdcl -> [Component (rewrite_citem_component_dcl selector rewriter cdcl)]
        | t -> [ t ]
        and rewrite_citem_term selector rewriter = map_places (rewrite_citem_term_ selector rewriter) 

        and rewrite_citem_program (selector : _component_item -> bool) (rewriter : Error.place -> _component_item -> _component_item list) program = List.flatten (List.map (rewrite_citem_term selector rewriter) program )

        let rewrite_component_program (selector : component_structure -> bool) (rewriter : Error.place -> component_structure -> component_structure list) program : term list = 
            let selector_t = function
            | Component {value=ComponentStructure cdcl} when selector cdcl -> true
            | _ -> false
            in
            let rewriter_t place = function
            | Component {value=ComponentStructure cdcl} -> List.map (function x -> Component {place; value=ComponentStructure x}) (rewriter place cdcl)
            in
            rewrite_term_program selector_t rewriter_t program


        let rec rewrite_scopeterm_component_item_  selector rewriter place = function 
        | Term t -> List.map (function x -> Term x) (rewrite_scopeterm_term selector rewriter t)
        | citem -> [citem]
        and rewrite_scopeterm_component_item selector rewriter = map_places (rewrite_scopeterm_component_item_ selector rewriter) 

        and rewrite_scopeterm_component_dcl_  selector rewriter place = function 
        | ComponentStructure cdcl -> 
            
            if List.exists (function |{value=Term t} -> selector t | _ -> false) cdcl.body then (* Scope of the searched term is the current component *)
                let scope = List.map (function | {value=Term t} -> t) (List.filter (function |{value=Term _} -> true | _ -> false) cdcl.body) in
                let remaining_body = List.filter (function |{value=Term _} -> false | _ -> true) cdcl.body in

                ComponentStructure { cdcl with body = (List.map (function t -> {place=t.place; value=Term t}) (rewriter scope)) @ remaining_body }
            else
                ComponentStructure { cdcl with body = List.flatten (List.map (rewrite_scopeterm_component_item selector rewriter) cdcl.body)}
        | x -> x
        and rewrite_scopeterm_component_dcl selector rewriter = map_place (rewrite_scopeterm_component_dcl_ selector rewriter) 

        and rewrite_scopeterm_term_ selector rewriter place = function 
        | Component cdcl -> [Component (rewrite_scopeterm_component_dcl selector rewriter cdcl)]
        | t -> [ t ]
        and rewrite_scopeterm_term selector rewriter = map_places (rewrite_scopeterm_term_ selector rewriter) 

        and rewrite_scopeterm_program (selector : term -> bool) (rewriter : term list -> term list) program = 
            if List.exists selector program then( (* Scope of the searched term is [Top-level]*)
                logger#debug "all program";
                rewriter program
            )else
                List.flatten (List.map (rewrite_scopeterm_term selector rewriter) program )


        (*
        expr -> stmts before * new_expr
        can only be applied in expression that are inside a statement
        *)
        let rec rewrite_exprstmts_expr_ parent_opt selector rewriter place (e, mt_e) : stmt list * (_expr * main_type) = 
            let rewrite_exprstmts_expr = rewrite_exprstmts_expr parent_opt selector rewriter in
            match e with  
            | _ when selector e -> rewriter parent_opt mt_e e
            | EmptyExpr | VarExpr _ -> [], (e,mt_e)
            | ActivationAccessExpr (cname, e, mname) ->
                let stmts, e = rewrite_exprstmts_expr e in
                stmts, (ActivationAccessExpr (cname, e, mname), mt_e)
            | AccessExpr (e1, e2) ->
                let stmts1, e1 = rewrite_exprstmts_expr e1 in
                let stmts2, e2 = rewrite_exprstmts_expr e2 in
                stmts1@stmts2, (AccessExpr (e1, e2), mt_e)
            | BinopExpr (e1, op, e2) ->
                let stmts1, e1 = rewrite_exprstmts_expr e1 in
                let stmts2, e2 = rewrite_exprstmts_expr e2 in
                stmts1@stmts2, (BinopExpr (e1, op, e2), mt_e)
            | (CallExpr (e1, es) as e) | (NewExpr (e1, es) as e)->
                let stmts1, e1 = rewrite_exprstmts_expr e1 in
                let stmtses = List.map (function e -> rewrite_exprstmts_expr e) es in
                let stmts_n = List.map fst stmtses in
                let es_n = List.map snd stmtses in

                stmts1 @ (List.flatten stmts_n),
                ((match e with
                    | CallExpr _ -> CallExpr (e1, es_n)
                    | NewExpr _ -> NewExpr (e1, es_n)
                ), mt_e)
            | (BridgeCall _ as e ) | (LambdaExpr _ as e) | (LitExpr _ as e) | (This as e) -> (* Interaction primtives are forbidden in LambdaExpr 
            TODO check as a precondition
            *)
                [], (e, mt_e)
            | InterceptedActivationRef (e1, e2_opt) ->
                let stmts1, e1 = rewrite_exprstmts_expr e1 in
                let stmts2, e2_opt = match e2_opt with
                    | None -> [], None
                    | Some e2 -> 
                        let stmts2, e2 = rewrite_exprstmts_expr e2 in
                        stmts2, Some e2
                in
                stmts1@stmts2, (InterceptedActivationRef (e1, e2_opt), mt_e)
            | UnopExpr (op, e) -> 
                let stmts, e = rewrite_exprstmts_expr e in
                stmts, (UnopExpr(op, e), mt_e)
            | Spawn sp -> begin
                let opt = Option.map (function e -> rewrite_exprstmts_expr e) sp.at in
                let stmtses = List.map (function e -> rewrite_exprstmts_expr e) sp.args in
                let stmts_n = List.flatten (List.map fst stmtses) in
                let es_n = List.map snd stmtses in
                
                match opt with
                | None -> stmts_n, (Spawn {
                    sp with args = es_n
                }, mt_e)
                | Some (stmts, e) -> stmts_n@stmts, (Spawn {
                    sp with 
                        args = es_n;
                        at = Some e;
                }, mt_e)
            end
            | OptionExpr None as e -> [], (e, mt_e)
            | OptionExpr Some e->
                let stmts, e = rewrite_exprstmts_expr e in
                stmts, (OptionExpr(Some e), mt_e)
            | ResultExpr (None, None) as e -> [], (e, mt_e)
            | ResultExpr (Some e, None) ->
                let stmts, e = rewrite_exprstmts_expr e in
                stmts, (ResultExpr(Some e, None), mt_e)
            | ResultExpr (None, Some e) ->
                let stmts, e = rewrite_exprstmts_expr e in
                stmts, (ResultExpr(None, Some e), mt_e)
            | ResultExpr (Some e1, Some e2) ->
                let stmts1, e1 = rewrite_exprstmts_expr e1 in
                let stmts2, e2 = rewrite_exprstmts_expr e2 in
                stmts1@stmts2, (ResultExpr(Some e1, Some e2), mt_e)
            | BlockExpr (b, es) ->
                let stmtses = List.map (function e -> rewrite_exprstmts_expr e) es in
                let stmts_n = List.flatten (List.map fst stmtses) in
                let es_n = List.map snd stmtses in
                stmts_n, (BlockExpr (b, es_n), mt_e)
            | Block2Expr (b, ees) ->
                let stmtses = List.map (function (e1, e2) -> 
                    rewrite_exprstmts_expr e1, 
                    rewrite_exprstmts_expr e2 
                ) ees in
                let stmts_n = List.flatten (List.map (function (a,b) -> fst a @ fst b) stmtses) in
                let ees_n = List.map  (function (a,b) -> snd a, snd b) stmtses in
                stmts_n, (Block2Expr (b, ees_n), mt_e)
        and rewrite_exprstmts_expr parent_opt (selector:_expr -> bool) rewriter : expr -> stmt list * expr = map2_place (rewrite_exprstmts_expr_ parent_opt selector rewriter)

        and rewrite_exprstmts_stmt_ parent_opt exclude_stmt selector rewriter place : _stmt -> stmt list =
            let fplace = (Error.forge_place "rewrite_exprstmts_stmt" 0 0) in
            let auto_place smth = {place = place; value=smth} in
            let auto_fplace smth = {place = fplace; value=smth} in

            let rewrite_exprstmts_expr = rewrite_exprstmts_expr parent_opt selector rewriter in
            let rewrite_exprstmts_stmt = rewrite_exprstmts_stmt parent_opt exclude_stmt selector rewriter in

            function
            (* Do not process excluded statement *)
            | stmt when exclude_stmt stmt -> [ auto_place stmt]

            (* Classical expr *)
            | EmptyStmt -> [ auto_place EmptyStmt]
            | AssignExpr (x, e) ->
                let estmts, e = rewrite_exprstmts_expr e in
                estmts @ [ auto_place (AssignExpr (x,e)) ]
            | AssignThisExpr (x, e) ->
                let estmts, e = rewrite_exprstmts_expr e in
                estmts @ [ auto_place (AssignThisExpr (x,e)) ]
            | BreakStmt -> [auto_place BreakStmt]
            | ContinueStmt -> [auto_place ContinueStmt]
            | CommentsStmt c -> [auto_place (CommentsStmt c)]
            | ExitStmt i -> [auto_place (ExitStmt i)]
            | ExpressionStmt e -> 
                let estmts, e = rewrite_exprstmts_expr e in
                estmts @ [ auto_place (ExpressionStmt e)]
            | ForStmt (mt, x, e, stmt) ->
                let estmts, e = rewrite_exprstmts_expr e in
                estmts @ [
                    auto_place (ForStmt(
                        mt,
                        x, 
                        e, 
                        auto_fplace (BlockStmt (rewrite_exprstmts_stmt stmt))
                    ))
                ]
            | IfStmt (e, stmt1, stmt2_opt) ->
                let estmts, e = rewrite_exprstmts_expr e in
                estmts @ [auto_place (IfStmt (
                    e,
                    (auto_fplace (BlockStmt (rewrite_exprstmts_stmt stmt1))),
                    Option.map (function stmt2 ->
                        auto_fplace (BlockStmt (rewrite_exprstmts_stmt stmt2))
                    ) stmt2_opt
                ))]    
            | LetStmt (mt, x, e) ->
                let estmts, e = rewrite_exprstmts_expr e in
                estmts @ [auto_place (LetStmt (mt, x, e))]
            | MatchStmt (e, branches) ->
                let estmts, e = rewrite_exprstmts_expr e in
                (* interaction primitives are disallowed in pattern *)
                estmts @ [ auto_place (MatchStmt (
                    e,
                    List.map (function (pattern, stmt) ->
                        (
                            pattern,
                            match rewrite_exprstmts_stmt stmt with
                            | [stmt] -> stmt
                            | stmts -> auto_fplace (BlockStmt stmts)
                            )        
                    ) branches
                ))] 
            | ReturnStmt e -> 
                let estmts, e = rewrite_exprstmts_expr e in
                estmts @ [auto_place (ReturnStmt e) ]
            | BlockStmt stmts ->
                [
                    auto_place (BlockStmt (List.flatten (List.map (function stmt -> rewrite_exprstmts_stmt stmt) stmts)))
                ]
            | GhostStmt stmt -> List.map (function stmt -> auto_place (GhostStmt stmt)) (rewrite_exprstmts_stmt stmt)
            | WithContextStmt(anonymous_mod, cname, e, stmts) ->
                let estmts, e = rewrite_exprstmts_expr e in
                estmts @ [ 
                    auto_place (WithContextStmt (
                        anonymous_mod,
                        cname, 
                        e, 
                        List.flatten (List.map rewrite_exprstmts_stmt stmts)
                    ))
                ]

        and rewrite_exprstmts_stmt parent_opt exclude_stmt selector rewriter = map0_place (rewrite_exprstmts_stmt_ parent_opt exclude_stmt selector rewriter)

        and rewrite_exprstmts_component_item_ parent_opt exclude_stmt selector rewriter place citem = 
        match citem with
        | Method m ->[Method { m with
        value = {
            m.value with body = rewrite_exprstmts_custom_method0_body rewrite_exprstmts_stmt parent_opt exclude_stmt selector rewriter m.value.body
        }
        }]
        | Term t -> List.map (function t -> Term t) (rewrite_exprstmts_term parent_opt exclude_stmt selector rewriter t)
        (* citem without statement *)
        | Contract _ | Include _ |Inport _ | Outport _ | State _ -> [citem]
        and rewrite_exprstmts_component_item parent_opt exclude_stmt selector rewriter = map_places (rewrite_exprstmts_component_item_ parent_opt exclude_stmt selector rewriter) 


        and rewrite_exprstmts_component_dcl_ parent_opt  exclude_stmt selector rewriter place = function
        | ComponentStructure cdcl -> 
            let parent_opt = Some cdcl.name in
            ComponentStructure {
            cdcl with 
                body = List.flatten (List.map (rewrite_exprstmts_component_item parent_opt exclude_stmt selector rewriter) cdcl.body)
        }
        and rewrite_exprstmts_component_dcl parent_opt  exclude_stmt selector rewriter = map_place (rewrite_exprstmts_component_dcl_ parent_opt  exclude_stmt selector rewriter) 

        and rewrite_exprstmts_term_ (parent_opt:component_variable option) exclude_stmt selector rewriter place t =  
        match t with
        | Component cdcl -> [Component (rewrite_exprstmts_component_dcl parent_opt exclude_stmt selector rewriter cdcl)]
        | Function fdcl -> [Function { fdcl with
            value = {
                fdcl.value with body = rewrite_exprstmts_custom_method0_body rewrite_exprstmts_stmt parent_opt  exclude_stmt selector rewriter fdcl.value.body
            }
        }]
        | Stmt stmt -> List.map (function stmt -> Stmt stmt) (rewrite_exprstmts_stmt parent_opt  exclude_stmt selector rewriter stmt)

        (* Term without statement*)
        | EmptyTerm | Comments _ | Typealias _ | Typedef _ | Derive _ -> [t]
        and rewrite_exprstmts_term parent_opt exclude_stmt selector rewriter = map_places (rewrite_exprstmts_term_  parent_opt exclude_stmt selector rewriter) 

        and rewrite_exprstmts_program exclude_stmt selector rewriter program =
            List.flatten (List.map (rewrite_exprstmts_term None exclude_stmt selector rewriter) program)

        (**********************************************************************)



        let rec rewrite_stmt_component_item_ recurse selector rewriter place citem = 
        match citem with
        | Method m ->[Method { m with
        value = {
            m.value with body = rewrite_stmt_custom_method0_body rewrite_stmt_stmt recurse selector rewriter m.value.body
        }
        }]
        | Term t -> List.map (function t -> Term t) (rewrite_stmt_term recurse selector rewriter t)
        (* citem without statement *)
        | Contract _ | Include _ |Inport _ | Outport _ | State _ -> [citem]
        and rewrite_stmt_component_item recurse selector rewriter = map_places (rewrite_stmt_component_item_ recurse selector rewriter) 


        and rewrite_stmt_component_dcl_ recurse selector rewriter place = function
        | ComponentStructure cdcl -> ComponentStructure {
            cdcl with 
                body = List.flatten (List.map (rewrite_stmt_component_item recurse selector rewriter) cdcl.body)
        }
        and rewrite_stmt_component_dcl recurse selector rewriter = map_place (rewrite_stmt_component_dcl_ recurse selector rewriter) 

        and rewrite_stmt_term_ recurse selector rewriter place t =  
        match t with
        | Component cdcl -> [Component (rewrite_stmt_component_dcl recurse selector rewriter cdcl)]
        | Function fdcl -> [Function { fdcl with
            value = {
                fdcl.value with body = rewrite_stmt_custom_method0_body rewrite_stmt_stmt recurse selector rewriter fdcl.value.body
            }
        }]
        | Stmt stmt -> List.map (function stmt -> Stmt stmt) (rewrite_stmt_stmt  recurse selector rewriter stmt)

        (* Term without statement*)
        | EmptyTerm | Comments _ | Typealias _ | Typedef _ | Derive _ -> [t]
        and rewrite_stmt_term recurse selector rewriter = map_places (rewrite_stmt_term_ recurse selector rewriter) 

        and rewrite_stmt_program recurse selector rewriter program =
            List.flatten (List.map (rewrite_stmt_term recurse selector rewriter) program)

        (********************************************************************************************)
        let protect_renaming renaming = function x ->
            if Atom.is_builtin x then x 
            else renaming x

        let rec _rename_composed_type renaming place = 
            let rmt = rename_main_type renaming in   
        function  
        | TActivationRef mt -> TActivationRef (rmt mt)
        | TArrow (mt1, mt2) -> TArrow (rmt mt1, rmt mt2)
        | TVar x -> TVar (renaming x)
        | TFlatType _ as ct -> ct
        | TArray mt -> TArray (rmt mt)
        | TDict (mt1, mt2) -> TDict (rmt mt1, rmt mt2)
        | TList mt -> TList (rmt mt)
        | TOption mt -> TOption (rmt mt)
        | TResult (mt1, mt2) -> TResult (rmt mt1, rmt mt2)
        | TSet mt -> TSet (rmt mt)
        | TTuple (mts) -> TTuple (List.map rmt mts)
        | TVPlace mt -> TVPlace (rmt mt)
        | TUnion (mt1, mt2) -> TUnion (rmt mt1, rmt mt2)
        | TBridge tb -> TBridge {
            in_type = rmt tb.in_type;
            out_type = rmt tb.out_type;
            protocol = rmt tb.protocol;
        }
        | TInport mt -> TInport (rmt mt)
        | TOutport -> TOutport
        | TForall (x, mt) -> TForall (renaming x, rmt mt)
        | TPolyVar x -> TPolyVar (renaming x)
        and rename_composed_type renaming = map_place (_rename_composed_type (protect_renaming renaming))

        and _rename_session_type renaming place = 
            let rmt = rename_main_type renaming in   
            let rst = rename_session_type renaming in   
            let rb (x, st, ac_opt) = (renaming x, rst st, Option.map (rename_applied_constraint renaming) ac_opt) in
        function  
        | STEnd | STWildcard -> STEnd
        | STVar x -> STVar (renaming x)
        | STSend (mt, st) -> STSend (rmt mt, rst st)
        | STRecv (mt, st) -> STRecv (rmt mt, rst st)
        | STBranch branches -> STBranch (List.map rb branches)
        | STSelect branches -> STSelect (List.map rb branches)
        | STRec (x, st) -> STRec (renaming x, rst st)
        | STInline x -> STInline (renaming x)
        | STPolyVar x -> STPolyVar (renaming x)
        | STDual st -> STDual (rst st)
        and rename_session_type renaming : session_type -> session_type = map_place (_rename_session_type (protect_renaming renaming))

        and _rename_component_type renaming place = 
            let rmt = rename_main_type renaming in   
        function  
        | CompTUid x -> CompTUid (renaming x)
        | TStruct (x, sign) -> TStruct 
            (renaming x, Atom.VMap.fold (fun x mt acc -> Atom.VMap.add (renaming x) (rmt mt) acc) sign Atom.VMap.empty)
        | TPolyCVar x -> TPolyCVar (renaming x)
        and rename_component_type renaming = map_place (_rename_component_type renaming)

        and _rename_main_type renaming place = 
            let rmt = rename_main_type renaming in   
        function  
        | EmptyMainType -> EmptyMainType
        | CType ct -> CType (rename_composed_type renaming ct)
        | SType st -> SType (rename_session_type renaming st)
        | CompType cmt -> CompType (rename_component_type renaming cmt)
        | ConstrainedType (mt, ac) -> ConstrainedType (rmt mt, rename_applied_constraint renaming ac)
        and rename_main_type renaming = map_place (_rename_main_type (protect_renaming renaming))

        and _rename_constraint_header renaming place = function
        | UseMetadata (mt, x) -> UseMetadata (rename_main_type renaming mt, renaming x)
        | SetTimer x -> SetTimer (renaming x)
        | SetFireTimer (x,i) -> SetFireTimer (renaming x, i)
        and rename_constraint_header renaming = map_place (_rename_constraint_header (protect_renaming renaming))

        and rename_applied_constraint renaming (headers,e_opt) = 
            List.map (rename_constraint_header renaming) headers, Option.map (rename_expr renaming) e_opt

        and _rename_literal renaming place lit = 
        match lit with
        | VoidLit | BoolLit _ | FloatLit _ | IntLit _ | LabelLit _ | StringLit _ | ActivationRef _ | Place _  -> lit
        | BLabelLit x -> BLabelLit (renaming x)
        | VPlace vp ->
            let rec rename_vp (vp:vplace) =  {
                name = renaming vp.name;
                nbr_instances = rename_expr renaming vp.nbr_instances;
                features = vp.features;
                children = List.map rename_vp vp.children
            } in
            VPlace (rename_vp vp)
        | StaticBridge b -> StaticBridge {
            id = renaming b.id;
            protocol_name = renaming b.protocol_name
        }
        and rename_literal renaming = map_place (_rename_literal (protect_renaming renaming)) 

        and _rename_expr renaming place (e, mt_e) = 
            let re = rename_expr renaming in
            let rmt = rename_main_type renaming in

            let rename_attribute = false in (* TODO expose argument, renaming attributes is wrong except if type schemas have been changed *)

            let e = match e with 
            | EmptyExpr -> EmptyExpr
            | VarExpr x -> VarExpr (renaming x)
            | ImplicitVarExpr x -> ImplicitVarExpr (renaming x)
            | InterceptedActivationRef (e1, e2_opt) -> InterceptedActivationRef (re e1, Option.map re e2_opt) 
            | ActivationAccessExpr (x, e, y) -> ActivationAccessExpr (renaming x, re e, if rename_attribute then renaming y else y)
            | AccessExpr (e1, ({value=VarExpr _,_} as e2)) -> AccessExpr (re e1, if rename_attribute then re e2 else e2)
            | AccessExpr (e1, e2) -> AccessExpr (re e1, re e2)
            | BinopExpr (e1, op, e2) -> BinopExpr (re e1, op, re e2)
            | LambdaExpr (params, e) -> 
                LambdaExpr (
                    List.map (map_place (fun _ (mt,x) -> rmt mt, renaming x)) params,
                    re e) 
            | LitExpr l -> LitExpr (rename_literal renaming l)
            | UnopExpr (op, e) -> UnopExpr (op, re e)
            | CallExpr (e, es) -> CallExpr (re e, List.map re es)
            | NewExpr (e, es) -> NewExpr (re e, List.map re es)
            | PolyApp (e, mts) -> PolyApp (re e, List.map rmt mts)
            | BridgeCall{protocol_name} -> BridgeCall{
                protocol_name = renaming protocol_name;
            }
            | TernaryExpr (e1, e2, e3) -> TernaryExpr (re e1, re e2, re e3)
            | This -> This
            | Spawn spawn -> Spawn {
                c = rename_component_expr renaming spawn.c;
                args = List.map re spawn.args;
                at = Option.map re spawn.at;
            }
            | BoxCExpr ce -> BoxCExpr (rename_component_expr renaming ce)
            | OptionExpr e_opt -> OptionExpr (Option.map re e_opt)
            | ResultExpr (e1_opt, e2_opt) -> ResultExpr (Option.map re e1_opt, Option.map re e2_opt)
            | BlockExpr (b, es) -> BlockExpr (b, List.map re es)
            | Block2Expr (b, ees) -> Block2Expr (b,
                List.map (function (e1, e2) -> (re e1, re e2)) ees
            )
            in
            (e, rmt mt_e)
        and rename_expr renaming : expr -> expr = map_place (_rename_expr (protect_renaming renaming))

        and _rename_stmt renaming place = 
            let re = rename_expr renaming in
            let rmt = rename_main_type renaming in
            let rstmt = rename_stmt renaming in
        function
        | EmptyStmt -> EmptyStmt
        | AssignExpr (x, e) -> AssignExpr (renaming x, re e)
        | AssignThisExpr (x, e) -> AssignThisExpr (renaming x, re e)
        | LetStmt (mt, x, e) -> LetStmt (rmt mt, renaming x, re e)
        | CommentsStmt _ as stmt -> stmt
        | BreakStmt -> BreakStmt
        | ContinueStmt -> ContinueStmt
        | ExitStmt i -> ExitStmt i
        | ForStmt (mt, x, e, stmt) -> ForStmt (rmt mt, renaming x, re e, rstmt stmt)
        | IfStmt (e, stmt1, stmt2_opt) -> IfStmt (re e, rstmt stmt1, Option.map rstmt stmt2_opt)
        | MatchStmt (e, branches) -> MatchStmt (re e, List.map (function (e, stmt) -> (re e, rstmt stmt)) branches) 
        | ReturnStmt e -> ReturnStmt (re e)
        | ExpressionStmt e -> ExpressionStmt (re e)
        | BlockStmt stmts -> BlockStmt (List.map rstmt stmts)
        | GhostStmt stmt -> GhostStmt (rstmt stmt)
        | WithContextStmt (flag, x, e, stmts) -> WithContextStmt (flag, renaming x, re e, List.map rstmt stmts)
        and rename_stmt renaming = map_place (_rename_stmt (protect_renaming renaming))

        and _rename_param renaming place (mt, x) = (rename_main_type renaming mt, renaming x)
        and rename_param renaming = map_place (_rename_param (protect_renaming renaming))


        and _rename_port renaming place ((p, mt_p): _port * main_type) = ({
            name = renaming p.name;
            expecting_st = rename_main_type renaming p.expecting_st;
            callback = rename_expr renaming p.callback;
        }, rename_main_type renaming mt_p)
        and rename_port renaming = map_place (_rename_port (protect_renaming  renaming))

        and _rename_outport renaming place ((p, mt_p): _outport * main_type) = ({
            name = renaming p.name;
        }, rename_main_type renaming mt_p)
        and rename_outport renaming = map_place (_rename_outport (protect_renaming renaming))

        and rename_method_annotation renaming : method_annotation -> method_annotation = function
        | MsgInterceptor _ as a -> a
        | SessionInterceptor _ as a -> a
        | Onboard xs -> Onboard (List.map renaming xs)

        and rename_component_annotation renaming = function
        | Capturable {allowed_interceptors} -> Capturable {allowed_interceptors = List.map renaming allowed_interceptors}

        and _rename_contract renaming place c = {
            method_name = renaming c.method_name;
            pre_binders = List.map (function (mt, x, e) -> (rename_main_type renaming mt, renaming x, rename_expr renaming e)) c.pre_binders;
            ensures = Option.map (rename_expr renaming) c.ensures; 
            returns = Option.map (rename_expr renaming) c.returns;
        }
        and rename_contract renaming = map_place (_rename_contract (protect_renaming renaming))

        and _rename_component_expr renaming place (ce, mt_ce) = 
            let rce = rename_component_expr renaming in
            let re = rename_expr renaming in
            let ce = match ce with
            | VarCExpr x -> VarCExpr (renaming x)
            | AppCExpr (ce, ces) -> AppCExpr (rce ce, List.map rce ces)
            | UnboxCExpr e -> UnboxCExpr (re e)
            | AnyExpr e -> AnyExpr (re e)
            in 
            (ce, rename_main_type renaming mt_ce)
        and rename_component_expr renaming = map_place (_rename_component_expr (protect_renaming renaming))



        let rec _rename_state renaming place = function
        | StateDcl s -> StateDcl {
            ghost = s.ghost;
            type0 = rename_main_type renaming s.type0;
            name = renaming s.name;
            body = rename_state_dcl_body rename_expr renaming s.body;
        }
        and rename_state renaming = map_place (_rename_state (protect_renaming renaming))

        and _rename_method renaming place (m:_method0) = {
            annotations = List.map (rename_method_annotation renaming) m.annotations;
            ghost = m.ghost;
            ret_type = rename_main_type renaming m.ret_type;
            name = renaming m.name;
            args = List.map (rename_param renaming) m.args;
            body = rename_custom_method0_body rename_stmt renaming m.body;
            contract_opt  = Option.map (rename_contract renaming) m.contract_opt;
            on_destroy = m.on_destroy;
            on_startup = m.on_startup;
        }
        and rename_method renaming = map_place (_rename_method (protect_renaming renaming))

        and _rename_component_item renaming place = function
        | Contract c -> Contract (rename_contract renaming c)
        | Method m -> Method (rename_method renaming m)
        | State s -> State (rename_state renaming s)
        | Inport p -> Inport (rename_port renaming p)
        | Outport p -> Outport (rename_outport renaming p)
        | Term t -> Term (rename_term renaming t)
        | Include ce -> Include (rename_component_expr renaming ce)
        and rename_component_item renaming = map_place (_rename_component_item (protect_renaming renaming))

        and _rename_component_dcl renaming place = function
        | ComponentAssign {name; value} -> ComponentAssign {
            name = renaming name;
            value =rename_component_expr renaming value;
        }
        | ComponentStructure {target_name; annotations; name; body} -> ComponentStructure {
            target_name = target_name;
            annotations = List.map (rename_component_annotation renaming) annotations;
            name = renaming name;
            body = List.map (rename_component_item renaming) body
        }
        and rename_component_dcl renaming = map_place (_rename_component_dcl (protect_renaming renaming))

        and _rename_function_dcl renaming place (fdcl: _function_dcl) = {
            name = renaming fdcl.name;
            targs = List.map renaming fdcl.targs;
            ret_type = rename_main_type renaming fdcl.ret_type;
            args = List.map (rename_param renaming) fdcl.args;
            body = rename_custom_method0_body rename_stmt renaming fdcl.body
        }
        and rename_function_dcl renaming = map_place (_rename_function_dcl (protect_renaming renaming))

        and _rename_typedef renaming place = 
            let rmt = rename_main_type renaming in
        function
        (* TODO body is not processed yet*)
        | ClassicalDef (x, mts, body) -> ClassicalDef (renaming x, List.map rmt mts, body)
        | EventDef (x, mts, body) -> EventDef (renaming x, List.map rmt mts, body)
        | ProtocolDef (x, mt) -> ProtocolDef (renaming x, rmt mt)
        | VPlaceDef x -> VPlaceDef (renaming x)
        and rename_typedef renaming = map_place (_rename_typedef (protect_renaming renaming))

        and rename_derivation renaming (d:derivation) = {
            name = renaming d.name;
            cargs = List.map (rename_component_expr renaming) d.cargs;
            targs = List.map (rename_main_type renaming) d.targs;
            eargs = List.map (rename_expr renaming) d.eargs
        }

        and _rename_term renaming place = function
        | EmptyTerm -> EmptyTerm
        | Comments _ as t -> t
        | Stmt stmt -> Stmt (rename_stmt renaming stmt)
        | Component c -> Component (rename_component_dcl renaming c)
        | Function fdcl -> Function (rename_function_dcl renaming fdcl)
        | Typealias (x, mt_opt) -> Typealias (renaming x, rename_typealias_body rename_main_type renaming mt_opt)
        | Typedef tdef -> Typedef (rename_typedef renaming tdef)
        | Derive d -> Derive (rename_derivation renaming d)
        and rename_term renaming = map_place (_rename_term (protect_renaming renaming))

        let rename_program renaming = List.map (rename_term renaming)

        (********************************************************************************************)

        (* Where add them in the scope ?
            Just after the point where there is no more free vars in new_terms
            than in terms
        *)
        let insert_in_terms new_terms terms = 
            let fvars0 = Atom.Set.of_seq (List.to_seq (List.map snd (snd (free_vars_program Atom.Set.empty terms)))) in
            let ftvars0 = Atom.Set.of_seq (List.to_seq (snd (free_tvars_program Atom.Set.empty terms))) in

            (*Since rec def*)
            let shallow_scan already_binded = function
            | {value = Component {value=ComponentStructure {name}}} -> Atom.Set.add name already_binded 
            | _ -> already_binded
            in
            let toplevel_components =  List.fold_left shallow_scan Atom.Set.empty terms in

            (* 
                acc::t::ts 
                try to add it between acc and t
            *)
            let insert_depth = ref 0 in
            let rec insert_new_terms acc = function
            | [] -> new_terms
            | t::ts -> 
                let current_terms = acc@new_terms in 
                let _fvars = Atom.Set.of_seq (List.to_seq (List.map snd (snd (free_vars_program Atom.Set.empty current_terms)))) in
                let _ftvars = Atom.Set.of_seq (List.to_seq (snd (free_tvars_program Atom.Set.empty current_terms))) in
                (* Because component are type rec per scope *)
                let _ftvars = Atom.Set.diff _ftvars toplevel_components in

                if Atom.Set.subset _fvars  fvars0  && Atom.Set.subset _ftvars ftvars0 then(
                    logger#debug "insert_in_terms insert depth %d" !insert_depth;
                    new_terms@(t::ts)
                )else
                    t::(insert_new_terms (acc@[t]) ts) (* FIXME O(n²) *)
            in

            (* New terms do not depend of binders in terms *)
            insert_new_terms [] terms

        (*********************************************************************)

        (* lowest common ancestor *)
        let rec aux_find_lca names current_name (subcomponents : component_dcl list) : bool * Atom.atom option  = 
            assert (Atom.Set.cardinal names > 1);
            let tmp = List.map (find_lca_cdcl names) subcomponents in
            let tmp_true = List.filter (function (f,_) -> f) tmp in
            if List.length tmp_true > 1 then 
                true, current_name
            else (
                (* Target contains all names + LCA *)
                let [_, Some target_name] = tmp_true in 
                let [target] = List.filter (function | {value=ComponentStructure scdcl} -> scdcl.name = target_name) subcomponents in

                find_lca_cdcl names target
            )

        and find_lca_cdcl_ names place : _component_dcl ->  bool * Atom.atom option = function
        | ComponentStructure cdcl when Atom.Set.mem cdcl.name names -> true, None
        | ComponentStructure cdcl -> 
            let subcomponents = List.filter (function | {value=Term{value=Component _}} -> true | _ -> false) cdcl.body in
            let subcomponents = List.map (function | {value=Term{value=Component cdcl}} -> cdcl) subcomponents in

            aux_find_lca names (Some cdcl.name) subcomponents
        and find_lca_cdcl names : component_dcl ->  bool * Atom.atom option = map0_place (find_lca_cdcl_ names)

        and find_lca_program names program =  
            if Atom.Set.cardinal names > 1 then (
                let subcomponents = List.filter (function | {value=Component _} -> true | _ -> false) program in
                let subcomponents = List.map (function | {value=Component cdcl} -> cdcl) subcomponents in

                snd (aux_find_lca names None subcomponents)
            )else Some (Atom.Set.min_elt names)

        (*
            lca_name = None -> toplevel
            see semantics of IR_utils.insert_in_terms for insertion into lca
        *)
        let insert_terms_into_lca (parents: (Atom.atom option) list) terms_to_insert program = 
            assert( parents <> []);

            let common_ancestor_name = 
                if List.mem None parents then ( (* LCA = Top-level *)
                    None
                )
                else (
                    let parents = List.map Option.get (List.filter (function x -> x <> None) parents) in
                    let parents_set = Atom.Set.of_seq (List.to_seq parents) in

                    (* Search for lowest common ancestor *)
                    find_lca_program parents_set program
                ) 
            in
            logger#warning "LCA = %s"(match common_ancestor_name with | None -> "None" | Some x -> Atom.to_string x);

            let insert_in_ancestor (program: program) : Atom.atom option -> program = function
                | None -> insert_in_terms terms_to_insert program 
                | Some lca_name ->
                    let ancestor_selector = function 
                        | Component {value=ComponentStructure cdcl} -> cdcl.name = lca_name 
                        | _ -> false
                    in
                    let ancestor_rewriter place = function
                        | Component {place; value=ComponentStructure cdcl} ->
                            let terms_body = List.map (function | {value=Term t} -> t) (List.filter (function |{value=Term _} -> true | _ -> false) cdcl.body) in
                            let remaining_body = List.filter (function |{value=Term _} -> false | _ -> true) cdcl.body in

                            let terms_body = insert_in_terms terms_to_insert terms_body in 
                            let terms_body = List.map (function t -> {place=t.place; value=Term t}) terms_body in


                            [ 
                                Component {place; value = ComponentStructure {cdcl with
                                    body = terms_body @ remaining_body 
                                }} 
                            ]
                    in

                    rewrite_term_program ancestor_selector ancestor_rewriter program
            in 

            insert_in_ancestor program common_ancestor_name
    end
end  