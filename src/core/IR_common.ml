open AstUtils
open Easy_logging

(************************************* Base types ****************************)
open Label

type test = int Atom.VMap.t 
[@@deriving show { with_path = false }]


let logger = Logging.make_logger "_1_ compspec" Debug [];;


(* NB TIRC is a **copy** of Make content, it should not be edit manually pour la partie AST uniquement 
*)
module type TIRC = sig
    module Variable : TVariable 
    type ident = string (*TODO remove ident if not used *) 
    and expr_variable = Variable.t
    and type_variable = Atom.atom
    and component_variable = Atom.atom
    and comments = AstUtils.comments

    (************************************ Types **********************************)
    and flat_type = AstUtils.flat_type

    and tbridge = {
        in_type: main_type; 
        out_type: main_type;
        protocol: main_type;
    }

    and _composed_type =
        | TActivationInfo of main_type
        (*| TSession of session_type * variable*)
        | TArrow of main_type * main_type

        | TVar of type_variable 
        | TFlatType of flat_type

        | TArray of main_type 
        | TDict of main_type * main_type
        | TList of main_type 
        | TOption of main_type
        | TResult of main_type * main_type
        | TSet of main_type
        | TTuple of main_type list
        | TVPlace of main_type

        | TUnion of main_type * main_type


        (** Message-passing *)
        | TBridge of tbridge
        | TPort of main_type * main_type (* session_type * bridge type *)

        | TRaw of Impl_common.blackbox_term (*TODO move it to IRI by doing so composed type should not be any more in common *)

        (* Polymorphsim*)
        | TPolyVar of type_variable (* TODO do we need the disctinction between TVar and TPolyVar ????? *)
        | TForall of type_variable * main_type 
    and composed_type = _composed_type placed

    and _session_type =  
        | STEnd 
        | STVar of type_variable  (** x *) 
        | STSend of main_type * session_type
        | STRecv of main_type * session_type
        | STBranch of (type_variable * session_type * applied_constraint option) list            
        | STSelect of (type_variable * session_type *  applied_constraint option) list               
        | STRec of type_variable * session_type (* X * type*) 
        | STInline of type_variable (* syntaxic suggar in order to inline an existing session type definition*)

        (* Polymorphsim*)
        | STPolyVar of type_variable
    and session_type = _session_type placed

    and _component_type =
        | CompTUid of component_variable 
        | TStruct of main_type Atom.VMap.t (*types of states, methods, ports, ... and subcomponents *)
        (* Polymorphsim*)
        | TPolyCVar of component_variable
    and component_type = _component_type placed

    and _main_type = 
        | EmptyMainType (* Just not to have to manipulate option
            There is more some in pattern matching than None
        *) 
        | CType of composed_type 
        | SType of session_type
        (* First value component type*)
        | CompType of component_type
        (* Dynamic (or not) contraints*)
        | ConstrainedType of main_type * applied_constraint 
        (*gadt contraints: type -> bool *)
    and main_type = _main_type placed

    (******************************** Constraints ********************************)
    and _constraint_header =      
        | UseMetadata of main_type * expr_variable
        | SetTimer of expr_variable
        | SetFireTimer of expr_variable * int (* specify timeout delay *)
    and constraint_header = _constraint_header placed

    and _constraints = (_expr * main_type) (* for now, maybe we will need to restrict a bit for SMT solving*)
    and constraints = _constraints placed 

    and applied_constraint = (constraint_header list) * constraints option

    (************************************ (V) - Place ****************************)
    and _place = unit (* tODO get ride of this its dynamic can not be instancied statically*)
    and place = _place placed
    and vplace = { 
        name:           component_variable;
        nbr_instances:  expr;
        features:     (string, string) Hashtbl.t;[@opaque]
        children:       vplace list
    }

    and vplaces = vplace list

    (*module VPlaceEnv = LabelMap*)

    (************************************* Literals ******************************)

    and _literal = 
        | VoidLit
        | BoolLit of bool
        | FloatLit of float 
        | IntLit of int
        | LabelLit of label 
        | StringLit of string

        (** Activations *)
        | ActivationInfo of unit (* TODO get ride of literal can not be instancied statically *)

        (** Placement *)
        | Place of place (* TODO get ride *)
        | VPlace of vplace 

        (** Message-passing *)
        | Bridge of {
            id: component_variable; 
            protocol_name: component_variable;
        }
    and literal = _literal placed

    (************************************* Operations ******************************)

    and unop = AstUtils.unop
    and binop = AstUtils.binop
    and block = AstUtils.block
    and block2 = AstUtils.block2

    (************************************ Expr & Stmt *****************************)

    and _expr = 
        | VarExpr of expr_variable 
        | ImplicitVarExpr of expr_variable

        | ActivationAccessExpr of component_variable * expr * expr_variable (* cname, e, x*)
        | AccessExpr of expr * expr (*e1.e2*)
        | BinopExpr of expr * binop * expr 
        | LambdaExpr of expr_variable * main_type * expr 
        | LitExpr of literal
        | UnopExpr of unop * expr 

        (** *)
        (* TODO closure/lamba*)
        | CallExpr of expr * expr list(** method and fct can be recursive *)
        | NewExpr of expr * expr list                   
        | PolyApp of expr * main_type list

        (** Control flow *)

        (* Reflexifity *)
        | This (* current activation *)

        (* Activation lifetime expr *)
        | Spawn of {c: component_expr; args: expr list; at: expr option}  

        (* Structure expr *)
        | BoxCExpr of component_expr

        (** *)
        | OptionExpr of expr option
        | ResultExpr of (expr option * expr option) (* Ok, Err *)
        | BlockExpr of block * expr list
        | Block2Expr of block2 * (expr * expr) list
    and expr = (_expr * main_type) placed

    and _stmt = 
        | EmptyStmt

        (** Binders *)
        | AssignExpr of expr_variable * expr
        | AssignThisExpr of component_variable * expr
        | LetExpr of main_type * expr_variable * expr

        (** Comments *)
        | CommentsStmt of comments

        (** Control flow *)
        | BreakStmt
        | ContinueStmt
        | ExitStmt of int
        | ForStmt of main_type * expr_variable * expr * stmt
        | IfStmt of expr * stmt * stmt option
        | MatchStmt of expr * (expr * stmt) list
        | ReturnStmt of expr

        (**type name, type definition*)
        | ExpressionStmt of expr
        | BlockStmt of stmt list

        | GhostStmt of stmt
    and stmt = _stmt placed

    and _param = main_type * expr_variable
    and param = _param placed

    (******************************** Component **********************************)
    and _port = {
        name: component_variable;
        input: expr;
        expecting_st: main_type;
        callback: expr
    }
    and port = (_port * main_type) placed

    (******************************** Contracts **********************************)
    and _contract = { (* TODO GADT *)
        method_name: component_variable;
        pre_binders: (main_type * expr_variable * expr) list; 
        ensures: expr option;
        returns: expr option;
    }
    and contract = _contract placed


    (********************** Manipulating component structure *********************)
    and _component_expr = 
        | VarCExpr of component_variable  
        (* functor or X(1) *)
        | AppCExpr of component_expr * component_expr 
        | UnboxCExpr of expr
        | AnyExpr of expr
    and component_expr = (_component_expr * main_type) placed

    (* The following annotation requests the automatic generation of a [show_]
    function for each of the types defined above.*)
    [@@deriving show { with_path = false }]


    (****** BEGIN EDIT BY HUMAN*****)
    val dual : session_type -> session_type
    val collect_expr_expr : Variable.Set.t -> (_expr -> bool) -> (Variable.Set.t -> expr -> 'a list) -> expr -> Variable.Set.t * 'a list * (main_type*expr_variable) list
    val collect_expr_stmt : Variable.Set.t -> (_expr -> bool) -> (Variable.Set.t -> expr -> 'a list) -> stmt -> Variable.Set.t * 'a list * (main_type*expr_variable) list
    val collect_expr_mtype : Variable.Set.t -> (_expr -> bool) -> (Variable.Set.t -> expr -> 'a list) -> main_type -> Variable.Set.t * 'a list *(main_type*expr_variable) list
    val free_vars_expr : Variable.Set.t -> expr -> Variable.Set.t * (main_type*expr_variable) list
    val free_vars_stmt : Variable.Set.t -> stmt -> Variable.Set.t * (main_type*expr_variable) list
    val free_vars_mtype : Variable.Set.t -> main_type -> Variable.Set.t * (main_type*expr_variable) list
    val free_tvars_mtype : Atom.Set.t -> main_type -> Atom.Set.t * type_variable list
    val  timers_of_headers : constraint_header list -> expr_variable list
    val rewrite_expr_expr : (_expr -> bool) -> (_expr -> _expr) -> expr -> expr
    val rewrite_expr_stmt : (_expr -> bool) -> (_expr -> _expr) -> stmt -> stmt
    val replace_expr_expr : expr_variable -> (expr_variable option * _expr option) -> expr -> expr
    val replace_expr_stmt : expr_variable -> (expr_variable option * _expr option) -> stmt -> stmt
    val replace_type_main_type : type_variable ->(type_variable option * _main_type option) -> main_type -> main_type
    val replace_stype_session_type : type_variable ->(type_variable option * _session_type option) -> session_type -> session_type
    val equal_ctype : composed_type -> composed_type -> bool
    val equal_cmtype : component_type -> component_type -> bool
    val equal_stype : session_type -> session_type -> bool
    val equal_mtype : main_type -> main_type -> bool
    val equal_expr : expr -> expr -> bool
    val equal_cexpr : component_expr -> component_expr -> bool

    val unfold_st_star : session_type -> session_type
end




module Make (V : TVariable) : (TIRC with module Variable = V and type Variable.t = V.t)  = struct
    module Variable = V 
    (*let show_variable = Variable.show_variable
    let pp_variable = Variable.pp_variable*)

    (* TODO clean AST *)

    type ident = string (*TODO remove ident if not used *) 
    and expr_variable = Variable.t
    and type_variable = Atom.atom
    and component_variable = Atom.atom
    and comments = AstUtils.comments

    (************************************ Types **********************************)
    and flat_type = AstUtils.flat_type

    and tbridge = {
        in_type: main_type; 
        out_type: main_type;
        protocol: main_type;
    }

    and _composed_type =
        | TActivationInfo of main_type
        (*| TSession of session_type * variable*)
        | TArrow of main_type * main_type

        | TVar of type_variable
        | TFlatType of flat_type

        | TArray of main_type 
        | TDict of main_type * main_type
        | TList of main_type 
        | TOption of main_type
        | TResult of main_type * main_type
        | TSet of main_type
        | TTuple of main_type list
        | TVPlace of main_type

        | TUnion of main_type * main_type


        (** Message-passing *)
        | TBridge of tbridge
        | TPort of main_type * main_type (* session_type * bridge type *)

        | TRaw of Impl_common.blackbox_term (*TODO move it to IRI by doing so composed type should not be any more in common *)

        (* Polymorphsim*)
        | TPolyVar of type_variable
        | TForall of type_variable * main_type
    and composed_type = _composed_type placed

    and _session_type =  
        | STEnd 
        | STVar of type_variable
        | STSend of main_type * session_type
        | STRecv of main_type * session_type
        | STBranch of (type_variable * session_type * applied_constraint option) list            
        | STSelect of (type_variable * session_type *  applied_constraint option) list               
        | STRec of type_variable * session_type (* X * type*) 
        | STInline of type_variable (* syntaxic suggar in order to inline an existing session type definition*)
        (* Polymorphsim*)
        | STPolyVar of type_variable
    and session_type = _session_type placed

    and _component_type =
        | CompTUid of component_variable 
        | TStruct of main_type Atom.VMap.t (*types of states, methods, ports, ... and subcomponents *)
        (* Polymorphsim*)
        | TPolyCVar of component_variable
    and component_type = _component_type placed

    and _main_type = 
        | EmptyMainType
        | CType of composed_type 
        | SType of session_type
        (* First value component type*)
        | CompType of component_type
        (* Dynamic (or not) contraints*)
        | ConstrainedType of main_type * applied_constraint 
        (*gadt contraints: type -> bool *)
    and main_type = _main_type placed

    (******************************** Constraints ********************************)
    and _constraint_header =      
        | UseMetadata of main_type * expr_variable
        | SetTimer of expr_variable
        | SetFireTimer of expr_variable * int (* specify timeout delay *)
    and constraint_header = _constraint_header placed

    and _constraints = _expr * main_type (* for now, maybe we will need to restrict a bit for SMT solving*)
    and constraints = _constraints placed 

    and applied_constraint = (constraint_header list) * constraints option

    (************************************ (V) - Place ****************************)
    and _place = unit 
    and place = _place placed
    and vplace = { 
        name:           component_variable;
        nbr_instances:  expr;
        features:     (string, string) Hashtbl.t;[@opaque]
        children:       vplace list
    }

    and vplaces = vplace list (* TODO get ride notused + defined twice also*)

    (*module VPlaceEnv = LabelMap*)

    (************************************* Literals ******************************)

    and _literal = 
        | VoidLit
        | BoolLit of bool
        | FloatLit of float 
        | IntLit of int
        | LabelLit of label 
        | StringLit of string

        (** Activations *)
        | ActivationInfo of unit (* TODO *)

        (** Placement *)
        | Place of place
        | VPlace of vplace 

        (** Message-passing *)
        | Bridge of {
            id: component_variable; 
            protocol_name: component_variable;
        }
    and literal = _literal placed

    (************************************* Operations ******************************)
    and unop = AstUtils.unop
    and binop = AstUtils.binop
    and block = AstUtils.block
    and block2 = AstUtils.block2

    (************************************ Expr & Stmt *****************************)

    and _expr = 
        | VarExpr of expr_variable 
        | ImplicitVarExpr of expr_variable

        | ActivationAccessExpr of component_variable * expr * expr_variable (* cname, e, x*)
        | AccessExpr of expr * expr (*e1.e2*)
        | BinopExpr of expr * binop * expr 
        | LambdaExpr of expr_variable * main_type * expr 
        | LitExpr of literal
        | UnopExpr of unop * expr 

        (** *)
        (* TODO closure/lamba*)
        | CallExpr of expr * expr list(** method and fct can be recursive *)
        | NewExpr of expr * expr list                   
        | PolyApp of expr * main_type list

        (** Control flow *)

        (* Reflexifity *)
        | This (* current activation *)

        (* Activation lifetime expr *)
        | Spawn of {c: component_expr; args: expr list; at: expr option}  

        (* Structure expr *)
        | BoxCExpr of component_expr

        (** *)
        | OptionExpr of expr option
        | ResultExpr of (expr option * expr option) (* Ok, Err *)
        | BlockExpr of block * expr list
        | Block2Expr of block2 * (expr * expr) list
    and expr = (_expr * main_type) placed

    and _stmt = 
        | EmptyStmt

        (** Binders *)
        | AssignExpr of expr_variable * expr
        | AssignThisExpr of component_variable * expr
        | LetExpr of main_type * expr_variable * expr

        (** Comments *)
        | CommentsStmt of comments

        (** Control flow *)
        | BreakStmt
        | ContinueStmt
        | ExitStmt of int
        | ForStmt of main_type * expr_variable * expr * stmt
        | IfStmt of expr * stmt * stmt option
        | MatchStmt of expr * (expr * stmt) list
        | ReturnStmt of expr

        (**type name, type definition*)
        | ExpressionStmt of expr
        | BlockStmt of stmt list

        | GhostStmt of stmt
    and stmt = _stmt placed

    and _param = main_type * expr_variable
    and param = _param placed

    (******************************** Component **********************************)
    and _port = {
        name: component_variable;
        input: expr;
        expecting_st: main_type;
        callback: expr
    }
    and port = (_port * main_type) placed

    (******************************** Contracts **********************************)
    and _contract = { (* TODO GADT *)
        method_name: component_variable;
        pre_binders: (main_type * expr_variable * expr) list; 
        ensures: expr option;
        returns: expr option;
    }
    and contract = _contract placed


    (********************** Manipulating component structure *********************)
    and _component_expr = 
        | VarCExpr of component_variable  
        (* functor or X(1) *)
        | AppCExpr of component_expr * component_expr 
        | UnboxCExpr of expr
        | AnyExpr of expr
    and component_expr = (_component_expr * main_type) placed

    (* The following annotation requests the automatic generation of a [show_]
    function for each of the types defined above.*)
    [@@deriving show { with_path = false }]



    let rec _dual place : _session_type -> _session_type  = function
    | STEnd -> STEnd
    | STVar _ as st -> st
    | STSend (mt, st) -> STRecv (mt, dual st)
    | STRecv (mt, st) -> STSend (mt, dual st)
    | STBranch choices -> STSelect (List.map (function (x, st, c) -> (x, dual st, c)) choices)
    | STSelect choices -> STBranch (List.map (function (x, st, c) -> (x, dual st, c)) choices)
    | STRec (x, st) -> STRec (x, dual st)
    | STInline x -> STInline x
    and dual st : session_type = 
    { st with value = _dual st.place st.value }

    (* TODO get ride of return fvars and encode it as a selector collector if possible ?? links with stmt ?? *)
    (* collector : env -> expr -> 'a list*)
    let rec collect_expr_expr_ (already_binded:Variable.Set.t) selector (collector:Variable.Set.t -> expr -> 'a list) place (e,mt) : Variable.Set.t * 'a list * (main_type * expr_variable) list = 
        (* Collection *)
        let collected_elts0 = if selector e then collector already_binded {place; value=(e,mt)} else [] in 

        (* Handling scope and propagation *)
        match e with 
        | LambdaExpr (x, mt, e) ->
            let _, collected_elts1, fvars1 = collect_expr_expr (Variable.Set.add x already_binded) selector collector e in
            already_binded, collected_elts0@collected_elts1, fvars1
        | (VarExpr x) | (ImplicitVarExpr x) when Variable.Set.find_opt x already_binded <> None  -> already_binded, collected_elts0, [] 
        | (VarExpr x) | (ImplicitVarExpr x) when Variable.is_builtin x -> already_binded, collected_elts0, [] 
        | (VarExpr x) | (ImplicitVarExpr x)-> 
            logger#error "free var of %s " (Variable.to_string x);
            already_binded, collected_elts0, [mt, x]
        | BoxCExpr _ | LitExpr _ | OptionExpr None | ResultExpr (None, None) |This -> already_binded, collected_elts0, []
        | AccessExpr (e1, {value=VarExpr _, _}) -> (* TODO AccessExpr : expr * Atom.t *)
            let _, collected_elts1, fvars1 = collect_expr_expr already_binded selector collector e1 in
            already_binded, collected_elts1, fvars1

        | AccessExpr (e1, e2) | BinopExpr (e1, _, e2) | ResultExpr (Some e1, Some e2) ->
            let _, collected_elts1, fvars1 = collect_expr_expr already_binded selector collector e1 in
            let _, collected_elts2, fvars2 = collect_expr_expr already_binded selector collector e2 in
            already_binded, collected_elts1@collected_elts2, fvars1@fvars2
        | ActivationAccessExpr (_, e, _) | UnopExpr (_, e) | OptionExpr (Some e) | ResultExpr (Some e, None) | ResultExpr (None, Some e)->
            let _, collected_elts, fvars = collect_expr_expr already_binded selector collector e in
            already_binded, collected_elts0@collected_elts, fvars
        | CallExpr ({value=(VarExpr _,_) }, es) | NewExpr ({value=(VarExpr _, _)}, es) -> (* no first class function nor constructor inside stmt - so we get ride of all possible constructors *)
            let collected_elts, fvars = List.fold_left (fun (acc0, acc1) e -> 
                let _, collected_elts, fvars = collect_expr_expr already_binded selector collector e in
                collected_elts@acc0, fvars@acc1
            ) ([], []) es in 
            already_binded, collected_elts0@collected_elts, fvars
        | CallExpr (e, es) | NewExpr (e, es) | Spawn {args=es; at = Some e} ->
            let _, collected_elts1, fvars1 = collect_expr_expr already_binded selector collector e in
            let collected_elts2, fvars2 = List.fold_left (fun (acc0, acc1) e -> 
                let _, collected_elts, fvars = collect_expr_expr already_binded selector collector e in
                collected_elts@acc0, fvars@acc1
            ) ([], []) es
            in
            already_binded, collected_elts0@collected_elts1@collected_elts2, fvars1@fvars2
        | BlockExpr (_, es) | Spawn {args=es} -> 
            let collected_elts, fvars = List.fold_left (fun (acc0, acc1) e -> 
                let _, collected_elts, fvars = collect_expr_expr already_binded selector collector e in
                collected_elts@acc0, fvars@acc1) (collected_elts0, []) es
            in
            already_binded, collected_elts, fvars
    and collect_expr_expr (already_binded:Variable.Set.t) selector collector expr = 
        map0_place (collect_expr_expr_ already_binded selector collector) expr
    and free_vars_expr already_binded e = 
        let already_binded, _, fvars = collect_expr_expr  already_binded (function e -> false) (fun env e -> []) e in
        already_binded, Utils.deduplicate snd fvars 

    (* TODO FIXME to be used in side fvars_expr/stmt *)
    and collect_expr_mtype already_binded selector collector mt = already_binded, [], [] (* TODO FIXME to be implemented*)
    and free_vars_mtype already_binded mt = already_binded, [] (* TODO FIXME to be implemented*)

    and collect_expr_stmt_ (already_binded:Variable.Set.t) selector collector place = function 
    | EmptyStmt -> already_binded, [], []
    | AssignExpr (x, e) ->
        collect_expr_expr already_binded selector collector e
    | AssignThisExpr (x, e) ->
        collect_expr_expr already_binded selector collector e
    | BlockStmt stmts ->
        let _, res = List.fold_left_map (fun already_binded  stmt -> 
            let already_binded, collected_elts, fvars = collect_expr_stmt already_binded selector collector stmt in
            already_binded, (collected_elts, fvars)
        ) already_binded stmts in 
        let collected_elts = List.map fst res in
        let fvars = List.map snd res in
        already_binded, List.flatten collected_elts, List.flatten fvars
    | BreakStmt -> already_binded, [], []
    | CommentsStmt c -> already_binded, [], []
    | ContinueStmt -> already_binded, [], []
    | ExpressionStmt e -> 
        let _, collected_elts, fvars = collect_expr_expr already_binded selector collector e in
        already_binded, collected_elts, fvars
    | ExitStmt _ -> already_binded, [], []
    | ForStmt (mt, x, e, stmt) ->
        let _, collected_elts1, fvars1 = collect_expr_expr already_binded selector collector e in
        let _, collected_elts2, fvars2 = collect_expr_stmt (Variable.Set.add x already_binded) selector collector stmt in
        already_binded, collected_elts1@collected_elts2,  fvars1@fvars2
    | GhostStmt stmt -> 
        let _, collected_elts, fvars = collect_expr_stmt already_binded selector collector stmt in
        already_binded, collected_elts, fvars
    | IfStmt (e, stmt1, stmt2_opt) -> begin 
        let _, collected_elts0, fvars0 = collect_expr_expr already_binded selector collector e in
        let _, collected_elts1, fvars1 = collect_expr_stmt already_binded selector collector stmt1 in

        match Option.map (collect_expr_stmt already_binded selector collector) stmt2_opt with
        | None ->already_binded, collected_elts0@collected_elts1, fvars0@fvars1
        | Some (_, collected_elts2, fvars2) -> already_binded, collected_elts0@collected_elts1@collected_elts2,fvars0@fvars1@fvars2
    end
    | LetExpr (ct, x, e) -> 
        let already_binded = Variable.Set.add x already_binded in
        let _, collected_elts, fvars = collect_expr_expr already_binded selector collector e in  
        already_binded, collected_elts, fvars 
    | MatchStmt (e, branches) ->
        let _, collected_elts, fvars = collect_expr_expr already_binded selector collector e in
        let collected_elts1, fvars1 = List.fold_left (fun (acc0, acc1) (_,stmt) -> 
            let _, collected_elts, fvars =  collect_expr_stmt already_binded selector collector stmt in
            acc0@collected_elts, acc1@fvars
        ) (collected_elts, fvars) branches in
        already_binded, collected_elts1, fvars1 
    | ReturnStmt e ->
        let _, collected_elts, fvars = collect_expr_expr already_binded selector collector e in
        already_binded, collected_elts, fvars

    and collect_expr_stmt (already_binded:Variable.Set.t) selector collector stmt =       
        map0_place (collect_expr_stmt_ already_binded selector collector) stmt

    and free_vars_stmt already_binded stmt = 
        let already_binded, _, fvars = collect_expr_stmt  already_binded (function e -> false) (fun env e -> []) stmt in
        already_binded, Utils.deduplicate snd fvars 

    (*retrun free  type variable *)
    let rec _free_tvars_ctype already_binded place = function
    | TFlatType _ -> already_binded, []
    | TActivationInfo mt | TArray mt | TList mt | TOption mt | TSet mt | TVPlace mt -> free_tvars_mtype already_binded mt
    | TArrow (mt1, mt2) | TDict (mt1, mt2) | TResult (mt1, mt2) | TUnion (mt1, mt2) | TPort (mt1, mt2) -> 
        let _, ftvars1 = free_tvars_mtype already_binded mt1 in
        let _, ftvars2 = free_tvars_mtype already_binded mt2 in
        already_binded, ftvars1@ftvars2
    | TBridge b -> 
        let _, ftvars1 = free_tvars_mtype already_binded b.in_type in
        let _, ftvars2 = free_tvars_mtype already_binded b.out_type in
        let _, ftvars3 = free_tvars_mtype already_binded b.protocol in
        already_binded, ftvars1@ftvars2@ftvars3
    | TVar x | TPolyVar x when Atom.Set.find_opt x already_binded <> None  -> already_binded, [] 
    | TVar x when Atom.is_builtin x -> already_binded, [] 
    | TVar x | TPolyVar x  -> 
        logger#error "free tvar of %s " (Atom.to_string x);
        already_binded, [x]
    | TForall (x, mt) -> free_tvars_mtype (Atom.Set.add x already_binded) mt
    and free_tvars_ctype (already_binded:Atom.Set.t) ct = 
        let already_binded, fvars = map0_place (_free_tvars_ctype already_binded) ct in
        already_binded, Utils.deduplicate Fun.id fvars 

    and _free_tvars_stype already_binded place = function
    | STEnd -> already_binded, []
    | STVar x | STPolyVar x when Atom.Set.find_opt x already_binded <> None  -> already_binded, []
    | STVar x | STPolyVar x -> already_binded, [x]
    | STRecv (mt, st) | STSend (mt, st) -> 
        let _, ftvars1 = free_tvars_mtype already_binded mt in
        let _, ftvars2 = free_tvars_stype already_binded st in
        already_binded, ftvars1@ftvars2
    | STBranch branches | STSelect branches -> 
        (*TODO FIXME constraint to proccess*)
        already_binded, List.flatten(List.map (function (_,st,_) -> snd (free_tvars_stype already_binded st)) branches)
    | STRec (x, st) -> free_tvars_stype (Atom.Set.add x already_binded) st
    | STInline _ -> raise (Error.DeadbranchError "STInline should have been resolved before running free_tvars")
    and free_tvars_stype (already_binded:Atom.Set.t) st = 
        let already_binded, fvars = map0_place (_free_tvars_stype already_binded) st in
        already_binded, Utils.deduplicate Fun.id fvars

    and _free_tvars_cmtype already_binded place = function
    | CompTUid _ | TPolyCVar _ -> already_binded, [] (*Not a type variable but a component variable *)
    | TStruct sign -> already_binded, List.flatten(List.map (function mt -> snd (free_tvars_mtype already_binded mt)) (List.map snd (List.of_seq (Atom.VMap.to_seq sign))))
    and free_tvars_cmtype (already_binded:Atom.Set.t) cmt = 
        let already_binded, fvars =  map0_place (_free_tvars_cmtype already_binded) cmt in
        already_binded, Utils.deduplicate Fun.id fvars

    and _free_tvars_mtype already_binded place = function
    | EmptyMainType -> already_binded, []
    | CType ct -> free_tvars_ctype already_binded ct
    | SType st -> free_tvars_stype already_binded st
    | CompType cmt -> free_tvars_cmtype already_binded cmt
    | ConstrainedType (mt,ac) -> free_tvars_mtype already_binded mt (* TODO FIXME tvars in ac to proccessed yet *)
    
    and free_tvars_mtype (already_binded:Atom.Set.t) mt = 
        let already_binded, fvars =  map0_place (_free_tvars_mtype already_binded) mt in
        already_binded, Utils.deduplicate Fun.id fvars


    let rec timers_of_headers = function
        | [] -> []
        | {value=UseMetadata _} ::headers-> timers_of_headers headers
        | {value=SetTimer x}::headers -> x::(timers_of_headers headers)
        | {value=SetFireTimer (x,_)}::headers -> raise (Error.DeadbranchError "SetFireTimer should exists before GuardTransform - not supported yet") 
    and timers_of_st_ = function
    | STEnd -> []
    | STRecv ({value=ConstrainedType (_, (guard_headers, _))}, st) | STSend ({value=ConstrainedType (_, (guard_headers, _))}, st) -> 
        (timers_of_headers guard_headers) @ (timers_of_st st)
    and timers_of_st st = timers_of_st_ st.value

    let rec replace_type_place (replace_type_value : Error.place -> 'a -> 'a) ({ AstUtils.place ; AstUtils.value}: 'a AstUtils.placed) = 
        let value = replace_type_value place value in
        {AstUtils.place; AstUtils.value}

    let rec _replace_type_composed_type x_to_replace ((replaceby_x_opt, _)as replaceby) place : _composed_type -> _composed_type = function
        | TActivationInfo mt -> TActivationInfo (replace_type_main_type x_to_replace replaceby mt) 
        | TArrow (mt1, mt2) -> TArrow (
            replace_type_main_type x_to_replace replaceby mt1,
            replace_type_main_type x_to_replace replaceby mt2
        ) 
        | TVar x when x = x_to_replace && replaceby_x_opt <> None -> TVar (Option.get replaceby_x_opt)
        | TPolyVar x when x = x_to_replace && replaceby_x_opt <> None -> TPolyVar (Option.get replaceby_x_opt)
        | (TVar _ as t) | (TBridge _ as t) | (TRaw _ as t) | (TFlatType _ as t) | (TPolyVar _ as t) -> t
        | TArray mt -> TArray (replace_type_main_type x_to_replace replaceby mt)  
        | TDict (mt1, mt2) -> TDict (
            replace_type_main_type x_to_replace replaceby mt1,
            replace_type_main_type x_to_replace replaceby mt2
        ) 
        | TList mt -> TList (replace_type_main_type x_to_replace replaceby mt) 
        | TOption mt -> TOption (replace_type_main_type x_to_replace replaceby mt)
        | TResult (mt1, mt2) -> TResult (
            replace_type_main_type x_to_replace replaceby mt1,
            replace_type_main_type x_to_replace replaceby mt2
        ) 
        | TSet mt -> TSet (replace_type_main_type x_to_replace replaceby mt)
        | TTuple mts -> TTuple (
            List.map (
                replace_type_main_type x_to_replace replaceby
            ) mts
        ) 
        | TVPlace mt -> TVPlace (replace_type_main_type x_to_replace replaceby mt)
        | TUnion (mt1, mt2) -> TUnion (
            replace_type_main_type x_to_replace replaceby mt1,
            replace_type_main_type x_to_replace replaceby mt2
        ) 
        | TForall (x, mt) when x = x_to_replace && replaceby_x_opt <> None -> TForall (Option.get replaceby_x_opt, replace_type_main_type x_to_replace replaceby mt)
        | TForall (x, mt) -> TForall (x, replace_type_main_type x_to_replace replaceby mt)
    and replace_type_composed_type x_to_replace replaceby = replace_type_place (_replace_type_composed_type x_to_replace replaceby)
    
    and _replace_type_session_type x_to_replace ((replaceby_x_opt, _)as replaceby) place = function
    | STEnd -> STEnd 
    | STVar x when x = x_to_replace && replaceby_x_opt <> None -> STVar (Option.get replaceby_x_opt) 
    | STVar x -> STVar x 
    | STSend (mt, st) -> STSend (
        replace_type_main_type x_to_replace replaceby mt,
        replace_type_session_type x_to_replace replaceby st
    ) 
    | STRecv (mt, st) -> STRecv (
        replace_type_main_type x_to_replace replaceby mt,
        replace_type_session_type x_to_replace replaceby st
    ) 
    | STBranch branches -> STBranch (
        List.map (function (l, st, guard_opt) ->
            l,
            replace_type_session_type x_to_replace replaceby st,
            guard_opt (*Not rewrite here for now TODO*)
        ) branches
    )            
    | STSelect branches -> STSelect (
        List.map (function (l, st, guard_opt) ->
            l,
            replace_type_session_type x_to_replace replaceby st,
            guard_opt
        ) branches
    )               

    | STRec (x, st) when x = x_to_replace && replaceby_x_opt <> None -> STRec (
        Option.get replaceby_x_opt,
        replace_type_session_type x_to_replace replaceby st
    ) 
    | STRec (x, st) -> STRec (
        x,
        replace_type_session_type x_to_replace replaceby st
    ) 
    | STInline x -> STInline x 
    and replace_type_session_type x_to_replace replaceby = replace_type_place (_replace_type_session_type x_to_replace replaceby)

    and _replace_type_main_type (x_to_replace:type_variable) ((replaceby_x, replaceby_e_opt)as replaceby) place = function
    | CType {value=TVar x} when x = x_to_replace && replaceby_e_opt <> None -> 
        Option.get replaceby_e_opt
    | CType {value=TPolyVar x} when x = x_to_replace && replaceby_e_opt <> None -> 
        Option.get replaceby_e_opt
    | CType ct -> CType (replace_type_composed_type x_to_replace replaceby ct)
    | SType st -> SType (replace_type_session_type x_to_replace replaceby st) 
    and replace_type_main_type (x_to_replace:type_variable) (replaceby:type_variable option * _main_type option) = replace_type_place (_replace_type_main_type x_to_replace replaceby)


    let rec _replace_stype_session_type x_to_replace ((replaceby_x_opt, replaceby_e_opt)as replaceby) place = function
    | STEnd -> STEnd 
    | STVar x when x = x_to_replace && replaceby_x_opt <> None -> STVar (Option.get replaceby_x_opt) 
    | STVar x when x = x_to_replace && replaceby_e_opt <> None -> (Option.get replaceby_e_opt) 
    | STVar x -> STVar x 
    | STSend (mt, st) -> STSend (
        mt,
        replace_stype_session_type x_to_replace replaceby st
    ) 
    | STRecv (mt, st) -> STRecv (
        mt,
        replace_stype_session_type x_to_replace replaceby st
    ) 
    | STBranch branches -> STBranch (
        List.map (function (l, st, guard_opt) ->
            l,
            replace_stype_session_type x_to_replace replaceby st,
            guard_opt (*Not rewrite here for now TODO*)
        ) branches
    )            
    | STSelect branches -> STSelect (
        List.map (function (l, st, guard_opt) ->
            l,
            replace_stype_session_type x_to_replace replaceby st,
            guard_opt
        ) branches
    )               

    | STRec (x, st) when x = x_to_replace && replaceby_x_opt <> None -> STRec (
        Option.get replaceby_x_opt,
        replace_stype_session_type x_to_replace replaceby st
    ) 
    | STRec (x, st) -> STRec (
        x,
        replace_stype_session_type x_to_replace replaceby st
    ) 
    | STInline x -> STInline x 
    and replace_stype_session_type x_to_replace replaceby = replace_type_place (_replace_stype_session_type x_to_replace replaceby)





    let rec rewrite_expr_place rewrite_expr_value { AstUtils.place ; AstUtils.value} = 
        let _value = rewrite_expr_value place (fst value) in
        {AstUtils.place; AstUtils.value = _value, snd value}

    let rec _rewrite_expr_expr selector rewriter place = function
    | e when selector e -> rewriter e
    | (VarExpr _ as e) | (ImplicitVarExpr _ as e) -> e
    | ActivationAccessExpr (cname, e, mname) ->
        ActivationAccessExpr(
            cname,
            rewrite_expr_expr selector rewriter e,
            mname
        )
    | AccessExpr (e1, e2) -> AccessExpr (
        rewrite_expr_expr selector rewriter e1,
        rewrite_expr_expr selector rewriter e2
    )
    | BinopExpr (e1, op, e2) -> BinopExpr (
        rewrite_expr_expr selector rewriter e1,
        op,
        rewrite_expr_expr selector rewriter e2
    )
    | LambdaExpr (x, mt, e) -> LambdaExpr (
        x,
        mt, (* WARNIN TODO FIXME replace in type predicates *)
        rewrite_expr_expr selector rewriter e
    )
    | LitExpr _ as e -> e
    | UnopExpr (op, e) -> UnopExpr (op, rewrite_expr_expr selector rewriter e)
    | CallExpr (e, es) -> CallExpr(
        rewrite_expr_expr selector rewriter e,
        List.map (rewrite_expr_expr selector rewriter) es
    )
    | NewExpr (e, es) -> NewExpr(
        rewrite_expr_expr selector rewriter e,
        List.map (rewrite_expr_expr selector rewriter) es
    )
    | This -> This 
    | Spawn sp -> Spawn { sp with 
        args = List.map (rewrite_expr_expr selector rewriter) sp.args;
        at = Option.map (rewrite_expr_expr selector rewriter) sp.at
    }
    | BoxCExpr _ as e -> e
    | OptionExpr e_opt -> OptionExpr (
        Option.map (rewrite_expr_expr selector rewriter) e_opt
    ) 
    | ResultExpr (e1_opt, e2_opt) -> ResultExpr (
        Option.map (rewrite_expr_expr selector rewriter) e1_opt,
        Option.map (rewrite_expr_expr selector rewriter) e2_opt
    ) 
    | BlockExpr(b, es) -> BlockExpr (b,
        List.map (rewrite_expr_expr selector rewriter) es 
    )
    | Block2Expr(b, ees) -> Block2Expr (b,
        List.map (function (e1,e2) ->
            rewrite_expr_expr selector rewriter e1,
            rewrite_expr_expr selector rewriter e2
        ) ees 
    )
    and rewrite_expr_expr selector rewriter = rewrite_expr_place (_rewrite_expr_expr selector rewriter)

    and _rewrite_expr_stmt selector rewriter place = function 
        | EmptyStmt -> EmptyStmt
        | AssignExpr (x, e) -> AssignExpr (x, rewrite_expr_expr selector rewriter e) 
        | AssignThisExpr (x, e) -> AssignThisExpr (x, rewrite_expr_expr selector rewriter e)
        | LetExpr (mt, x, e) -> (* TODO FIXME expr in type are not yet concerned *)
            LetExpr (mt, x, rewrite_expr_expr selector rewriter e)
        | CommentsStmt c -> CommentsStmt c
        | BreakStmt -> BreakStmt
        | ContinueStmt -> ContinueStmt
        | ExitStmt i -> ExitStmt i
        | ForStmt (mt, x, e, stmt) -> (* TODO FIXME expr in type are not yet concerned *)
            ForStmt(mt, x, 
                rewrite_expr_expr selector rewriter e,
                rewrite_expr_stmt selector rewriter stmt)
        | IfStmt (e, stmt1, stmt2_opt) ->
            IfStmt (
                rewrite_expr_expr selector rewriter e,
                rewrite_expr_stmt selector rewriter stmt1,
                Option.map (rewrite_expr_stmt selector rewriter) stmt2_opt
            )
        | MatchStmt (e, branches) ->
            MatchStmt (
                rewrite_expr_expr selector rewriter e,
                List.map (function (e, stmt) ->
                    rewrite_expr_expr selector rewriter e,
                    rewrite_expr_stmt selector rewriter stmt
                ) branches
            )
        | ReturnStmt e -> ReturnStmt (rewrite_expr_expr selector rewriter e) 
        | ExpressionStmt e -> ExpressionStmt (rewrite_expr_expr selector rewriter e) 
        | BlockStmt stmts -> BlockStmt (List.map (rewrite_expr_stmt selector rewriter) stmts) 
        | GhostStmt stmt -> GhostStmt (rewrite_expr_stmt selector rewriter stmt)
    and rewrite_expr_stmt selector rewriter = map_place (_rewrite_expr_stmt selector rewriter)
                    
    (* Warning do not replace ImplictVar !!! *)
    let make x_to_replace ((replaceby_x_opt, replaceby_e_opt)as replaceby) = 
        let selector = function |VarExpr x when x = x_to_replace -> true | _ -> false in
        let rewriter e = match replaceby_x_opt with | Some x -> VarExpr x | None -> Option.get replaceby_e_opt in
        selector, rewriter
    let replace_expr_expr x_to_replace replaceby = 
        let selector, rewriter = make x_to_replace replaceby in
        rewrite_expr_expr selector rewriter
    let replace_expr_stmt x_to_replace replaceby = 
        let selector, rewriter = make x_to_replace replaceby in
        rewrite_expr_stmt selector rewriter





    (* elim place *)
    let rec equal_place inner_equal (x1:'a AstUtils.placed) (x2:'a AstUtils.placed) = inner_equal (x1.value, x2.value)

    let rec _equal_ctype = function 
    | TActivationInfo mt1, TActivationInfo mt2 | TArray mt1, TArray mt2 | TList mt1, TList mt2 | TOption mt1, TOption mt2 | TSet mt1, TSet mt2 | TVPlace mt1, TVPlace mt2 ->
        equal_mtype mt1 mt2
    | TArrow (mt1_a, mt1_b), TArrow (mt2_a, mt2_b) | TDict (mt1_a, mt1_b), TDict (mt2_a, mt2_b) | TResult (mt1_a, mt1_b), TResult (mt2_a, mt2_b) | TUnion (mt1_a, mt1_b), TUnion (mt2_a, mt2_b) ->
        equal_mtype mt1_a mt2_a && equal_mtype mt1_b mt1_b
    | TVar x1, TVar x2 -> x1 = x2
    | TFlatType TInt, TFlatType TTimer | TFlatType TTimer, TFlatType TInt -> true 
    | TFlatType ft1, TFlatType ft2 -> ft1 = ft2
    | TTuple mts1, TTuple mts2 -> List.equal equal_mtype mts1 mts2
    | TBridge b1, TBridge b2 ->
        equal_mtype b1.in_type b2.in_type &&
        equal_mtype b1.out_type b2.out_type &&
        equal_mtype b1.protocol b2.protocol
    | TRaw r1, TRaw r2 -> r1 = r2
    | TPolyVar x1, TPolyVar x2 -> x1 = x2
    | TForall (x1, mt1), TForall (x2, mt2) ->
        let mt2' = replace_type_main_type x2 (Some x1, None) mt2 in
        equal_mtype mt1 mt2'
    | _ -> false
    and equal_ctype ct1 ct2 = 
    (=) ct1 ct2 || (* optimization *)
    equal_place _equal_ctype ct1 ct2

    and _equal_stype = function
    | STEnd, STEnd -> true 
    | STSend (mt1,st1), STSend (mt2, st2) | STRecv (mt1,st1), STRecv (mt2, st2) ->
        equal_mtype mt1 mt2 && equal_stype st1 st2
    | STSelect branches1, STSelect branches2 | STBranch branches1, STBranch branches2->
        List.equal (fun (x1, st1, guard_opt1) (x2, st2, guard_opt2) ->
            x1 = x2 &&
            equal_stype st1 st2 &&
            Option.equal equal_applied_constraint guard_opt1  guard_opt2
        ) branches1 branches2
    | STVar x1, STVar x2 -> x1 = x2
    | STRec (x1, st1), STRec (x2, st2) ->
        let st2' = replace_stype_session_type x2 (Some x1, None) st2 in
        equal_stype st1 st2'
    | STInline x1, STInline x2 -> x1 = x2
    | STPolyVar x1, STPolyVar x2 -> x1 = x2
    | _ -> false
    and equal_stype st1 st2 = 
    (=) st1 st2 ||
    equal_place _equal_stype st1 st2


    and _equal_cmtype = function
    | CompTUid x1, CompTUid x2 -> x1 = x2
    | TStruct struct1, TStruct struct2 -> failwith "FIXME TODO equality for TStruct i.e. replace list by hashtbl"
    | TPolyCVar x1, TPolyCVar x2 -> x1 = x2
    | _ -> false
    and equal_cmtype cmt1 cmt2 =
    (=) cmt1 cmt2 ||
    equal_place _equal_cmtype cmt1 cmt2

    and _equal_mtype = function
    | EmptyMainType, EmptyMainType -> true 
    | CType ct1, CType ct2 -> equal_ctype ct1 ct2
    | SType st1, SType st2 -> equal_stype st1 st2
    | CompType cmt1, CompType cmt2 -> equal_cmtype cmt1 cmt2
    | ConstrainedType (mt1, ac1), ConstrainedType (mt2, ac2) ->
        equal_mtype mt1 mt2 &&
        failwith "TODO equal guard not yet defined"
    | _ -> false
    and equal_mtype mt1 mt2 = 
    (=) mt1 mt2 ||
    equal_place _equal_mtype mt1 mt2

    (* return flag * variable to replace for equality of the second argument*)
    and _equal_header = function
    | UseMetadata (mt1, x1), UseMetadata (mt2, x2) ->
        equal_mtype mt1 mt2, (x1,x2)
    | SetTimer x1, SetTimer x2 -> true, (x1, x2)
    | SetFireTimer (x1, i1), SetFireTimer (x2, i2) -> i1 = i2, (x1, x2)
    and equal_header (h1, h2) = 
    equal_place _equal_header h1 h2

    and equal_applied_constraint (hs1, g1_opt) (hs2, g2_opt) = 
        let tmp = List.map equal_header (List.combine hs1 hs2) in
        let flag = List.exists (function | (false,_)-> false | _ -> true) tmp in
        let vars = snd(List.split tmp) in
        
        let g2_opt' : constraints option = Option.map (function g2 ->
            {
                place = g2.place;
                value = List.fold_left (fun (e2, mt) (x1, x2) -> 
                (replace_expr_expr x2 (Some x1, None) {place=g2.place;value=(e2, mt)}).value    
            ) g2.value vars
            }

        ) g2_opt in

        (* TODO FIXME renaming is not propgated to the remaining part of the protocol .......... Protocol definition (with headers should refactorized) 
        TODO WARNING
        *)


        flag &&
        Option.equal (fun {value=(e1,_)} {value=(e2, _)} -> _equal_expr (e1, e2)) g1_opt g2_opt'


    and _equal_expr = function
    | VarExpr x1, VarExpr x2 -> x1 = x2
    | AccessExpr (e1_a, e1_b), AccessExpr (e2_a, e2_b) ->
        equal_expr e1_a e2_a &&
        equal_expr e1_b e2_b
    | BinopExpr (e1_a, op1, e1_b), BinopExpr (e2_a, op2, e2_b) ->
        equal_expr e1_a e2_a &&
        op1 = op2 &&
        equal_expr e1_b e2_b
    | LambdaExpr (x1, mt1, e1), LambdaExpr (x2, mt2, e2) -> 
        let e2' = replace_expr_expr x2 (Some x1, None) e2 in

        equal_mtype mt1 mt2 &&
        equal_expr e1 e2'
    | LitExpr l1, LitExpr l2 -> l1 = l2
    | UnopExpr(op1, e1), UnopExpr (op2, e2) -> 
        op1 = op2 &&
        equal_expr e1 e2
    | CallExpr (e1, es1), CallExpr (e2, es2) | NewExpr (e1, es1), NewExpr (e2, es2) ->
        equal_expr e1 e2 &&
        List.equal equal_expr es1 es2 
    | This, This -> true
    | Spawn sp1, Spawn sp2 ->
        equal_cexpr sp1.c sp2.c &&
        List.equal equal_expr sp1.args sp2.args &&
        Option.equal equal_expr sp1.at sp2.at
    | BoxCExpr ce1, BoxCExpr ce2 -> equal_cexpr ce1 ce2
    | OptionExpr e1_opt, OptionExpr e2_opt -> Option.equal equal_expr e1_opt e2_opt
    | ResultExpr (e1a_opt, e1b_opt), ResultExpr (e2a_opt, e2b_opt) -> 
        Option.equal equal_expr e1a_opt e2a_opt &&
        Option.equal equal_expr e1b_opt e2b_opt
    | BlockExpr (b1, es1), BlockExpr (b2, es2) ->
        b1 = b2 &&
        List.equal equal_expr es1 es2
    | Block2Expr (b1, ees1), Block2Expr (b2, ees2) ->
        b1 = b2 &&
        List.equal (fun (e1a,e1b) (e2a, e2b) -> 
            equal_expr e1a e2a &&
            equal_expr e2a e2b
        ) ees1 ees2
    | _ -> false
    and equal_expr (e1:expr) (e2:expr) =
    (=) e1 e2 ||
    _equal_expr ((fst e1.value), (fst e2.value))

    and _equal_cexpr = function
    | VarCExpr x1, VarCExpr x2 -> x1 = x2
    | AppCExpr (ce1a, ce1b), AppCExpr (ce2a, ce2b) ->
        equal_cexpr ce1a ce2a &&
        equal_cexpr ce1b  ce2b
    | UnboxCExpr e1, UnboxCExpr e2 -> equal_expr e1 e2
    | AnyExpr e1, AnyExpr e2 -> equal_expr e1 e2
    and equal_cexpr ce1 ce2 = 
    (=) ce1 ce2 ||
    _equal_cexpr ((fst ce1.value), (fst ce2.value))


    (* Return form do not start with an µx.*)
    let rec _unfold_st_star = function
    | STRec (x, st) ->
        let st = replace_stype_session_type x (None, Some st.value) st in
        st.value
    | st -> st
    and unfold_st_star st = {place = st.place; value = _unfold_st_star st.value}
end
