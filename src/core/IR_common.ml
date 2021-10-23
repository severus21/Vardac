open AstUtils
open Easy_logging

(************************************* Base types ****************************)
open Label

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

        | TRaw of Impl_common.blackbox_term (*TODO move it to IRI by doing so composed type should not be any more in common *)

        (* Polymorphsim*)
        | TPolyVar of type_variable
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
        | TStruct of main_type list (*types of states, methods, ports, ... and subcomponents *)
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

        | AccessExpr of expr * expr (*e1.e2*)
        | BinopExpr of expr * binop * expr 
        | LambdaExpr of expr_variable * main_type * expr 
        | LitExpr of literal
        | UnopExpr of unop * expr 

        (** *)
        (* TODO closure/lamba*)
        | CallExpr of expr * expr list(** method and fct can be recursive *)
        | NewExpr of expr * expr list                   

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
    and port = _port placed

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
    val free_vars_stmt : Variable.Set.t -> stmt -> Variable.Set.t * expr_variable list
    val  timers_of_headers : constraint_header list -> expr_variable list
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
        | TStruct of main_type list (*types of states, methods, ports, ... and subcomponents *)
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

        | AccessExpr of expr * expr (*e1.e2*)
        | BinopExpr of expr * binop * expr 
        | LambdaExpr of expr_variable * main_type * expr 
        | LitExpr of literal
        | UnopExpr of unop * expr 

        (** *)
        (* TODO closure/lamba*)
        | CallExpr of expr * expr list(** method and fct can be recursive *)
        | NewExpr of expr * expr list                   

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
    and port = _port placed

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

    let rec free_variable_place free_variable_value ({ AstUtils.place ; AstUtils.value}: 'a AstUtils.placed) = 
        free_variable_value place value

    let rec free_vars_expr_ (already_binded:Variable.Set.t) place (e,mt) : Variable.Set.t * expr_variable list = 
        match e with 
        | LambdaExpr (x, mt, e) ->
            already_binded, snd (free_vars_expr (Variable.Set.add x already_binded) e)
        | VarExpr x when Variable.Set.find_opt x already_binded <> None  -> already_binded, [] 
        | VarExpr x when Variable.is_builtin x -> already_binded, [] 
        | VarExpr x -> 
            logger#error "free var of %s " (Variable.to_string x);
            already_binded, [x]
        | BoxCExpr _ | LitExpr _ | OptionExpr None | ResultExpr (None, None) |This -> already_binded, []
        | AccessExpr (e1, e2) | BinopExpr (e1, _, e2) | ResultExpr (Some e1, Some e2) ->
            let _, fvars1 = free_vars_expr already_binded e1 in
            let _, fvars2 = free_vars_expr already_binded e2 in
            already_binded, fvars1@fvars2
        | UnopExpr (_, e) | OptionExpr (Some e) | ResultExpr (Some e, None) | ResultExpr (None, Some e)->
            let _, fvars = free_vars_expr already_binded e in
            already_binded, fvars
        | CallExpr ({value=(VarExpr _,_) }, es) | NewExpr ({value=(VarExpr _, _)}, es) -> (* no first class function nor constructor inside stmt - so we get ride of all possible constructors *)
            already_binded, List.fold_left (fun acc e -> acc @ (snd (free_vars_expr already_binded e))) [] es
        | CallExpr (e, es) | NewExpr (e, es) | Spawn {args=es; at = Some e} ->
            let _, fvars = free_vars_expr already_binded e in
            already_binded, List.fold_left (fun acc e -> acc @ (snd (free_vars_expr already_binded e))) fvars es
        | BlockExpr (_, es) | Spawn {args=es} -> 
            already_binded, List.fold_left (fun acc e -> acc @ (snd (free_vars_expr already_binded e))) [] es
    and free_vars_expr (already_binded:Variable.Set.t) expr : Variable.Set.t * expr_variable list = free_variable_place (free_vars_expr_ already_binded) expr

    and free_vars_stmt_ (already_binded:Variable.Set.t) place : _stmt -> Variable.Set.t * expr_variable list = function 
    | EmptyStmt -> already_binded, []
    | AssignExpr (x, e) ->
        free_vars_expr already_binded e
    | AssignThisExpr (x, e) ->
        free_vars_expr already_binded e
    | BlockStmt stmts ->
        let already_binded, fvars = List.fold_left_map (fun already_binded stmt -> free_vars_stmt already_binded stmt) already_binded stmts in 
        already_binded, List.flatten fvars
    | BreakStmt -> already_binded, []
    | CommentsStmt c -> already_binded, []
    | ContinueStmt -> already_binded, []
    | ExpressionStmt e -> 
        let _, fvars = free_vars_expr already_binded e in
        already_binded, fvars
    | ExitStmt _ -> already_binded, []
    | ForStmt (mt, x, e, stmt) ->
        let _, fvars1 = free_vars_expr already_binded e in
        let _, fvars2 = free_vars_stmt (Variable.Set.add x already_binded) stmt in
        already_binded, fvars1@fvars2
    | GhostStmt stmt -> 
        let _, fvars = free_vars_stmt already_binded stmt in
        already_binded, fvars
    | IfStmt (e, stmt1, stmt2_opt) -> 
        let _, fvars0 = free_vars_expr already_binded e in
        let _, fvars1 = free_vars_stmt already_binded stmt1 in
        let ret2_opt = Option.map (free_vars_stmt already_binded) stmt2_opt in
        already_binded, fvars0@fvars1@(if ret2_opt = None then [] else snd (Option.get ret2_opt))
    | LetExpr (ct, x, e) -> 
        let already_binded = Variable.Set.add x already_binded in
        let _, fvars = free_vars_expr already_binded e in  
        already_binded, fvars 
    | MatchStmt (e, branches) ->
        let _, fvars = free_vars_expr already_binded e in
        already_binded, List.fold_left (fun acc (_,stmt) -> acc @ (snd (free_vars_stmt already_binded stmt))) fvars branches 
    | ReturnStmt e ->
        let _, fvars = free_vars_expr already_binded e in
        already_binded, fvars

    and free_vars_stmt (already_binded:Variable.Set.t) stmt : Variable.Set.t * expr_variable list = free_variable_place (free_vars_stmt_ already_binded) stmt

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
        | (TVar _ as t) | (TBridge _ as t) | (TRaw _ as t) | (TFlatType _ as t) -> t
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
    | CType ct -> CType (replace_type_composed_type x_to_replace replaceby ct)
    | SType st -> SType (replace_type_session_type x_to_replace replaceby st) 
    and replace_type_main_type (x_to_replace:type_variable) (replaceby:type_variable option * _main_type option) = replace_type_place (_replace_type_main_type x_to_replace replaceby)
end