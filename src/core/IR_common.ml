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
    val free_vars_expr : Variable.Set.t -> expr -> Variable.Set.t * (main_type*expr_variable) list
    val free_vars_stmt : Variable.Set.t -> stmt -> Variable.Set.t * (main_type*expr_variable) list
    val free_vars_mtype : Variable.Set.t -> main_type -> Variable.Set.t * (main_type*expr_variable) list
    val free_tvars_mtype : Atom.Set.t -> main_type -> type_variable list
    val  timers_of_headers : constraint_header list -> expr_variable list
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

    let rec free_vars_expr_ (already_binded:Variable.Set.t) place (e,mt) : Variable.Set.t * (main_type*expr_variable) list = 
        match e with 
        | LambdaExpr (x, mt, e) ->
            already_binded, snd (free_vars_expr (Variable.Set.add x already_binded) e)
        | VarExpr x when Variable.Set.find_opt x already_binded <> None  -> already_binded, [] 
        | VarExpr x when Variable.is_builtin x -> already_binded, [] 
        | VarExpr x -> 
            logger#error "free var of %s " (Variable.to_string x);
            already_binded, [mt, x]
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
    and free_vars_expr (already_binded:Variable.Set.t) expr : Variable.Set.t * (main_type*expr_variable) list = map0_place (free_vars_expr_ already_binded) expr

    (* TODO FIXME to be used in side fvars_expr/stmt *)
    and free_vars_mtype already_binded mt = already_binded, [] (* TODO FIXME to be implemented*)

    and free_vars_stmt_ (already_binded:Variable.Set.t) place = function 
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

    and free_vars_stmt (already_binded:Variable.Set.t) stmt = map0_place (free_vars_stmt_ already_binded) stmt

    (*retrun free  type variable *)
    let rec _free_tvars_ctype already_binded place = function
    | TFlatType _ -> []
    | TActivationInfo mt | TArray mt | TList mt | TOption mt | TSet mt | TVPlace mt -> free_tvars_mtype already_binded mt
    | TArrow (mt1, mt2) | TDict (mt1, mt2) | TResult (mt1, mt2) | TUnion (mt1, mt2) | TPort (mt1, mt2) -> 
        let ftvars1 = free_tvars_mtype already_binded mt1 in
        let ftvars2 = free_tvars_mtype already_binded mt2 in
        ftvars1@ftvars2
    | TBridge b -> 
        let ftvars1 = free_tvars_mtype already_binded b.in_type in
        let ftvars2 = free_tvars_mtype already_binded b.out_type in
        let ftvars3 = free_tvars_mtype already_binded b.protocol in
        ftvars1@ftvars2@ftvars3
    | TVar x | TPolyVar x when Atom.Set.find_opt x already_binded <> None  -> [] 
    | TVar x when Atom.is_builtin x -> [] 
    | TVar x | TPolyVar x  -> 
        logger#error "free tvar of %s " (Atom.to_string x);
        [x]
    | TForall (x, mt) -> free_tvars_mtype (Atom.Set.add x already_binded) mt
    and free_tvars_ctype (already_binded:Atom.Set.t) = map0_place (_free_tvars_ctype already_binded) 

    and _free_tvars_stype already_binded place = function
    | STEnd -> []
    | STVar x | STPolyVar x when Atom.Set.find_opt x already_binded <> None  -> []
    | STVar x | STPolyVar x -> [x]
    | STRecv (mt, st) | STSend (mt, st) -> 
        let ftvars1 = free_tvars_mtype already_binded mt in
        let ftvars2 = free_tvars_stype already_binded st in
        ftvars1@ftvars2
    | STBranch branches | STSelect branches -> 
        (*TODO FIXME constraint to proccess*)
        List.flatten(List.map (function (_,st,_) -> free_tvars_stype already_binded st) branches)
    | STRec (x, st) -> free_tvars_stype (Atom.Set.add x already_binded) st
    | STInline _ -> raise (Error.DeadbranchError "STInline should have been resolved before running free_tvars")
    and free_tvars_stype (already_binded:Atom.Set.t) = map0_place (_free_tvars_stype already_binded) 

    and _free_tvars_cmtype already_binded place = function
    | CompTUid _ | TPolyCVar _ -> [] (*Not a type variable but a component variable *)
    | TStruct sign -> List.flatten(List.map (free_tvars_mtype already_binded) (List.map snd (List.of_seq (Atom.VMap.to_seq sign))))
    and free_tvars_cmtype (already_binded:Atom.Set.t) = map0_place (_free_tvars_cmtype already_binded) 

    and _free_tvars_mtype already_binded place = function
    | EmptyMainType -> []
    | CType ct -> free_tvars_ctype already_binded ct
    | SType st -> free_tvars_stype already_binded st
    | CompType cmt -> free_tvars_cmtype already_binded cmt
    | ConstrainedType (mt,ac) -> free_tvars_mtype already_binded mt (* TODO FIXME tvars in ac to proccessed yet *)
    
    and free_tvars_mtype (already_binded:Atom.Set.t) = map0_place (_free_tvars_mtype already_binded) 


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





    let rec replace_expr_place replace_expr_value { AstUtils.place ; AstUtils.value} = 
        let _value = replace_expr_value place (fst value) in
        {AstUtils.place; AstUtils.value = _value, snd value}

    let rec _replace_expr_expr x_to_replace ((replaceby_x_opt, replaceby_e_opt)as replaceby) place = function
    | VarExpr x  when x = x_to_replace && replaceby_x_opt <> None -> VarExpr (Option.get replaceby_x_opt)
    | VarExpr x  when x = x_to_replace && replaceby_e_opt <> None -> Option.get replaceby_e_opt
    | VarExpr _ as e -> e
    | AccessExpr (e1, e2) -> AccessExpr (
        replace_expr_expr x_to_replace replaceby e1,
        replace_expr_expr x_to_replace replaceby e2
    )
    | BinopExpr (e1, op, e2) -> BinopExpr (
        replace_expr_expr x_to_replace replaceby e1,
        op,
        replace_expr_expr x_to_replace replaceby e2
    )
    | LambdaExpr (x, mt, e) -> LambdaExpr (
        x,
        mt, (* WARNIN TODO FIXME replace in type predicates *)
        replace_expr_expr x_to_replace replaceby e
    )
    | LitExpr _ as e -> e
    | UnopExpr (op, e) -> UnopExpr (op, replace_expr_expr x_to_replace replaceby e)
    | CallExpr (e, es) -> CallExpr(
        replace_expr_expr x_to_replace replaceby e,
        List.map (replace_expr_expr x_to_replace replaceby) es
    )
    | NewExpr (e, es) -> NewExpr(
        replace_expr_expr x_to_replace replaceby e,
        List.map (replace_expr_expr x_to_replace replaceby) es
    )
    | This -> This 
    | Spawn sp -> Spawn { sp with 
        args = List.map (replace_expr_expr x_to_replace replaceby) sp.args;
        at = Option.map (replace_expr_expr x_to_replace replaceby) sp.at
    }
    | BoxCExpr _ as e -> e
    | OptionExpr e_opt -> OptionExpr (
        Option.map (replace_expr_expr x_to_replace replaceby) e_opt
    ) 
    | ResultExpr (e1_opt, e2_opt) -> ResultExpr (
        Option.map (replace_expr_expr x_to_replace replaceby) e1_opt,
        Option.map (replace_expr_expr x_to_replace replaceby) e2_opt
    ) 
    | BlockExpr(b, es) -> BlockExpr (b,
        List.map (replace_expr_expr x_to_replace replaceby) es 
    )
    | Block2Expr(b, ees) -> Block2Expr (b,
        List.map (function (e1,e2) ->
            replace_expr_expr x_to_replace replaceby e1,
            replace_expr_expr x_to_replace replaceby e2
        ) ees 
    )
    and replace_expr_expr x_to_replace replaceby = replace_expr_place (_replace_expr_expr x_to_replace replaceby)


    and _replace_expr_stmt x_to_replace ((replaceby_x_opt, replaceby_e_opt)as replaceby) place = function 
        | EmptyStmt -> EmptyStmt
        | AssignExpr (x, e) -> AssignExpr (x, replace_expr_expr x_to_replace replaceby e) 
        | AssignThisExpr (x, e) -> AssignThisExpr (x, replace_expr_expr x_to_replace replaceby e)
        | LetExpr (mt, x, e) when x = x_to_replace -> failwith "can not replace explictly binded variable"
        | LetExpr (mt, x, e) -> (* TODO FIXME expr in type are not yet concerned *)
            LetExpr (mt, x, replace_expr_expr x_to_replace replaceby e)
        | CommentsStmt c -> CommentsStmt c
        | BreakStmt -> BreakStmt
        | ContinueStmt -> ContinueStmt
        | ExitStmt i -> ExitStmt i
        | ForStmt (mt, x, e, stmt) -> (* TODO FIXME expr in type are not yet concerned *)
            ForStmt(mt, x, 
                replace_expr_expr x_to_replace replaceby e,
                replace_expr_stmt x_to_replace replaceby stmt)
        | IfStmt (e, stmt1, stmt2_opt) ->
            IfStmt (
                replace_expr_expr x_to_replace replaceby e,
                replace_expr_stmt x_to_replace replaceby stmt1,
                Option.map (replace_expr_stmt x_to_replace replaceby) stmt2_opt
            )
        | MatchStmt (e, branches) ->
            MatchStmt (
                replace_expr_expr x_to_replace replaceby e,
                List.map (function (e, stmt) ->
                    replace_expr_expr x_to_replace replaceby e,
                    replace_expr_stmt x_to_replace replaceby stmt
                ) branches
            )
        | ReturnStmt e -> ReturnStmt (replace_expr_expr x_to_replace replaceby e) 
        | ExpressionStmt e -> ExpressionStmt (replace_expr_expr x_to_replace replaceby e) 
        | BlockStmt stmts -> BlockStmt (List.map (replace_expr_stmt x_to_replace replaceby) stmts) 
        | GhostStmt stmt -> GhostStmt (replace_expr_stmt x_to_replace replaceby stmt)
    and replace_expr_stmt x_to_replace replaceby = map_place (_replace_expr_stmt x_to_replace replaceby)




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
                _replace_expr_expr  x2 (Some x1, None) g2.place e2 ,mt    
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
