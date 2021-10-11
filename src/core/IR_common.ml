open AstUtils
open Easy_logging

(************************************* Base types ****************************)
open Label

let logger = Logging.make_logger "_1_ compspec" Debug [];;

type ident = string 

and variable = Atom.atom
and _comments =
    | BlockComment of string
    | DocComment of string
    | LineComment of string
and comments = _comments placed

(************************************ Types **********************************)
and flat_type = 
    (** Literal types *)
    | TBool
    | TInt
    | TUUID
    | TFloat 
    | TStr
    | TLabel
    | TVoid
    | TPlace
    | TVPlace
    | TWildcard

and tbridge = {
    in_type: main_type; 
    out_type: main_type;
    protocol: main_type;
}

and _composed_type =
    | TActivationInfo of main_type
    (*| TSession of session_type * variable*)
    | TArrow of main_type * main_type

    | TVar of variable
    | TFlatType of flat_type

    | TArray of main_type 
    | TDict of main_type * main_type
    | TList of main_type 
    | TOption of main_type
    | TResult of main_type * main_type
    | TSet of main_type
    | TTuple of main_type list

    | TUnion of main_type * main_type


    (** Message-passing *)
    | TBridge of tbridge

    | TRaw of Impl_common.blackbox_term (*TODO move it to IRI by doing so composed type should not be any more in common *)
and composed_type = _composed_type placed

and _session_type =  
    | STEnd 
    | STVar of variable * applied_constraint option  (** x *) 
    | STSend of main_type * session_type
    | STRecv of main_type * session_type
    | STBranch of (variable * session_type * applied_constraint option) list            
    | STSelect of (variable * session_type *  applied_constraint option) list               
    | STRec of variable * session_type (* X * type*) 
    | STInline of variable (* syntaxic suggar in order to inline an existing session type definition*)
and session_type = _session_type placed

and _component_type =
    | CompTUid of variable 
and component_type = _component_type placed

and _main_type = 
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
    | UseGlobal of main_type * variable 
    | UseMetadata of main_type * variable
    | SetTimer of variable
    | SetFireTimer of variable * int (* specify timeout delay *)
and constraint_header = _constraint_header placed

and _constraints = _expr (* for now, maybe we will need to restrict a bit for SMT solving*)
and constraints = _constraints placed 

and applied_constraint = (constraint_header list) * constraints option

(************************************ (V) - Place ****************************)
and _place = unit 
and place = _place placed
and _vplace = 
| VPlaceVar of variable 
| VPlaceDcl of { 
    name:           variable;
    nbr_instances:  expr;
    features:     (string, string) Hashtbl.t;[@opaque]
    children:       vplace list
}

and vplace = _vplace placed
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
    | ActivationInfo of unit (* TODO *)

    (** Placement *)
    | Place of place
    | VPlace of vplace 

    (** Message-passing *)
    | Bridge of {
        id: variable; 
        protocol_name: variable;
    }
and literal = _literal placed

(************************************* Operations ******************************)
and unop = 
    | Not 
    | UnpackResult

and binop =
    (* Boolean *)
    | And    
    | Or 

    (* Numeric *)
    | Plus
    | Minus
    | Mult
    | Divide

    (** Comparison *)
    | StructuralEqual (*(e.g. like equals in Java or = in Ocaml)*)
    | Equal 
    | GreaterThanEqual
    | LessThanEqual
    | GreaterThan
    | LessThan

    (* Iterators *)
    | In

    (* Others TODO*)
    (*
    | Dot
    | Sequence
    *)

(************************************ Expr & Stmt *****************************)

and block = 
    | Block
    | List 
    | Tuple
    | Set
and block2 =
    | Dict

and _expr = 
    | VarExpr of variable 

    | AccessExpr of expr * expr (*e1.e2*)
    | BinopExpr of expr * binop * expr 
    | LambdaExpr of variable * main_type * stmt 
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
and expr = _expr placed

and _stmt = 
    | EmptyStmt

    (** Binders *)
    | AssignExpr of variable * expr
    | AssignThisExpr of variable * expr
    | LetExpr of main_type * variable * expr

    (** Comments *)
    | CommentsStmt of comments

    (** Control flow *)
    | BreakStmt
    | ContinueStmt
    | ExitStmt of int
    | ForStmt of main_type * variable * expr * stmt
    | IfStmt of expr * stmt * stmt option
    | MatchStmt of expr * (expr * stmt) list
    | ReturnStmt of expr

    (**type name, type definition*)
    | ExpressionStmt of expr
    | BlockStmt of stmt list

    | GhostStmt of stmt
and stmt = _stmt placed

and _param = main_type * variable
and param = _param placed

(******************************** Component **********************************)
and state_kind = Global | Local | Shared

and _port = {
    name: variable;
    input: expr;
    expecting_st: main_type;
    callback: expr
}
and port = _port placed

(******************************** Contracts **********************************)
and _contract = { (* TODO GADT *)
    method_name: variable;
    pre_binders: (main_type * variable * expr) list; 
    ensures: expr option;
    returns: expr option;
}
and contract = _contract placed


(********************** Manipulating component structure *********************)
and _component_expr = 
    | VarCExpr of variable  
    (* functor or X(1) *)
    | AppCExpr of component_expr * component_expr 
    | UnboxCExpr of expr
    | AnyExpr of expr
and component_expr = _component_expr placed

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

let rec free_vars_expr_ (already_binded:Atom.Set.t) place : _expr -> Atom.Set.t * variable list = function 
| LambdaExpr (x, mt, stmt) ->
    already_binded, snd (free_vars_stmt (Atom.Set.add x already_binded) stmt)
| VarExpr x when Atom.Set.find_opt x already_binded <> None  -> already_binded, [] 
| VarExpr x when Atom.is_builtin x -> already_binded, [] 
| VarExpr x -> 
    logger#error "free var of %s " (Atom.to_string x);
    already_binded, [x]
| BoxCExpr _ | LitExpr _ | OptionExpr None | ResultExpr (None, None) |This -> already_binded, []
| AccessExpr (e1, e2) | BinopExpr (e1, _, e2) | ResultExpr (Some e1, Some e2) ->
    let _, fvars1 = free_vars_expr already_binded e1 in
    let _, fvars2 = free_vars_expr already_binded e2 in
    already_binded, fvars1@fvars2
| UnopExpr (_, e) | OptionExpr (Some e) | ResultExpr (Some e, None) | ResultExpr (None, Some e)->
    let _, fvars = free_vars_expr already_binded e in
    already_binded, fvars
| CallExpr ({value=VarExpr _ }, es) | NewExpr ({value=VarExpr _}, es) -> (* no first class function nor constructor inside stmt - so we get ride of all possible constructors *)
    already_binded, List.fold_left (fun acc e -> acc @ (snd (free_vars_expr already_binded e))) [] es
| CallExpr (e, es) | NewExpr (e, es) | Spawn {args=es; at = Some e} ->
    let _, fvars = free_vars_expr already_binded e in
    already_binded, List.fold_left (fun acc e -> acc @ (snd (free_vars_expr already_binded e))) fvars es
| BlockExpr (_, es) | Spawn {args=es} -> 
    already_binded, List.fold_left (fun acc e -> acc @ (snd (free_vars_expr already_binded e))) [] es
and free_vars_expr (already_binded:Atom.Set.t) expr : Atom.Set.t * variable list = free_variable_place (free_vars_expr_ already_binded) expr

and free_vars_stmt_ (already_binded:Atom.Set.t) place : _stmt -> Atom.Set.t * variable list = function 
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
    let _, fvars2 = free_vars_stmt (Atom.Set.add x already_binded) stmt in
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
    let already_binded = Atom.Set.add x already_binded in
    let _, fvars = free_vars_expr already_binded e in  
    already_binded, fvars 
| MatchStmt (e, branches) ->
    let _, fvars = free_vars_expr already_binded e in
    already_binded, List.fold_left (fun acc (_,stmt) -> acc @ (snd (free_vars_stmt already_binded stmt))) fvars branches 
| ReturnStmt e ->
    let _, fvars = free_vars_expr already_binded e in
    already_binded, fvars

and free_vars_stmt (already_binded:Atom.Set.t) stmt : Atom.Set.t * variable list = free_variable_place (free_vars_stmt_ already_binded) stmt

let rec timers_of_headers = function
    | [] -> []
    | {value=UseGlobal _}::headers | {value=UseMetadata _} ::headers-> timers_of_headers headers
    | {value=SetTimer x}::headers -> x::(timers_of_headers headers)
    | {value=SetFireTimer (x,_)}::headers -> raise (Error.DeadbranchError "SetFireTimer should exists before GuardTransform - not supported yet") 
and timers_of_st_ = function
| STEnd -> []
| STRecv ({value=ConstrainedType (_, (guard_headers, _))}, st) | STSend ({value=ConstrainedType (_, (guard_headers, _))}, st) -> 
    (timers_of_headers guard_headers) @ (timers_of_st st)
and timers_of_st st = timers_of_st_ st.value