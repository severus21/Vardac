open AstUtils

(************************************* Base types ****************************)
open Label

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
    | TFloat 
    | TStr
    | TLabel
    | TVoid
    | TPlace
    | TVPlace

and _composed_type =
    | TActivationInfo of main_type
    | TArrow of main_type * main_type

    | TVar of variable
    | TFlatType of flat_type

    | TDict of main_type * main_type
    | TList of main_type 
    | TOption of main_type
    | TResult of main_type * main_type
    | TSet of main_type
    | TTuple of main_type list


    (** Message-passing *)
    | TBridge of {
        in_type: main_type; 
        out_type: main_type;
        protocol: main_type;
    }

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
    | STTimeout of expr * session_type (* time in second * protocol*)
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
and constraint_header = _constraint_header placed

and _constraints = 
    | CExpr of expr (* for now, maybe we will need to restrict a bit for SMT solving*)
and constraints = _constraints placed 

and applied_constraint = (constraint_header list) * constraints

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
    | EmptyLit 
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
        protocol: session_type;
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
    | LambdaExpr of variable * stmt 
    | LitExpr of literal
    | UnopExpr of unop * expr 

    (** *)
    (* TODO closure/lamba*)
    | CallExpr of expr * expr list(** method and fct can be recursive *)

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
    | ForStmt of variable * expr * stmt
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

