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
    | TActivationInfo
    | TBool
    | TInt
    | TFloat 
    | TStr
    | TLabel
    | TVoid
    | TPlace
    | TVPlace

and _composed_type =
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

(************************************ Component *****************************)
and state_kind = Global | Local | Shared

and _state = 
    | StateDcl of  {ghost: bool; kind: state_kind; type0: main_type; name: variable; init_opt: expr option}

    (*use global x as y;*)
    | StateAlias of  {ghost: bool; kind: state_kind; type0: main_type; name: variable}
and state = _state placed

and _param = main_type * variable
and param = _param placed

and _contract = { (* TODO GADT *)
    method_name: variable;
    pre_binders: (main_type * variable * expr) list; 
    ensures: expr option;
    returns: expr option;
}
and contract = _contract placed

and _method0 = 
    | CustomMethod of {
        ghost: bool; 
        ret_type: main_type; 
        name: variable; 
        args: param list; 
        abstract_impl: stmt option; 
        contract_opt: contract option
    }
    (* Activation liftime management*)
    | OnStartup of method0
    | OnDestroy of method0
and method0 = _method0 placed

and _port = {
    name: variable;
    input: expr;
    expecting_st: main_type;
    callback: expr
}
and port = _port placed

and _component_item =  
    | State of state 
    | Method of method0 
    | Contract of contract 

    (** Inter-component composition*)
    | Port of port 

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

and _component_dcl = 
    (* 
    component X args = {
        component_stmt1; 
        ...
        component_stmtn;
    } =>
    TODO component X = lambda arg1: ... lambda argn: { body } ???
    *)
    | ComponentStructure of {name: variable; args: param list; body: component_item list} 
    | ComponentAssign of {name: variable; args: param list; value: component_expr}

and component_dcl = _component_dcl placed

(********************** Manipulating component structure *********************)
and _component_expr = 
    | VarCExpr of variable  
    (* functor or X(1) *)
    | AppCExpr of component_expr * component_expr 
    | UnboxCExpr of expr
    | AnyExpr of expr
and component_expr = _component_expr placed

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
and _term =
    | Comments of comments
    
    (* Dynamic part*)
    | Stmt of stmt

    (** Structure part*)
    | Component of component_dcl

    (* Static part*)
    (*TODO | SignatureDcl of signature_dcl*)   
    | Typedef of variable * main_type option
and term = _term placed

and program = term list

(* The following annotation requests the automatic generation of a [show_]
   function for each of the types defined above.*)
[@@deriving show { with_path = false }]

(** TODO * 

and grpplace =      
  | GrpNone
  | GrpVar of variable    
  *)