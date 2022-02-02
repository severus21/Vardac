open Core
open Core.AstUtils
open Easy_logging

let logger = Logging.make_logger "_1_ compspec" Debug [];;
(************************************* Base types ****************************)
open Label
type variable = string
and _comments = AstUtils._comments
and comments = AstUtils.comments

(************************************ Types **********************************)
and flat_type = IR.flat_type
and _composed_type =
    | TActivationRef of main_type
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
    | TVPlace of main_type

    | TUnion of main_type * main_type

    | TBridge of {
        in_type: main_type; 
        out_type: main_type;
        protocol: main_type;
    }
and composed_type = _composed_type placed

and _session_type =  
    | STEnd 
    | STVar of variable
    | STSend of main_type * session_type
    | STRecv of main_type * session_type
    | STBranch of (variable * session_type * applied_constraint option) list            
    | STSelect of (variable * session_type *  applied_constraint option) list               
    | STRec of variable * session_type (* X * type*) 
    | STInline of variable (* syntaxic suggar in order to inline an existing session type definition*)

    | STDual of main_type (* a session_type or a TVar*)

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
    | UseMetadata of main_type * variable
    | SetTimer of variable
and constraint_header = _constraint_header placed

and _constraints = _expr (* for now, maybe we will need to restrict a bit for SMT solving*)
and constraints = _constraints placed 

and applied_constraint = (constraint_header list) * constraints option
(************************************ (V) - Place ****************************)
and vplace = { 
    name:           variable;
    nbr_instances:  expr;
    features:     (string, string) Hashtbl.t;[@opaque]
    children:      vplace list
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
    | ActivationRef of unit (* TODO *)

    (** No Placement here, since place are defined in a n external file*)
and literal = _literal placed
(************************************* Operations ******************************)
and unop = IR.unop
and binop = IR.binop 

(************************************ Expr & Stmt *****************************)
and block =  IR.block
and block2 =  IR.block2

and _expr = 
    | VarExpr of variable 
    | ImplicitVarExpr of variable

    | AccessExpr of expr * expr (*e1.e2*)
    | BinopExpr of expr * binop * expr 
    | LambdaExpr of variable * main_type * expr 
    | LitExpr of literal
    | UnopExpr of unop * expr 

    (** *)
    (* TODO closure/lamba*)
    | CallExpr of expr * expr list(** method and fct can be recursive *)
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

and expr = _expr placed

and _stmt = 
    | EmptyStmt

    (** Binders *)
    | AssignExpr of variable * expr
    | AssignThisExpr of variable * expr
    | LetStmt of main_type * variable * expr

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

    | WithContextStmt of bool * variable * expr * stmt list

    | GhostStmt of stmt
and stmt = _stmt placed


and _function_dcl = {
    name: variable;
    targs: variable list; (* generic type parameters *)
    ret_type: main_type;
    args: param list;
    abstract_impl: stmt list;
}
and function_dcl = _function_dcl placed


(************************************ Component *****************************)
and _state = 
    | StateDcl of  {
        ghost: bool; 
        type0: main_type; 
        name: variable; 
        init_opt: expr option}

    (*use global x as y;*)
    | StateAlias of  {
        ghost: bool; 
        type0: main_type; 
        name: variable
    }
and state = _state placed

and _param = main_type * variable
and param = _param placed

and _contract = { (* TODO GADT *)
    method_name: variable;
    pre_binders: (main_type * variable * expr) list; (* list of binder, should be let*)
    ensures: expr option;
    returns: expr option;
    invariant: expr option;
}
and contract = _contract placed

and _method0 = {
        annotations: annotation list;
        ghost: bool; 
        ret_type: main_type; 
        name: variable; 
        args: param list; 
        abstract_impl: stmt list; 
        on_destroy: bool;
        on_startup: bool
    }
and method0 = _method0 placed

and _port = {
    name: variable;
    input: expr;
    input_type: main_type;
    expecting_st: main_type;
    callback: expr
}
and port = _port placed

and _outport = {
    name: variable;
    input: expr;
    input_type: main_type;
}
and outport = _outport placed

and _component_item =  
    | State of state 
    | Method of method0 
    | Contract of contract 

    (** Inter-component composition*)
    |Inport of port 
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

and _component_dcl = 
    | ComponentStructure of {
        name: variable; 
        annotations: annotation list;
        args: param list; 
        body: component_item list
    } 
    | ComponentAssign of {name: variable; args: param list; value: component_expr}

and component_dcl = _component_dcl placed

(********************** Manipulating component structure *********************)
and _component_expr = 
    | VarCExpr of variable  
    (* functor or X(1) *)
    | AppCExpr of component_expr * component_expr list 
    | UnboxCExpr of expr
    | AnyExpr of expr
and component_expr = _component_expr placed

(********************** Signatures *********************)

(************************************ Program *****************************)

and _annotation = 
    | Capturable of {allowed_interceptors: variable list;}
    | MsgInterceptor of {
        kind: IR.session_interceptor_kind;
    }
    | SessionInterceptor of {
        anonymous: bool;
        kind: IR.session_interceptor_kind;
    }
    | Onboard of variable list
and annotation = _annotation placed

(** Preprocessor terms *)
and _pp_term = 
    | UsePP of string list 
and pp_term = _pp_term placed

and _typedef = 
| ClassicalDef of variable * main_type list 
| EventDef of variable * main_type list
| ProtocolDef of variable * main_type
| VPlaceDef of variable * variable
and typedef = _typedef placed

and _term =
    | Comments of comments

    | PPTerm of pp_term
    
    (* Dynamic part*)
    | Stmt of stmt
    | Annotation of annotation

    (** Structure part*)
    | Component of component_dcl
    | Function of function_dcl

    (* Static part*)
    (*TODO | SignatureDcl of signature_dcl*)   
    | Typealias of variable * main_type option (* name, optional value*) 
    | Typedef of typedef 
    | Derive of { (* Used to rewrite the ast *)
        name: variable;
        cargs: component_expr list;
        targs: main_type list; 
        eargs: expr list;    
    }
and term = _term placed

and program = term list
[@@deriving show { with_path = false }]

(* let make_vplace name nbr_instances features children = ()*)
let make_vplace _ _ _ _ = ()

let rec timers_of_headers = function
    | [] -> []
    | {value=UseMetadata _} :: headers -> timers_of_headers headers
    | {value=SetTimer x}::headers -> x::(timers_of_headers headers)
and timers_of_st_ = function
| STRecv ({value=ConstrainedType (_, (guard_headers, _))}, st) | STSend ({value=ConstrainedType (_, (guard_headers, _))}, st) -> 
    (timers_of_headers guard_headers) @ (timers_of_st st)
(* Just propagate *)
| STEnd -> []
| STInline _ | STDual {value=CType{value=TVar _}} -> [] 
| STRecv (_, st) | STSend (_, st) -> timers_of_st st
| STDual {value=SType st} -> timers_of_st st

and timers_of_st st = timers_of_st_ st.value