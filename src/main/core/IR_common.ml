open AstUtils
open Easy_logging
open Ppx_hash_lib.Std
open Hash.Builtin
open Ppx_compare_lib.Builtin

(************************************* Base types ****************************)
open Label

type test = int Atom.VMap.t 
[@@deriving show { with_path = false }]


let logger = Utils.make_log_of "IR_common" 


(* TODO clean AST *)

type expr_variable = Atom.t
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
    | TActivationRef of main_type
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
    | TInductive of main_type list
    | TVPlace of main_type

    | TObject of component_variable 

    | TUnion of main_type * main_type


    (** Message-passing *)
    | TBridge of tbridge
    | TInport of main_type (* session_type *)
    | TEport of main_type (* evenement from the runtime not a message from an other component *)
    | TOutport of main_type (* type of the generated session *)

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
    | STWildcard
    | STBottom

    | STDual of session_type 
and session_type = _session_type placed

(* NB component or class type *)
and _struct_type =
    | CompTUid of component_variable 
    | TStruct of component_variable * main_type Atom.VMap.t (** types of states, methods, ports, ... and subcomponents *)
    (* Polymorphsim*)
    | TPolyCVar of component_variable
    | CompTBottom
and struct_type = _struct_type placed

and _main_type = 
    | EmptyMainType
    | CType of composed_type 
    | SType of session_type
    (* First value component type*)
    | CompType of struct_type
    | ClType of struct_type
    (* Dynamic (or not) contraints*)
    | ConstrainedType of main_type * applied_constraint 
    (*gadt contraints: type -> bool *)
    | TRaw of string
and main_type = _main_type placed

(******************************** Constraints ********************************)
and _constraint_header =      
    | UseMetadata of main_type * expr_variable
    | SetTimer of expr_variable
    | SetFireTimer of expr_variable * int (** specify timeout delay *)
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
    features:     (string, string) Hashtbl.t;[@opaque][@hash.ignore][@compare.ignore][@equal.ignore]
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
    | BLabelLit of expr_variable
    | StringLit of string

    (** Activations *)
    | ActivationRef of unit (* TODO *)

    (** Placement *)
    | Place of place
    | VPlace of vplace 

    (** Message-passing *)
    | StaticBridge of { (* Create during partial eval *)
        id: component_variable;
        protocol_name: component_variable;
    } (* Resolve statically a "bridge ()" *)
and literal = _literal placed

(************************************* Operations ******************************)
and unop = AstUtils.unop
and binop = AstUtils.binop
and block = AstUtils.block
and block2 = AstUtils.block2

(************************************ Expr & Stmt *****************************)

and spawn = {c: component_expr; args: expr list; at: expr option; inline_in: expr option}
and create = {c: component_variable; args: expr list}
and _expr = 
    | EmptyExpr
    | VarExpr of expr_variable 
    | ImplicitVarExpr of expr_variable

    | InterceptedActivationRef of expr * expr option (* Interceptor activation * intercepted activation 
    If option for second arg => anonymous mod
    *)

    | ActivationAccessExpr of component_variable * expr * expr_variable (* cname, e, x*)
    | AccessExpr of expr * expr (*e1.e2*)
    | BinopExpr of expr * binop * expr 
    | LambdaExpr of param list * expr 
    | LitExpr of literal
    | UnopExpr of unop * expr 

    (** *)
    (* TODO closure/lamba*)
    | CallExpr of expr * expr list(** method and fct can be recursive *)
    | NewExpr of expr * expr list                   
    | PolyApp of expr * main_type list

    (** Control flow *)
    | BridgeCall of { (* denotes a "bridge()" *)
        protocol_name: component_variable;
    }

    (* condensed form of the if-else statement that also returns a value i.e. an expr *)
    | TernaryExpr of expr * expr * expr

    (* Reflexifity *)
    | This (* current activation *)
    | Self (* current obj *)

    (* Activation lifetime expr *)
    | Create of create
    | Spawn of spawn  

    (* Structure expr *)
    | BoxCExpr of component_expr

    (** *)
    | OptionExpr of expr option
    | ResultExpr of (expr option * expr option) (* Ok, Err *)
    | BlockExpr of block * expr list
    | Block2Expr of block2 * (expr * expr) list
    | RawExpr of string (* not interpreted *)
and expr = (_expr * main_type) placed

and branch_stmt = {branch_label: literal; branch_s: expr_variable; body: stmt}
and _stmt = 
    | EmptyStmt

    (** Binders *)
    | AssignExpr of expr_variable * expr
    | AssignThisExpr of component_variable * expr
    | AssignSelfExpr of component_variable * expr
    | LetStmt of main_type * expr_variable * expr

    (** Comments *)
    | CommentsStmt of comments

    (** Control flow *)
    | BreakStmt
    | ContinueStmt
    | ExitStmt of int
    | ForeachStmt of main_type * expr_variable * expr * stmt
    | IfStmt of expr * stmt * stmt option
    | MatchStmt of expr * (expr * stmt) list
    | ReturnStmt of expr

    (**type name, type definition*)
    | ExpressionStmt of expr
    | BlockStmt of stmt list

    | GhostStmt of stmt

    | WithContextStmt of bool * component_variable * expr * stmt list
    | BranchStmt of {
        s: expr;
        label_opt: expr option;
        branches: branch_stmt list 
    }
and stmt = _stmt placed

and _param = main_type * expr_variable
and param = _param placed

(******************************** Component **********************************)
and _port = {
    name: component_variable;
    expecting_st: main_type;
    callback: expr;
    _children: component_variable list; (* ports that are binded to the same channel as current port*)
    _disable_session: bool; (* for internal use only, when we need to add runtime port like without dynamic session mechanism *)
    _is_intermediate: bool;
    _receive_id: Atom.t option;
}
and port = (_port * main_type) placed

and _eport = {
    name: component_variable;
    expecting_mt: main_type;
    callback: expr;
}
and eport = (_eport * main_type) placed

and _outport = {
    name: component_variable;
    protocol: main_type;
    _children: component_variable list; (* ports that are binded to the same channel as current port*)
}
and outport = (_outport * main_type) placed

and session_interceptor_kind = 
    | Egress
    | Ingress
    | Both
and method_annotation = 
    | MsgInterceptor of {
        kind: session_interceptor_kind;
    }
    | SessionInterceptor of {
        anonymous: bool;
        kind: session_interceptor_kind;
    }
    | Onboard of component_variable list (* List of schemas that this function can onboard *)
    | Expose

and component_annotation = 
    | Capturable of {allowed_interceptors: component_variable list;}
    | InlinableIn of component_variable list

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
    | AppCExpr of component_expr * component_expr list 
    | UnboxCExpr of expr
    | AnyExpr of expr
and component_expr = (_component_expr * main_type) placed

(* The following annotation requests the automatic generation of a [show_]
function for each of the types defined above.*)
[@@deriving show { with_path = false }, hash, compare, equal]

