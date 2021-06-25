open AstUtils

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
end

include IR_common

module Make (Params : IRParams) = struct
  include IR_common
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

  (************************************ Component *****************************)

  type  _state = 
    | StateDcl of  {
        ghost: bool; 
        kind: state_kind; 
        type0: main_type; 
        name: variable; 
        body: _state_dcl_body
    }

    (* use global x as y; *)
    | StateAlias of  {
        ghost: bool; 
        kind: state_kind; 
        type0: main_type; 
        name: variable
    }
  and state = _state placed

  and _method0 = 
      | CustomMethod of {
          ghost: bool; 
          ret_type: main_type; 
          name: variable; 
          args: param list; 
          body: _custom_method0_body; 
          contract_opt: contract option
      }
      (* Activation liftime management*)
      | OnStartup of method0
      | OnDestroy of method0
  and method0 = _method0 placed


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
      | ComponentStructure of {target_name: target_name; name: variable; args: param list; body: component_item list} 
      | ComponentAssign of {name: variable; args: param list; value: component_expr}

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
    and _typedef = (* Two kind for type *) 
    | ClassicalDef of variable * main_type list * _typedef_body
    | EventDef of variable * main_type list * _typedef_body
    and typedef = _typedef placed

    and _term =
        | EmptyTerm
        | Comments of comments
        
        (* Dynamic part*)
        | Stmt of stmt

        (** Structure part*)
        | Component of component_dcl

        (* Static part*)
        (*TODO | SignatureDcl of signature_dcl*)   
        | Typealias of variable * _typealias_body
        | Typedef of typedef 
    and term = _term placed

  and program = term list

  (* The following annotation requests the automatic generation of a [show_]
    function for each of the types defined above.*)
  [@@deriving show { with_path = false }]
end  