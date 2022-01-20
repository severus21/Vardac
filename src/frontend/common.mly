%%

(************************************* Comments ****************************)

any_comments_:
| c=COMMENTS 
    { c }
%public %inline any_comments:
  t = placed(any_comments_)
    {t}

(************************************* Base types ****************************)
any_composed_type_:
| t1 = any_type SIMPLE_RARROW t2 = any_type
    { TArrow (t1, t2) }
| t1 = any_type MID t2 = any_type
    { TUnion (t1, t2) }
| x = LID | x = UID 
    { TVar x }
| flat_type = PTYPE
    { TFlatType flat_type}
| ct = any_composed_type LANGLEBRACKET args=right_flexible_list(COMMA, any_type) RANGLEBRACKET 
    {
        (* TODO Dict and co -> to lower *)
        match ct.value with
        (* Primitive types *)
        | TVar "array" -> begin
            match args with
            | [x] -> TArray x
            | _ -> Core.Error.error ct.place "Array type excepts exactly one type parameter, gets %d !" (List.length args)
        end
        | TVar "dict" -> begin
            match args with
            | x::y::[] -> TDict (x,y)
            | _ -> Core.Error.error ct.place "Dict type excepts exactly two type parameters (key * value), gets %d !" (List.length args)
        end
        | TVar "list" -> begin
            match args with
            | [x] -> TList x
            | _ -> Core.Error.error ct.place "List type excepts exactly one type parameter, gets %d !" (List.length args)
        end
        | TVar "vplace" -> begin
            match args with
            | [x] -> TVPlace x
            | _ -> Core.Error.error ct.place "Vplace type excepts exactly one type parameter, gets %d !" (List.length args)
        end
        | TVar "option" -> begin
            match args with
            | [x] -> TOption x
            | _ -> Core.Error.error ct.place "Option type excepts exactly one type parameter, gets %d !" (List.length args)
        end
        | TVar "vplace" -> begin
            match args with
            | [x] -> TVPlace x
            | _ -> Core.Error.error ct.place "vplace type excepts exactly one type parameter, gets %d !" (List.length args)
        end
        | TVar "result" -> begin
            match args with
            | x::y::[] -> TResult (x,y)
            | _ -> Core.Error.error ct.place "Result type excepts exactly two type parameters, gets %d !" (List.length args)
        end
        | TVar "set" -> begin
            match args with
            | [x] -> TSet x
            | _ -> Core.Error.error ct.place "Result type excepts exactly one type parameter, gets %d !" (List.length args)
        end
        | TVar "bridge" -> begin
            match args with
            | [in_type; out_type; protocol] -> TBridge {in_type; out_type; protocol}
            | _ -> Core.Error.error ct.place "Bridge type excepts exactly tree type parameters, gets %d !" (List.length args)
        end
        | TVar "tuple" -> TTuple args
        | TVar "activation_ref" -> begin 
            match args with
            | [arg] -> TActivationRef arg 
            | _ -> Core.Error.error ct.place "activation_ref except exactly one type parameter (a Component type)"
        end
        (* User defined parametrized types *)
        | _ -> Core.Error.error ct.place "Unknow parametric constructor"
    }
(* TODO other composed types *)
%public %inline any_composed_type:
    t = placed(any_composed_type_)
    {t}


any_st_match:
| x=STRLITERAL COLON st=any_session_type
    { (x,st, None) }
(* Constraints per branch *)
| x=STRLITERAL c= any_applied_constraint COLON st=any_session_type
    { (x,st, Some c) }


any_session_type_:
| SESSION LANGLEBRACKET x=LID RANGLEBRACKET
    { STInline x }
| SESSION LANGLEBRACKET st=any_session_type_ RANGLEBRACKET
    { st }
|DOT
    {STEnd}
|op=BINOP x = LID |op=BINOP x = UID
    {
        match op with
        | Minus -> STVar x
        | _ -> raise (Error.SyntaxError [$loc])
    }
|op=BINOP x = LID | op=BINOP x = UID 
    {
        match op with
        | Minus -> STVar x
        | _ -> raise (Error.SyntaxError [$loc])
    }
|BANG t=any_type st=any_session_type 
    {STSend (t, st)}
|RECV t=any_type st=any_session_type   
    {STRecv (t, st)}
|BRANCH LCURLYBRACKET l=right_flexible_list(SEMICOLON, any_st_match) RCURLYBRACKET
    {STBranch l}
|SELECT LCURLYBRACKET l=right_flexible_list(SEMICOLON, any_st_match) RCURLYBRACKET
    {STSelect l}
|RECST x = LID DOT t=any_session_type |RECST x = UID DOT t=any_session_type
    {STRec (x,t)}
|LPAREN st=any_session_type_ RPAREN
    {st}
|INLINE x=LID | INLINE x=UID 
    { STInline x}
|DUAL t = any_type
{
    match t with
    | {value=SType _} | {value=CType {value=TVar _}} -> STDual t 
    | _ -> Core.Error.error [$loc] "dual except exactly one type parameter (a session type)"
}

%public %inline any_session_type:
  t = placed(any_session_type_)
    {t}

any_component_type_:
| COMPONENT x=UID
    {CompTUid x}
%public %inline any_component_type:
  t = placed(any_component_type_)
    {t}

any_type_:
| st = any_session_type
    { SType st }
| ct = any_composed_type
    { CType ct }
| cpt = any_component_type
    { CompType cpt }
| t = any_type c= any_applied_constraint
    { ConstrainedType (t, (fst c, snd c))}
| LPAREN t = any_type_ RPAREN
    { t }
%public %inline any_type:
  t = placed(any_type_)
    {t}

%public any_type_dcl_:
| TYPE x=LID SEMICOLON
    { Typealias (x, None) }
| TYPE x=LID EQ t=any_type SEMICOLON
    { Typealias (x, Some t) }
| TYPE x=UID SEMICOLON
    { Typealias (x, None) }
| TYPE x=UID EQ t=any_type SEMICOLON
    { Typealias (x, Some t) }
| TYPE x=LID OF args = right_flexible_list(COMMA, any_type) SEMICOLON
    { Typedef { place=[$loc]; value = ClassicalDef(x, args)} }
| EVENT x=LID OF args = right_flexible_list(COMMA, any_type) SEMICOLON
    { Typedef { place=[$loc]; value=EventDef(x, args)} }
| PROTOCOL x=LID EQ mt = any_type SEMICOLON
    { Typedef { place=[$loc]; value=ProtocolDef(x, mt)} }
| VPLACEDEF x=LID OF name = STRLITERAL SEMICOLON
    { Typedef { place=[$loc]; value=VPlaceDef(x, name)} }

(******************************** Constraints ********************************)
any_constraint_header_:
| METADATA mt=any_type x = LID
    { UseMetadata (mt,x) }
| TIMER x=LID
    { SetTimer x }
%public %inline any_constraint_header:
  t = placed(any_constraint_header_)
    {t}

any_constraint_:
| e = any_expr_
    { e }
%public %inline any_constraint:
  t = placed(any_constraint_)
    {t}

any_applied_constraint:    
|LCURLYBRACKET headers=right_flexible_list(COMMA, any_constraint_header) MID e_opt=option(any_constraint) RCURLYBRACKET
    { (headers, e_opt) }
(************************************* Literals ******************************)
atomic_literal_:
| b = BOOLLITERAL
    { BoolLit b}
| f = FLOATLITERAL
    { FloatLit f }
| n = INTLITERAL
    { IntLit n }
| str = STRLITERAL
    { StringLit str }

any_literal_:
| a = atomic_literal_
    { a }
| l = LABEL_LITERAL 
    { LabelLit l}
(* TODO
- activation info
- place
- vplace
*)

%public %inline any_literal:
    t = placed(any_literal_)
    { t }

(************************************* Operations ******************************)
atomic_bin_op:
| op=BINOP
    { op }
| LANGLEBRACKET
    { Core.AstUtils.LessThan }
| RANGLEBRACKET
    { Core.AstUtils.GreaterThan }
any_op_:
| op=UNOP e=any_expr
    { UnopExpr (op, e)}
| e=any_expr RECV
    { UnopExpr (Core.AstUtils.UnpackResult, e)}
|  x = any_expr  op=atomic_bin_op y = any_expr   
    { BinopExpr  (x, op, y) }

(************************************ Expr & Stmt *****************************)
any_dict_item:
| k=any_expr COLON v=any_expr
    { (k,v) }
any_expr_bloc:
(* {} could be Set or Dict, the matching order implies it is denoted as a Set *)
| LBRACKET exprs=right_flexible_list(COMMA, any_expr) RBRACKET
    {BlockExpr (List, exprs)}
| LCURLYBRACKET exprs=right_list(COMMA, any_dict_item) RCURLYBRACKET
    {Block2Expr (Dict, exprs)}
| LCURLYBRACKET exprs=right_flexible_list(COMMA, any_expr) RCURLYBRACKET
    {BlockExpr (Set, exprs)}
(* (e1,) ou (e1,e2,..,n) *)
| LPAREN e1=any_expr COMMA exprs=right_flexible_list(COMMA, any_expr) RPAREN
    { BlockExpr (Tuple, e1::exprs)}


match_entry:
| CASE e1 = any_expr DOUBLE_RARROW stmt = any_stmt
    { (e1, stmt) }

atomic_expr_:
| x = LID
    { VarExpr x }

any_expr_:
| t = atomic_expr_
    { t }
| LPAREN RPAREN
    { LitExpr {
        place = [$loc]; 
        value = VoidLit 
    }}
| LPAREN e=any_expr_ RPAREN
    { e }
| IMPLICIT DOUBLE_COLON x = LID
    { ImplicitVarExpr x}
| e1 = any_expr DOT e2=any_expr
    {AccessExpr (e1,e2)} (* FIXME can we do e2=LID *)
(* Hack because the lexer ouput a.b.c as an ATTR *)
| lids = ATTR
    {   let rec aux lids = { place = [$loc]; value = 
        match lids with
            | []  -> raise (Core.Error.PlacedDeadbranchError ([$loc], "parsing AccesExpr, should be a non empty list of lids"))
            | x::[y] -> AccessExpr ({ place = [$loc]; value = (if x = "this" then This else VarExpr x) }, { place = [$loc]; value = VarExpr y })
            | x::xs -> AccessExpr ( { place = [$loc]; value = (if x = "this" then This else  VarExpr x) }, aux xs)
        }
        in
        (aux lids).value
    }
| x = LID COLON mt = any_type SIMPLE_RARROW e = any_expr
    { LambdaExpr (x, mt, e) }
| x = LID COLON mt = any_type SIMPLE_RARROW LCURLYBRACKET e = any_expr RCURLYBRACKET
    { LambdaExpr (x, mt, e) }
| l = any_literal
    {LitExpr l}
| e1 = any_expr LPAREN args = right_flexible_list(COMMA,any_expr) RPAREN
    { CallExpr (e1, args) }
| t = any_op_
    { t }
| e = any_expr LANGLEBRACKET LANGLEBRACKET mts=right_flexible_list(COMMA,any_type) RANGLEBRACKET RANGLEBRACKET (* For some reason the parser can not distinguish between e<e> and e<mt> - NB. however e<e> should not be accepted*)
    { PolyApp (e,mts) }

(* Control-flow *)
(* TODO Ternary*)

(* Reflexifity *)
| THIS
    { This }

(* Activation lifetime *)
| SPAWN c = any_component_expr LPAREN args = right_flexible_list(COMMA, any_expr) RPAREN
    { Spawn {c=c; args=args; at=None} }
| SPAWN c = any_component_expr LPAREN args = right_flexible_list(COMMA, any_expr) RPAREN AT at=any_expr
    { Spawn {c=c; args=args; at=Some at} }

(* *)
| NONE
    { OptionExpr None }
| SOME LPAREN e=any_expr RPAREN
    { OptionExpr (Some e) }
| ERR LPAREN e=any_expr RPAREN
    { ResultExpr (None, Some e) }
| OK LPAREN e=any_expr RPAREN
    {ResultExpr (Some e, None) }



(* TODO 
BoxCExpr
*)
| b = any_expr_bloc
    { b }

%public %inline any_expr:
  t = placed(any_expr_)
    { t }

(************************************ Expr & Stmt *****************************)
any_stmt_:
(* Binders *)
| THIS DOT v=LID EQ e=any_expr SEMICOLON
    { AssignThisExpr (v, e) }
(* Hack because the lexer ouput a.b.c as an ATTR *)
| lids = ATTR EQ e=any_expr SEMICOLON
    {   match lids with 
        | []  -> raise (Core.Error.PlacedDeadbranchError ([$loc], "parsing AssignThisExpr, should be a non empty list of lids"))
        | "this"::[v] -> AssignThisExpr (v, e)
        | _ -> Core.Error.error [$loc] "Attribut assignement is only allowed for [this]: ``this.toto=1;``"
    }
| v=LID EQ e=any_expr SEMICOLON
    { AssignExpr (v, e) }
| mt = any_type v=LID EQ e=any_expr SEMICOLON
    { LetExpr (mt, v, e) }
| c=any_comments
    { CommentsStmt c}
(* Control flow *)
| BREAK SEMICOLON 
    { BreakStmt }
| CONTINUE SEMICOLON
    { ContinueStmt }
| EXIT i=INTLITERAL SEMICOLON
    { ExitStmt i}
| IF LPAREN e1=any_expr RPAREN LCURLYBRACKET e2=any_stmt RCURLYBRACKET ELSE  LCURLYBRACKET e3=any_stmt RCURLYBRACKET 
    { IfStmt (e1, e2, Some e3) }
| IF LPAREN e1=any_expr RPAREN LCURLYBRACKET e2=any_stmt RCURLYBRACKET
    { IfStmt (e1, e2, None) }
| FOR LPAREN mt = any_type x=LID IN e=any_expr RPAREN LCURLYBRACKET stmts = flexible_sequence(any_stmt) RCURLYBRACKET
    { 
        match stmts with
        | [] -> ForStmt (mt, x, e, {place=[$loc]; value=EmptyStmt})
        | [stmt] -> ForStmt (mt, x, e, stmt)
        | stmts -> ForStmt (mt, x, e, {place=[$loc]; value=BlockStmt stmts})
    }
| MATCH e1=any_expr LCURLYBRACKET exprs=flexible_sequence(match_entry) RCURLYBRACKET
    { MatchStmt (e1, exprs) }
(* TODO for*)
| RETURN e=any_expr SEMICOLON
    { ReturnStmt e }

| e = any_expr SEMICOLON
    { ExpressionStmt e}
| LCURLYBRACKET stmts = flexible_sequence(any_stmt) RCURLYBRACKET
    { BlockStmt stmts}
| GHOST BANG LCURLYBRACKET s = any_stmt RCURLYBRACKET 
    { GhostStmt s}
| WITH LANGLEBRACKET x=UID RANGLEBRACKET e = any_expr LCURLYBRACKET stmts = flexible_sequence(any_stmt) RCURLYBRACKET
    { WithContextStmt (false, x, e, stmts) }
| WITH LANGLEBRACKET x=UID COMMA y=LID RANGLEBRACKET e = any_expr LCURLYBRACKET stmts = flexible_sequence(any_stmt) RCURLYBRACKET
    { 
        match y with
        | "anonymous" -> WithContextStmt (true, x, e, stmts) 
        | _ -> Core.Error.error [$loc] "Wrong parameter for with stmt: %s" y
    }


%public %inline any_stmt:
  t = placed(any_stmt_)
    { t }

any_classical_function_:
| ret_type=any_type name=LID LPAREN args=right_flexible_list(COMMA, any_param) RPAREN SEMICOLON
    { {
        targs = [];
        ret_type=ret_type;
        name=name;
        args=args;
        abstract_impl = []
    } }
| ret_type=any_type name=LID LPAREN args=right_flexible_list(COMMA, any_param) RPAREN LCURLYBRACKET stmts=flexible_sequence(any_stmt) RCURLYBRACKET 
    { {
        targs = [];
        ret_type=ret_type;
        name=name;
        args=args;
        abstract_impl= stmts
    } }
any_id_:
| x = LID 
    { x }
| x = UID 
    { x }
any_function_:
(* Abstract method *)
| LANGLEBRACKET targs=right_flexible_list(COMMA, any_id_) RANGLEBRACKET fct = any_classical_function_ 
{
    {fct with targs} 
}
| fct = any_classical_function_
    { fct }

%public %inline any_function:
  t = placed(any_function_)
    { t }
(************************************ Component *****************************)
any_state_:
| mt=any_type name=LID
    { StateDcl { ghost=false; type0=mt; name=name; init_opt=None}}
| mt=any_type name=LID EQ e = any_expr
    { StateDcl { ghost=false; type0=mt; name=name; init_opt=Some e}}
(*| USE kind name
    { StateAlias {ghost=false; kind=kind; type0= TODO; name=name}}*)
| GHOST s=any_state_
    { match s with
        | StateDcl s -> StateDcl {s with ghost=true}
        | StateAlias s -> StateAlias {s with ghost=true}
    }
%inline any_state:
  t = placed(any_state_)
    { t }

any_contract_binder:
| mt = any_type x=LID EQ e=any_expr
    { (mt,x,e) }

any_contract_binders:
| binders=right_flexible_list(AND, any_contract_binder)
    { binders }

(* TODO factorize this code *)

any_contract_field:
| ENSURES e = any_expr
    { {method_name=""; pre_binders=[]; ensures=Some e; invariant=None; returns=None} }
| INVARIANT e = any_expr
    { {method_name=""; pre_binders=[]; ensures=None; invariant=Some e; returns=None} }
| RETURNS e = any_expr
    { {method_name=""; pre_binders=[]; ensures=None; invariant=None; returns=Some e} }

any_contract_:
| CONTRACT x = LID cfs=flexible_sequence(any_contract_field)
    { make_contract x [] cfs }
| CONTRACT x = LID WITH binders=any_contract_binders cfs=flexible_sequence(any_contract_field)
    { make_contract x binders cfs }

%inline any_contract:
  t = placed(any_contract_)
    { t }

any_param_:
| t=any_type x=LID
    { (t,x) }
%public %inline any_param:
    t = placed(any_param_)
    {t}

core_method:
(* Abstract method *)
| ret_type=any_type name=LID LPAREN args=right_flexible_list(COMMA, any_param) RPAREN SEMICOLON
    { {
        annotations = [];
        ghost=false;
        ret_type=ret_type;
        name=name;
        args=args;
        abstract_impl= [];
        on_destroy = false;
        on_startup = false;
    } }
| ret_type=any_type name=LID LPAREN args=right_flexible_list(COMMA, any_param) RPAREN LCURLYBRACKET stmts=flexible_sequence(any_stmt) RCURLYBRACKET 
    { {
        annotations = [];
        ghost=false;
        ret_type=ret_type;
        name=name;
        args=args;
        abstract_impl= stmts;
        on_destroy = false;
        on_startup = false;
    } }



lifetime_method_:
| ONSTARTUP  m=core_method
    { {m with on_startup = true} }
| ONDESTROY  m=core_method
    { {m with on_destroy = true} }

any_method_:
| m = core_method
    { m }
| m =  lifetime_method_
    { m }
| GHOST m=core_method 
    { {m with ghost=true} }    
| GHOST m=lifetime_method_ 
    { {m with ghost=true} }
%inline any_method:
  t = placed(any_method_)
    { t }

any_port_:
| PORT name=LID ON chan=any_expr DOUBLE_COLON chan_type=any_type EXPECTING t=any_type EQ callback=any_expr
    { {name=name; input=chan; input_type=chan_type; expecting_st=t; callback=callback} }
%inline any_port:
  t = placed(any_port_)
    { t }

any_outport_:
| OUTPORT name=LID ON chan=any_expr DOUBLE_COLON chan_type=any_type
    { {name=name; input=chan; input_type=chan_type} }
%inline any_outport:
  t = placed(any_outport_)
    { t }

any_component_item_:
| s = any_state SEMICOLON 
    { State s }
| m = any_method
    { Method m }
| c = any_contract
    { Contract c }
| p = any_port SEMICOLON
    { Port p}
| p = any_outport SEMICOLON
    { Outport p}
| t = any_term
    { 
        match t.value with
        | Stmt {place; value=LetExpr (mt, x, e)} -> State {place; value = StateDcl {
            ghost = false;
            type0 = mt;
            name = x;
            init_opt = Some e
        }}
        | Stmt _ -> Error.error [$loc] "Stmt can not be a component item"
        | Function _ -> failwith "Function can not be a component item"
        | _ -> Term t 
    }
| INCLUDE x = any_component_expr
    { Include x }
%inline any_component_item:
  t = placed(any_component_item_)
    { t }

any_component_dcl_:
(* component X {
    component_stmt1; 
    ...
    component_stmtn;
}*)
| COMPONENT name=UID LPAREN params=right_flexible_list(COMMA, any_param) RPAREN LCURLYBRACKET body=flexible_sequence(any_component_item) RCURLYBRACKET  
    { ComponentStructure {name=name; annotations=[]; args=params; body=body} }
(* component X = Y*)
| COMPONENT name=UID EQ value=any_component_expr SEMICOLON
    { ComponentAssign {name=name; args=[]; value=value} }
(* TODO args+functor *)
%inline any_component_dcl:
  t = placed(any_component_dcl_)
    { t }

(********************** Manipulating component structure *********************)
any_component_expr_:
| x = UID
    { VarCExpr x }
(* FIXME Conflict | e = any_expr
    { AnyExpr e} (*needed to pass non component as args to component*) *)
| ce = any_component_expr LPAREN arg = any_component_expr COMMA args = right_flexible_list(COMMA, any_component_expr) RPAREN (* otherwise parsing is broken *) 
    { AppCExpr (ce, args) }
(* TODO unbox expr*)
%inline any_component_expr:
  t = placed(any_component_expr_)
    { t }

(********************** Signatures *********************)

(************************************ Program *****************************)
any_intercept_kind:
| x = LID
    { 
        match x with
        | "egress" -> Core.IR.Egress 
        | "ingress" -> Ingress
        | "both" -> Both
    }

any_annotation_:
(* TODO syntax should be generalized *)
|AT x=LID LPAREN b = BOOLLITERAL COMMA kind=any_intercept_kind RPAREN 
    { 
        match x with 
        | "sessioninterceptor" -> SessionInterceptor {anonymous=b; kind}
        | _ -> Core.Error.error [$loc] "Unknown annotation: %s" x 
    }
|AT x=LID LPAREN kind=any_intercept_kind RPAREN 
    { 
        match x with 
        | "msginterceptor" -> MsgInterceptor {kind}
        | _ -> Core.Error.error [$loc] "Unknown annotation: %s" x 
    }
|AT x=LID LPAREN LBRACKET schemas=right_flexible_list(COMMA, UID) RBRACKET RPAREN
    { 
        match x with
        | "onboard" -> Onboard schemas 
    }
|AT x=LID LPAREN LBRACKET interceptors=right_flexible_list(COMMA, UID) RBRACKET COMMA LBRACKET excluded_ports=right_flexible_list(COMMA, LID) RBRACKET RPAREN
    { 
        match x with
        | "capturable" -> Capturable {interceptors; excluded_ports} 
    }

%inline any_annotation:
    t = placed(any_annotation_)
    { t }

(** Preprocessor terms *)
any_pp_term_:
| USE x=LID SEMICOLON
    { UsePP [x] }
| USE l=ATTR SEMICOLON
    { UsePP l }
%inline any_pp_term:
    t = placed(any_pp_term_)
    { t }

any_term_:
| a=any_annotation
    { Annotation a }
| c=any_comments
    { Comments c }
| ppt=any_pp_term
    { PPTerm ppt }
| stmt=any_stmt
    { Stmt stmt }
| c=any_component_dcl
    { Component c }
| t_dcl = any_type_dcl_
    { t_dcl }
| AT AT DERIVE name=LID LANGLEBRACKET cargs=right_flexible_list(COMMA, any_component_expr) RANGLEBRACKET LANGLEBRACKET targs=right_flexible_list(COMMA, any_type)  RANGLEBRACKET LPAREN eargs=right_flexible_list(COMMA, any_expr) RPAREN
{
    Derive {name; cargs; targs; eargs}
}

%public %inline any_term:
    t = placed(any_term_)
    { t }

any_toplevel_term_:
| t = any_term_ 
    { t }
| f=any_function (* Only top level function are allowed *)
    { Function f}
%public %inline any_toplevel_term:
    t = placed(any_toplevel_term_)
    { t }

(*
any_field_sig_:
| x=LID COLON  t=any_ctype
    {SF (x,t)}
| x=LID COLON t=any_ctype INVARIANT i=any_term
    {SFInvariant (x,t,i)}
%inline any_field_sig:
  t = placed(any_field_sig_)
    { t }

any_method_sig_:
| x=LID args=flexible_sequence(any_ctype) 
    {SM (x, args)}
%inline any_method_sig:
  t = placed(any_method_sig_)
    { t }


any_contract_sig_:
| c = any_contract_
    { match c with C (a,b,c) -> SC (a,b,c) | ContractWith (a,b,c,d,e) -> SContractWith (a,b,c,d,e)} 

%inline any_contract_sig:
  t = placed(any_contract_sig_)
    { t }

any_port_sig_:
| x=LID ON c=UID 
    {SP (x,c)}
%inline any_port_sig:
  t = placed(any_port_sig_)
    { t }

any_signature_item_:
| FIELD f = any_field_sig
    { SField f }
| CONTRACT c = any_contract_sig    
    { SContract c}
| METHOD m = any_method_sig     
    { SMethod m }
| PORT p =any_port_sig
    { SPort p }
| SIGNATURE c = any_signature_dcl
    { SigDcl c}

%inline any_signature_item:
  t = placed(any_signature_item_)
    { t }

any_signature_dcl_:
| x = UID EQ SIG lce = flexible_sequence(any_signature_item) END    
    { CSig (x, [], lce) }
| x = UID args=flexible_sequence(any_typed_pattern) EQ SIG lce = flexible_sequence(any_signature_item) END    
    { CSig (x, args, lce) }

%inline any_signature_dcl:
  t = placed(any_signature_dcl_)
    { t }
    *)