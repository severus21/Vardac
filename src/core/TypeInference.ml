open Utils
open Error
open Easy_logging
open Fieldslib
open AstUtils
open TypingUtils
open IRMisc

module S = IR
module T = IR
open IR

let logger = Logging.make_logger "_1_ compspec.TypeInference" Debug [];;

let fplace = (Error.forge_place "TypeInference" 0 0)
include AstUtils2.Mtype.Make(struct let fplace = fplace end)

module Make () = struct
    (* Typing context for expressions *)
    let ectx : (Atom.atom, main_type) Hashtbl.t = Hashtbl.create 256
    (* Contexts of types*)
    let tctx : (Atom.atom, main_type) Hashtbl.t = Hashtbl.create 256
    (* Typing context for components *)
    let cctx : (Atom.atom, main_type) Hashtbl.t = Hashtbl.create 256

    let typeof_var_expr place x : main_type =
        if Atom.is_builtin x then
            if Builtin.is_tuple_attr (Atom.value x) || Builtin.is_inductive_attr (Atom.value x) then
                mtype_of_ft TWildcard (* Has no meaning *)
            else Builtin.type_of place (Atom.value x)
        else
            try
                Hashtbl.find ectx x
            with Not_found -> failwith (Printf.sprintf "notfound type of expr %s" (Atom.to_string x))

    let typeof_var_cexpr x : main_type =
        try
            Hashtbl.find cctx x
        with Not_found -> failwith (Printf.sprintf "notfound type of cexpr %s" (Atom.to_string x))

    let defof_tvar x : main_type = 
        try 
            Hashtbl.find tctx x
        with Not_found -> failwith (Printf.sprintf "notfound def of tvar %s" (Atom.to_string x))


    let register_expr_type x mt : unit= 
        logger#debug "> %s" (Atom.to_string x); 
        assert(Hashtbl.find_opt ectx x = None);
        Hashtbl.add ectx x mt 

    let register_cexpr_type x mt : unit = 
        assert(Hashtbl.find_opt cctx x = None);
        Hashtbl.add cctx x mt

    let register_type x mt : unit= 
        assert( Hashtbl.find_opt tctx x = None);
        Hashtbl.add tctx x mt

    let register_def_type : _typedef -> unit = 
        let fplace = (Error.forge_place "TypeUtils.register_def_type" 0 0) in
        let auto_fplace smth = {place = fplace; value=smth} in
        let ctypeof x = auto_fplace (CType(auto_fplace x)) in
        
    function
    | ClassicalDef (x, mts, ()) | EventDef (x, mts, ()) ->
        register_type x (ctypeof(TTuple mts)); (*FIXME support other things than tuple*)
        register_expr_type x (fct_sign mts (ctypeof (TVar x))) (* register constructor *)
    | ProtocolDef (x, mt) -> 
            register_type x mt; (*FIXME*)
            register_expr_type x mt (* register protocol object *)
    | VPlaceDef x -> (* No constructor, a value with name x has been created by cook *)
        register_type x  (ctypeof(TTuple [])) (*FIXME TODO correct mt *)


    (***************************************************)


    let typeof_literal l = 
        let fplace = (Error.forge_place "TypeInference.typeof_literal" 0 0) in
        let auto_fplace smth = {place = fplace; value=smth} in
        let of_tflat ft = auto_fplace(CType ( auto_fplace (TFlatType ft))) in
        let ctypeof ct = auto_fplace(CType(auto_fplace ct)) in
    match l with
    | VoidLit -> of_tflat TVoid
    | BoolLit _ -> of_tflat TBool
    | FloatLit _ -> of_tflat TFloat
    | IntLit _ -> of_tflat TInt
    | LabelLit _ -> of_tflat TLabel
    | BLabelLit _ -> of_tflat TBLabel
    | StringLit _ -> of_tflat TStr
    | ActivationRef _ -> failwith "ActivationRef Typeinference - do we need this literal since it carries no value"
    | Place _ -> failwith "Place do we need this literal since it can not exists statically"
    | VPlace _-> 
        (* TODO replace the wildcard ?*)
        (* forall x, vplace<x> - x must be unified during typecheking *)
        ctypeof (TVPlace (mtype_of_ft TWildcard))
    | StaticBridge b -> 
        mtype_of_ct (TBridge {
            (* TODO *)
            in_type = ctypeof (TVPlace (mtype_of_ft TWildcard)); 
            (* TODO *)
            out_type = ctypeof (TVPlace (mtype_of_ft TWildcard));
            protocol = mtype_of_var b.protocol_name;
        }
        )

    let typeof_unop op mt_e = 
        let fplace = (Error.forge_place "TypeInference.typeof_literal" 0 0) in
        let auto_fplace smth = {place = fplace; value=smth} in
        let of_tflat ft = auto_fplace(CType ( auto_fplace (TFlatType ft))) in
    match (op, mt_e.value) with
    | Not, _ -> of_tflat TBool
    | UnpackResult, CType{value=TResult (ok,err)} -> ok

    let typeof_binop op mt_e1 mt_e2 = 
        let fplace = (Error.forge_place "TypeInference.typeof_literal" 0 0) in
        let auto_fplace smth = {place = fplace; value=smth} in
        let of_tflat ft = auto_fplace(CType ( auto_fplace (TFlatType ft))) in
    match (op, mt_e1.value, mt_e2.value) with
    | And, _,_ | Or, _, _ | StructuralEqual, _ ,_ | Equal, _, _ | GreaterThanEqual, _,_ | LessThanEqual, _,_ | GreaterThan, _, _ | LessThan, _, _| In, _, _ -> of_tflat TBool    
    | _, CType{value=TFlatType TInt},CType{value=TFlatType TInt} -> begin   
        match op with
        | Plus | Minus | Mult | Divide -> of_tflat TInt
    end
    | _, CType{value=TFlatType TFloat},CType{value=TFlatType TFloat} -> begin   
        match op with
        | Plus | Minus | Mult | Divide -> of_tflat TInt
    end


    let typeof_block b (mts:main_type list) = 
        let fplace = (Error.forge_place "TypeInference.typeof_block" 0 0) in
        let auto_fplace smth = {place = fplace; value=smth} in
        let of_tflat ft = auto_fplace(CType ( auto_fplace (TFlatType ft))) in

        let mt, wrapper = 
            match mts with
            | [] -> begin 
                let x = Atom.fresh "x" in
                auto_fplace(CType(auto_fplace (TPolyVar x))), function (mt:_main_type) -> auto_fplace(CType(auto_fplace (TForall(x, auto_fplace mt))))
            end
            | mt::_ -> mt, function mt -> auto_fplace mt 
        in

        wrapper(CType(auto_fplace(match b with
            | Block -> failwith "typeof_block Block semantics ????"
            | List -> TList mt
            | Tuple -> assert(mts <> []); TTuple mts
            | Set -> TSet mt 
        )))
    let typeof_block2 b (mts: (main_type * main_type) list) = 
        let fplace = (Error.forge_place "TypeInference.typeof_block" 0 0) in
        let auto_fplace smth = {place = fplace; value=smth} in
        let of_tflat ft = auto_fplace(CType ( auto_fplace (TFlatType ft
        ))) in

        let mt1, mt2, wrapper = 
            match mts with
            | [] -> begin 
                let x = Atom.fresh "x" in
                let y = Atom.fresh "y" in
                auto_fplace(CType(auto_fplace (TPolyVar x))), 
                auto_fplace(CType(auto_fplace (TPolyVar y))), 
                function mt -> auto_fplace(CType(auto_fplace (TForall(x, auto_fplace(CType(auto_fplace(TForall(y, auto_fplace mt))))))))
            end
            | (mt1, mt2)::_ -> mt1, mt2, function mt -> auto_fplace mt
        in
        wrapper(CType(auto_fplace(match b with
            | Dict -> TDict (mt1, mt2) 
        )))

    let typeof_arrow ret_type args= 
        let fplace = (Error.forge_place "TypeInference.typeof_arrow" 0 0) in
        let auto_fplace smth = {place = fplace; value=smth} in
        mtype_of_fun args ret_type 
    let typeof_method (m:method0) = typeof_arrow m.value.ret_type m.value.args
    let typeof_function fdcl = typeof_arrow fdcl.value.ret_type fdcl.value.args

    let typeof_port p = 
        snd p.value (* Already computed from programmer annotation *)

    let typeof_outport p = 
        snd p.value (* Already computed from programmer annotation *)
    let typeof_state s = 
        match s.value with
        | StateDcl sdcl -> sdcl.type0
        | _ -> failwith "typeof_state state alias not supported"



    (* Search for component definition
        We do not implement equi/iso recursive types
        we used named type for components and we unfold the definition when needed (e.g. subtyping)
        But we compute the signature of a component - based on named types - before doing more.
    *)
    let rec _scan_component_item parent_opt place = function
    | Contract _ -> [] 
    | Include _ -> []
    | Method m -> 
        logger#debug "scan method %s" (match parent_opt with | None -> "None" | Some p -> Atom.to_string p);
        register_expr_type m.value.name (typeof_method m);
        [m.value.name, typeof_method m]
    | Inport p -> 
        register_expr_type (fst p.value).name (typeof_port p);
        [(fst p.value).name, typeof_port p]
    | Outport p -> 
        register_expr_type (fst p.value).name (typeof_outport p);
        [(fst p.value).name, typeof_outport p]
    | State ({value=StateDcl {name}} as s) -> 
        logger#debug "state registration";
        register_expr_type name (typeof_state s);
        [name, typeof_state s]
    | Term t -> scan_term parent_opt t 
    and scan_component_item parent_opt = map0_place (_scan_component_item parent_opt)
    and _scan_component parent_opt place = function
    | ComponentStructure cdcl -> begin
        logger#warning "collect %s" (Atom.to_string cdcl.name);
        let cstruct = List.map (scan_component_item (Some cdcl.name)) cdcl.body in 
        let cstruct = Atom.VMap.of_seq (List.to_seq (List.flatten cstruct)) in

        let signature = auto_fplace(CompType(auto_fplace(TStruct (cdcl.name, cstruct)))) in
        register_cexpr_type cdcl.name signature;
        [cdcl.name, signature]
    end
    | ComponentAssign cdcl -> failwith "TypeInference shallow ComponentAssign not yet supported" 
    and scan_component parent_opt = map0_place (_scan_component parent_opt)

    and _scan_term parent_opt place = function 
    | Component c -> scan_component parent_opt c 
    | _ -> [] 
    and scan_term parent_opt = map0_place (_scan_term parent_opt)
    let scan_program = 
        List.iter (function t -> ignore (scan_term None t))

    (************************************ Types **********************************)

    let rec _tannot_session_type parent_opt place = function
    | STEnd -> STEnd
    | STWildcard -> STWildcard
    | STVar x -> STVar x
    | STSend (mt, st) -> 
        (* CTX to propagate headers *)
        let mt = tannot_full_main_type parent_opt mt in
        let st = tannot_full_session_type parent_opt st in
        STSend (mt, st)
    | STRecv (mt, st) -> 
        let mt = tannot_full_main_type parent_opt mt in
        let st = tannot_full_session_type parent_opt st in
        STRecv (mt, st)
    | STRec (x, st) -> 
        let st = tannot_full_session_type parent_opt st in
        STRec (x, st)
    | STInline x -> STInline x 
    | STDual st -> 
        let st = tannot_full_session_type parent_opt st in
        STDual st
    | (STBranch branches as st) | (STSelect branches as st) -> 
        let tannot_full_branch parent_opt (label, st, guard_opt) =  
            let st = tannot_full_session_type parent_opt st in
            (*
            TODO need to write tannot_full_guard
            let parent_opt , guard_opt = match guard_opt with
                | None -> None
                | Some guard -> 
                    let guard = tannot_full_guard parent_opt guard in
                    Some guard
            in*)
            (label, st, guard_opt)
        in
        let branches = List.map (tannot_full_branch parent_opt) branches in

        match st with
        | STBranch _ -> STBranch branches
        | STSelect _ -> STSelect branches
    and tannot_full_session_type parent_opt st = 
        let _st = _tannot_session_type parent_opt st.place st.value in
        {place = st.place; value = _st}
    and tannot_session_type parent_opt st = tannot_full_session_type parent_opt st


    (* Searching for constraints *)
    and _tannot_composed_type parent_opt place = function 
    | TActivationRef mt -> 
        assert( mt.value <> EmptyMainType);
        TActivationRef (tannot_main_type parent_opt mt)
    | TArrow (mt1, mt2) -> TArrow (
        tannot_main_type parent_opt mt1,
        tannot_main_type parent_opt mt2
    )
    | TVar x -> TVar (x)
    | TFlatType ft -> TFlatType ft
    | TArray mt -> TArray (tannot_main_type parent_opt mt)
    | TDict (mt1, mt2) -> TDict (
        tannot_main_type parent_opt mt1,
        tannot_main_type parent_opt mt2
    )
    | TList mt -> TList (tannot_main_type parent_opt mt)
    | TOption mt -> TOption (tannot_main_type parent_opt mt)
    | TResult (mt1, mt2) -> TResult (
        tannot_main_type parent_opt mt1,
        tannot_main_type parent_opt mt2
    )
    | TSet mt -> TSet (tannot_main_type parent_opt mt)
    | TTuple mts -> TTuple (List.map (tannot_main_type parent_opt) mts)
    | TVPlace mt -> TVPlace (tannot_main_type parent_opt mt)
    | TUnion (mt1, mt2) -> TUnion (
        tannot_main_type parent_opt mt1,
        tannot_main_type parent_opt mt2
    )
    | TBridge b -> TBridge {
        in_type = tannot_main_type parent_opt b.in_type;
        out_type = tannot_main_type parent_opt b.out_type;
        protocol = tannot_main_type parent_opt b.protocol;
    }
    and tannot_composed_type parent_opt = map_place (_tannot_composed_type parent_opt)

    and _tannot_component_type parent_opt place = function
    | CompTUid x -> CompTUid x
    and tannot_component_type parent_opt = map_place (_tannot_component_type parent_opt)

    and _tannot_main_type parent_opt place = function
    | CType ct -> CType (tannot_composed_type parent_opt ct)
    | SType st -> 
        let st = tannot_full_session_type parent_opt st in
        SType st 
    | CompType ct -> CompType (tannot_component_type parent_opt ct)
    | ConstrainedType (mt, guard) -> 
        let guard = tannot_applied_constraint parent_opt guard in (* FIXME only use for timer and metadata for protocol -> should not be used on other constraitn 
        therefore only  stype and constraint type returns an outer parent_opt CType and CompType return the identity
        *)
        ConstrainedType (
            tannot_main_type parent_opt mt, 
            guard
        )
    and tannot_full_main_type parent_opt = map_place (_tannot_main_type parent_opt) 
    and tannot_main_type parent_opt mt : main_type = (tannot_full_main_type parent_opt mt)

    (******************************** Constraints ********************************)

    and _tannot_constraint_header parent_opt place = 
        let fplace = (Error.forge_place "TypeInference._tannot_constraint_header" 0 0) in
        let auto_fplace smth = {place = fplace; value=smth} in
        let ctypeof x = auto_fplace (CType(auto_fplace x)) in
    function
    | UseMetadata (mt, x) -> 
        register_expr_type x mt;
        UseMetadata (
            tannot_main_type parent_opt mt,
            x
        )
    | SetTimer x -> 
        register_expr_type x (ctypeof (TFlatType TTimer));
        SetTimer x
    | SetFireTimer (x, i) -> 
        register_expr_type x (ctypeof (TFlatType TTimer));
        SetFireTimer (x, i)
    and tannot_constraint_header parent_opt h= 
        let _h = _tannot_constraint_header parent_opt h.place h.value in
        {
            place = h.place;
            value = _h 
        }

    and tannot_applied_constraint parent_opt (headers, guard_opt) = 
        let headers = List.map (tannot_constraint_header parent_opt) headers in
        (headers, Option.map (tannot_expr parent_opt) guard_opt)

    (************************************ (V) - Place ****************************)

    and tannot_vplace parent_opt (vp:vplace) = 
    {
        name = vp.name;
        nbr_instances = tannot_expr parent_opt vp.nbr_instances;
        features = vp.features;
        children = List.map (tannot_vplace parent_opt) vp.children;
    }

    (************************************* Literals ******************************)


    and mt_of_citem parent_opt place mt_component mname = 
        let c_sign = match mt_component.value with
            | CompType {value=TStruct (_,sign)} -> sign
            | CompType {value=CompTUid name} -> begin 
                match (typeof_var_cexpr name).value with 
                |  CompType {value=TStruct (_, sign)} -> sign
                | _ -> Error.error place "internal error when fetching structural type of component"
            end
            | _ -> Error.error place "This expr has no attributes" 
        in

        let ret_type = 
            match Atom.VMap.find_opt mname c_sign with
            | None -> raise (Error.PlacedDeadbranchError (place, (Printf.sprintf "The infered component have no field/method named %s" (Atom.to_string mname))))
            | Some mt -> mt
        in
        ret_type

    and _tannot_expr parent_opt place (e, mt_e) =
        let fplace = (Error.forge_place "TypeInference.typeof_block" 0 0) in
        let auto_fplace smth = {place = fplace; value=smth} in
        let ctypeof x = auto_fplace (CType(auto_fplace x)) in

        (* Annote every things not only EmptyMainType since type expression can have been rewritten *)
        match e with  
            | VarExpr x -> 
                VarExpr x, typeof_var_expr place x 
            | ImplicitVarExpr x -> 
                ImplicitVarExpr x, typeof_var_expr place x 
            | ActivationAccessExpr (cname, e, mname) ->
                let e = tannot_expr parent_opt e in
                
                ActivationAccessExpr (cname, e, mname), mt_of_citem parent_opt place ( typeof_var_cexpr cname) mname
            | AccessExpr (e1, e2) -> begin
                let e1 = tannot_expr parent_opt e1 in
                let mt1 = (snd e1.value).value in
                let e2 = tannot_expr parent_opt e2 in
                
                let ret_type = 
                    match fst e2.value with 
                    | VarExpr x when Builtin.is_inductive_attr (Atom.value x) -> begin
                        let i = Builtin.pos_of_inductive_attr (Atom.value x) in
                        (* Check that e1 has the right type and extract type of pos i *)

                        let aux targs i =
                            let n = List.length targs in
                            if n > i then List.nth targs i 
                            else Error.error place "Can not access the %d elmts of inductive types, it has only %d parts" i n
                        in

                        match mt1 with
                        | CType{value=TVar t1} -> begin 
                            match (defof_tvar t1).value  with
                            | CType {value=TTuple targs} -> aux targs i 
                            | _ -> Error.error e1.place "This not an inductive type (1)"
                        end
                        | CType {value=TTuple targs} -> aux targs i 
                        | CType {value=TFlatType TWildcard} ->
                            (* TODO generate constraints TTuple of length >= i *)
                            mtype_of_ft TWildcard
                        | CType {value=TFlatType ft} when Builtin.is_builtin_inductive_type ft -> begin
                            let targs = Builtin.sig_of_builtin_inductive_type ft in
                            aux targs i  
                        end
                        | _ -> 
                            Error.error e1.place "This not an inductive type (2)"
                    end
                    | VarExpr x when Builtin.is_tuple_attr (Atom.value x) -> begin
                        let i = Builtin.pos_of_tuple_attr (Atom.value x) in
                        (* Check that e1 has the right type and extract type of pos i *)

                        let aux targs i =
                            let n = List.length targs in
                            if n > i then List.nth targs i 
                            else Error.error place "Can not access the %d elmts of tuple types, it has only %d parts" i n
                        in

                        match mt1 with
                        | CType{value=TVar t1} -> begin 
                            match (defof_tvar t1).value  with
                            | CType {value=TTuple targs} -> aux targs i 
                            | _ -> Error.error e1.place "This not a type"
                        end
                        | CType {value=TTuple targs} -> aux targs i 
                        | CType {value=TFlatType TWildcard} ->
                            (* TODO generate constraints TTuple of length >= i *)
                            mtype_of_ft TWildcard
                        | _ -> 
                            Error.error e1.place "This not a tuple"
                    end
                    | VarExpr field -> 
                        (* TODO Clean this *)
                        mt_of_citem parent_opt place (snd e1.value) field
                    | _ -> Error.error place "Invalid attribute"

                in
                
                AccessExpr(e1, e2), ret_type 
            end
            | BinopExpr (e1, op, e2) ->
                let e1 = tannot_expr parent_opt e1 in
                let e2 = tannot_expr parent_opt e2 in
                BinopExpr (e1, op, e2), typeof_binop op (snd e1.value) (snd e2.value)
            | InterceptedActivationRef (e1, e2_opt) ->
                let e1 = tannot_expr parent_opt e1 in
                let e2_opt = Option.map (tannot_expr parent_opt) e2_opt in
                InterceptedActivationRef(e1, e2_opt), snd e1.value 
            | LambdaExpr (params, e) -> 
                List.iter (map0_place (fun place (mt, x) -> register_expr_type x mt)) params;

                let e = tannot_expr parent_opt e in
                let mt_fct = mtype_of_fun params (snd e.value) in

                LambdaExpr (params, e), mt_fct 
            | LitExpr l -> LitExpr l, typeof_literal l.value
            | UnopExpr (op, e) -> 
                let e = tannot_expr parent_opt e in
                UnopExpr (op, e), typeof_unop op (snd e.value) 
            | CallExpr (e, es) -> 
                let e = tannot_expr parent_opt e in
                let es = List.map (tannot_expr parent_opt) es in
                let rec ret_typeof depth mt = match mt.value with (*TODO check types here ??*)
                    | _ when depth = 0 -> mt
                    | CType{value=TArrow (_, mt2)} -> ret_typeof (depth-1) mt2 
                    | CType{value=TForall(_, mt)} -> ret_typeof depth mt
                    | _ -> Error.error place "Function [%s] expect %d args, not %d" (show_expr e) ((List.length es)-depth) (List.length es)
                in

                CallExpr(e, es), ret_typeof (List.length es) (snd e.value) 
            | NewExpr (e, es) -> 
                let e = tannot_expr parent_opt e in
                let es = List.map (tannot_expr parent_opt) es in
                let rec ret_typeof depth mt = match mt.value with (*TODO check types here ??*)
                (* TODO dedup with call expr*)
                    | _ when depth = 0 -> mt
                    | CType{value=TArrow (_, mt2)} -> ret_typeof (depth-1) mt2 
                    | CType{value=TForall(_, mt)} -> ret_typeof depth mt
                    | _ -> Error.error place "Type constructor expect %d args, not %d" ((List.length es)-depth) (List.length es)
                in
                NewExpr(e, es), ret_typeof (List.length es) (snd e.value)
            | This -> begin 
                match parent_opt with
                | None -> Error.error place "[this] can not be used outside component definition" 
                | Some self -> This, auto_fplace( CompType (auto_fplace (CompTUid (self))))
            end
            | Spawn spawn -> 
                let c = tannot_component_expr parent_opt spawn.c in

                (* Component type of a spawn must be knwon staticaly *)
                assert( (snd c.value).value <> EmptyMainType );

                Spawn {
                c = c;
                args = List.map (tannot_expr parent_opt) spawn.args;
                at = Option.map (tannot_expr parent_opt) spawn.at;
            }, ctypeof(TActivationRef(snd c.value))
            | TernaryExpr (e1, e2, e3) ->
                let e1 = tannot_expr parent_opt e1 in
                let e2 = tannot_expr parent_opt e2 in
                let e3 = tannot_expr parent_opt e3 in
                TernaryExpr(e1, e2, e3), snd e2.value
            | BridgeCall b -> 
                BridgeCall b, mtype_of_ct (TBridge {
                    (* TODO *)
                    in_type = mtype_of_ft TWildcard;
                    (* TODO *)
                    out_type = mtype_of_ft TWildcard;
                    protocol = mtype_of_var b.protocol_name 
                })
            | BoxCExpr ce -> failwith "BoxCExpr Typeinference"
            | OptionExpr e_opt ->  
                let e_opt = Option.map (tannot_expr parent_opt) e_opt in
                let ct = match e_opt with
                    | Some {value=(_, mt)} -> TOption mt
                    | None -> 
                        let x = Atom.fresh "x" in
                        TForall(x, ctypeof(TOption (ctypeof (TPolyVar x))))
                in
                OptionExpr e_opt, ctypeof ct
            | ResultExpr (e1_opt, e2_opt) -> 
                let e1_opt = Option.map (tannot_expr parent_opt) e1_opt in
                let e2_opt = Option.map (tannot_expr parent_opt) e2_opt in

                let x = Atom.fresh "x" in
                let ct = match e1_opt, e2_opt with
                    | Some {value=(_, mt)}, None  -> 
                        TForall(x, ctypeof(TResult (mt, ctypeof(TPolyVar x))))
                    | None, Some{value=(_, mt)} -> TForall(x, ctypeof( TResult (ctypeof(TPolyVar x), mt)))
                in
                ResultExpr (e1_opt, e2_opt), ctypeof ct 
            | BlockExpr (b, es) -> 
                let es = List.map (tannot_expr parent_opt) es in
                let mt = typeof_block b (List.map (function e -> snd e.value)  es) in
                BlockExpr ( b, es), mt
            | Block2Expr (b, es) -> 
                let es = List.map (function (e1, e2) -> tannot_expr parent_opt e1, tannot_expr parent_opt e2) es in
                let mt = typeof_block2 b (List.map (function (e1, e2) -> snd e1.value, snd e2.value) es) in
                
                Block2Expr (b, es), mt
            | PolyApp (e, mts) -> 
                let e = tannot_expr parent_opt e in
                let mt = snd e.value in
                let rec apply = function
                | {value=CType{value=TForall (x, mt)}}, mt'::mts' ->
                    replace_type_main_type x (None, Some mt'.value) (apply (mt, mts'))
                | mt, [] -> mt  
                | mt,_ -> Error.error (place@mt.place) "Type specialization error"
                in
                (fst e.value, apply (mt, mts))


    and tannot_expr parent_opt = map_place (_tannot_expr parent_opt)

    and _tannot_stmt parent_opt place = function
    | EmptyStmt -> EmptyStmt
    | AssignExpr (x, e) -> 
        let mt_x = typeof_var_expr place x in
        let e = tannot_expr parent_opt e in

        (* TODO move this checks into TypeChecking*)
        (*if Bool.not (is_subtype (snd e.value) mt_x) then
            Error.error place "Type error: types do not match";
        *)
        
        AssignExpr (x, e)
    | AssignThisExpr (x, e) -> 
        let mt_x = typeof_var_expr place x in
        let e = tannot_expr parent_opt e in

        (* TODO move this checks into TypeChecking*)
        (*if Bool.not (is_subtype (snd e.value) mt_x) then
            Error.error place "Type error: types do not match";
        *)
        
        AssignThisExpr (x, e)
    | LetStmt (mt, x, e) -> 
        logger#debug "let %s" (Atom.to_string x);
        register_expr_type x mt;
        let e = tannot_expr parent_opt e in 
        LetStmt (mt, x, e)
    | CommentsStmt c -> CommentsStmt c
    | BreakStmt -> BreakStmt
    | ContinueStmt -> ContinueStmt
    | ExitStmt i -> ExitStmt i
    | ForStmt (mt, x, e, stmt) -> 
        register_expr_type x mt; 
        ForStmt (
            tannot_main_type parent_opt mt,
            x,
            tannot_expr parent_opt e,
            tannot_stmt parent_opt stmt
        )
    | IfStmt (e, stmt1, stmt2_opt) -> 
        let stmt1 = tannot_stmt parent_opt stmt1 in
        let stmt2_opt = Option.map (tannot_stmt parent_opt) stmt2_opt in
        
        IfStmt ( tannot_expr parent_opt e, stmt1, stmt2_opt)
    | MatchStmt (e, branches) -> MatchStmt (
        tannot_expr parent_opt e,
        List.map (function (e, stmt) -> 
            tannot_expr parent_opt e, tannot_stmt parent_opt stmt
        ) branches
    )
    | ReturnStmt e -> ReturnStmt (tannot_expr parent_opt e)
    | ExpressionStmt e -> ExpressionStmt (tannot_expr parent_opt e)
    | BlockStmt stmts -> 
        let stmts = List.map (tannot_stmt parent_opt) stmts in
        BlockStmt stmts
    | WithContextStmt (anonymous_mod, cname, e, stmts) -> 
        (* From the outside WithContextStmt is transparent in term of parent_opt *)
        let stmts = List.map (tannot_stmt parent_opt) stmts in
        WithContextStmt (anonymous_mod, cname, tannot_expr parent_opt e, stmts)
    | BranchStmt {s; label; bridge; branches} -> 
        let s = tannot_expr parent_opt s in
        let mt_st = snd s.value in

        (* TODO generalised *)
        let rec _unalias _ = 
            let already_seen = Hashtbl.create 16 in 
            function
            (* TODO rewrite just find TVar *)
            | CType {value = TVar x} -> 
                if Hashtbl.find_opt already_seen x <> None then
                    Error.error place "cyclic type alias detected"
                else Hashtbl.add already_seen x ();

                let mt = defof_tvar x in
                (unalias mt).value
            | mt -> mt
        and unalias mt = map_place _unalias mt
        in

        let mt_st = unalias mt_st in 


        let tannot_branch {branch_label; branch_s; body} = 
            register_expr_type branch_s (mtype_of_st(st_branch_of mt_st branch_label).value);
            {
                branch_label = branch_label;
                branch_s = branch_s;
                body =tannot_stmt parent_opt body;
            }
        in
        BranchStmt {
            s;
            label = tannot_expr parent_opt label;
            bridge = tannot_expr parent_opt bridge;
            branches = List.map tannot_branch branches; 
        }
    and tannot_stmt parent_opt stmt =  
        let _stmt = _tannot_stmt parent_opt stmt.place stmt.value in
        {place = stmt.place; value = _stmt }

    and _tannot_param parent_opt place (mt, x) = ( tannot_main_type parent_opt mt, x)
    and tannot_param parent_opt arg = {
        place = arg.place;
        value = _tannot_param parent_opt arg.place arg.value
    }

    and _tannot_port parent_opt place ((p, mt_p):_port*main_type) = {
        name = p.name;
        input = tannot_expr parent_opt p.input;
        expecting_st = tannot_main_type parent_opt p.expecting_st;
        callback = tannot_expr parent_opt p.callback;
    } 
    and tannot_port parent_opt p = 
        let fplace = (Error.forge_place "TypeInference.tannot_port" 0 0) in
        let auto_fplace smth = {place = fplace; value=smth} in
        let ctypeof x = auto_fplace (CType(auto_fplace x)) in

        let _p = _tannot_port parent_opt p.place p.value in
        let mt_port = ctypeof (TInport (
            snd _p.input.value,
            _p.expecting_st
        )) in

        {
            place = p.place;
            value = _p, mt_port 
        }

    and _tannot_outport parent_opt place ((p, mt_p):_outport*main_type) = {
        name = p.name;
        input = tannot_expr parent_opt p.input;
    } 
    and tannot_outport parent_opt p = 
        let fplace = (Error.forge_place "TypeInference.tannot_outport" 0 0) in
        let auto_fplace smth = {place = fplace; value=smth} in
        let ctypeof x = auto_fplace (CType(auto_fplace x)) in

        let _p = _tannot_outport parent_opt p.place p.value in
        let mt_outport = ctypeof (TOutport (
            snd _p.input.value
        )) in

        {
            place = p.place;
            value = _p, mt_outport 
        }

    and _tannot_contract parent_opt ret_type place (p:_contract) = 
        List.iter (fun (mt, x, _) -> register_expr_type x mt) p.pre_binders;

        {
            method_name = p.method_name;
            pre_binders = List.map (function (mt, x, e) -> 
                tannot_main_type parent_opt mt,
                x,
                tannot_expr parent_opt e
            ) p.pre_binders;
            ensures = Option.map (tannot_expr parent_opt) p.ensures; 
            returns = match p.returns with
            | None -> None
            | Some e -> 
                let e, _ = (tannot_expr parent_opt e).value in
                Some {place; value = e, {place; value=T.CType{place; value=T.TArrow (ret_type, {place; value=T.CType{place;value=T.TFlatType AstUtils.TBool}})}}}
        } 
    and tannot_contract parent_opt ret_type c = {
        place = c.place;
        value = _tannot_contract parent_opt ret_type c.place c.value
    }

    and _tannot_method parent_opt place (m:_method0) =
        let fplace = (Error.forge_place "TypeInference.typeof_literal" 0 0) in
        let auto_fplace smth = {place = fplace; value=smth} in
        List.iter (fun {value=(mt, x)} -> register_expr_type x mt) m.args; 
        
        {
            m with
                ret_type = tannot_main_type parent_opt m.ret_type;
                args = List.map (tannot_param parent_opt) m.args;
                body = List.map (tannot_stmt parent_opt) m.body;
                contract_opt =(Option.map (tannot_contract parent_opt m.ret_type) m.contract_opt);
        } 

    and tannot_method parent_opt m = 
        let _m = _tannot_method parent_opt m.place m.value in
        {
            place = m.place;
            value = _m
        }

    and _tannot_state parent_opt place = function 
    | StateDcl s -> 
        (* Already registerd by shallow scan*)

        StateDcl {
        name = s.name;
        ghost = s.ghost;
        type0 = tannot_main_type parent_opt s.type0;
        body = Option.map (tannot_expr parent_opt) s.body;
    } 
    and tannot_state parent_opt s = 
        let _s = _tannot_state parent_opt s.place s.value in
        {
            place = s.place;
            value = _s
        }

    and _tannot_component_item parent_opt place = function 
    | Contract s -> failwith  "contract must have been bounded to method before calling type inference"
    | Include ce -> Include (tannot_component_expr parent_opt ce)
    | Method m -> 
        let m = tannot_method parent_opt m in
        Method m 
    |Inport p -> 
        let p = tannot_port parent_opt p in
        Inport p 
    | Outport p -> 
        let p = tannot_outport parent_opt p in
        Outport p 
    | State s -> 
        let s = tannot_state parent_opt s in
        State s
    | Term t -> 
        let t = tannot_term parent_opt t in
        Term t 
    and tannot_component_item parent_opt citem = 
        let _citem = _tannot_component_item parent_opt citem.place citem.value in 
        {
            place = citem.place;
            value = _citem
        }

    and _tannot_component_dcl parent_opt place = 
        let fplace = (Error.forge_place "TypeInference._tannot_component_dcl" 0 0) in
        let auto_fplace smth = {place = fplace; value=smth} in
    function 
    | ComponentStructure cdcl as c0 -> 
        let body = List.map (tannot_component_item  (Some cdcl.name)) cdcl.body in 

        ComponentStructure {
        target_name = cdcl.target_name;
        annotations = cdcl.annotations;
        name = cdcl.name;
        body =  body (* TODO first pass allow mutual recursive function ?? - only from header *)
    } 
    | ComponentAssign {name; value} -> ComponentAssign {
        name = name;
        value = tannot_component_expr parent_opt value;
    } 
    and tannot_component_dcl parent_opt cdcl = 
        let _cdcl = _tannot_component_dcl parent_opt cdcl.place cdcl.value in
        {
            place = cdcl.place;
            value = _cdcl 
        }



    (********************** Manipulating component structure *********************)
    and _tannot_component_expr parent_opt place (ce, _)=
        match ce with 
        | VarCExpr x -> (VarCExpr x, typeof_var_cexpr x)
        | _ -> failwith "tannot_component_cexpr semantics not defined" 
    and tannot_component_expr parent_opt ce = {
        place = ce.place; 
        value = _tannot_component_expr parent_opt ce.place ce.value
    }


    (************************************ Program *****************************)

    and _tannot_function_dcl parent_opt place (fdcl:_function_dcl) = 
        let fplace = (Error.forge_place "TypeInference._tannot_function_dcl" 0 0) in
        let auto_fplace smth = {place = fplace; value=smth} in

        List.iter (fun targ -> 
            if Str.string_match (Str.regexp "[A-Z].*") (Atom.hint targ) 0 then(
                register_cexpr_type targ (auto_fplace(CompType(auto_fplace(TPolyCVar targ))));
            )else
                failwith "TODO how to specify the write number of constructor"
        ) fdcl.targs;

        let fct_sign = mtype_of_fun fdcl.args fdcl.ret_type in 
        (* Adding forall targs on top of regular signature *)
        let fct_sign = List.fold_left (fun sign tvar -> 
            mtype_of_ct (TForall (tvar, sign))     
        ) fct_sign fdcl.targs in

        register_expr_type fdcl.name fct_sign;
        List.iter (fun {value=(mt, x)} -> register_expr_type x mt) fdcl.args; 
        
        {
            name = fdcl.name;
            targs = fdcl.targs; (* TODO annote with type constraints ??*)
            ret_type = tannot_main_type parent_opt fdcl.ret_type;
            args = List.map (tannot_param parent_opt) fdcl.args;
            body = List.map (tannot_stmt parent_opt) fdcl.body;
        } 
    and tannot_function_dcl parent_opt fdcl = 
        let _fdcl = _tannot_function_dcl parent_opt fdcl.place fdcl.value in
        {
            place = fdcl.place; 
            value = _fdcl
        }

    and _tannot_typedef parent_opt place = function 
    | ClassicalDef (x, mts, body) as tdef -> 
        logger#debug "register def %s" (Atom.to_string x);
        register_def_type tdef;
        ClassicalDef (
            x,
            List.map (tannot_main_type parent_opt) mts,
            body
        ) 
    | EventDef (x, mts, body) as tdef -> 
        register_def_type tdef;
        EventDef (
        x,
        List.map (tannot_main_type parent_opt) mts,
        body
    )  
    | ProtocolDef (x, mt) as tdef -> 
        register_def_type tdef;
        ProtocolDef (
        x,
        tannot_main_type parent_opt mt
    )  
    | VPlaceDef x as tdef ->
        register_def_type tdef;
        VPlaceDef x  
    and tannot_typedef parent_opt tdef = 
        let _tdef = _tannot_typedef parent_opt tdef.place tdef.value in
        {
            place = tdef.place;
            value = _tdef 
        }

    and _tannot_term parent_opt place = function 
    | EmptyTerm -> EmptyTerm
    | Comments c -> Comments c
    | Stmt stmt -> 
        let stmt = tannot_stmt parent_opt stmt in
        Stmt stmt
    | Component c -> 
        let c = tannot_component_dcl parent_opt c in
        Component c 
    | Function f -> 
        let f = tannot_function_dcl parent_opt f in
        Function f 
    | Typealias (x, body) -> begin
        match Option.map (tannot_main_type parent_opt) body with(*FIXME why an option for a type alias *)
        | None -> Typealias (x, None)
        | Some mt -> 
            register_type x mt;
            Typealias (x, Some mt)
    end
    | Typedef tdef -> 
        let tdef = tannot_typedef parent_opt tdef in
        Typedef tdef
    | Derive derive ->
        let cargs = List.map (tannot_component_expr parent_opt) derive.cargs in
        let targs = List.map (tannot_main_type parent_opt) derive.targs in
        let eargs = List.map (tannot_expr parent_opt) derive.eargs in
        Derive {name = derive.name; cargs; targs; eargs} 
    and tannot_term parent_opt t = 
        let _t = _tannot_term parent_opt t.place t.value in
        {
            place = t.place;
            value = _t 
        }

    and tannot_program program = 
        (* Scan header for recursive definition of function, method and state *)
        scan_program program; 
        List.map (tannot_term None) program


    (**********************************************************)
    let displayed_pass_shortdescription = "IR has been annotated with types (type reconstruction only)"
    let displayed_ast_name = "annotated IR (with types)"
    let show_ast = true
    let precondition program = program
    let postcondition program = program
    let apply_program = tannot_program
end