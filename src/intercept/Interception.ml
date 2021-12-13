open Core
open AstUtils
open IR
open Easy_logging

let logger = Logging.make_logger ("_1_ compspec.Intercept") Debug [];;

let fplace = (Error.forge_place "Intercept" 0 0) 
let auto_fplace smth = {place = fplace; value=smth}
include AstUtils2.Mtype.Make(struct let fplace = fplace end)

let make_citem_for_intercepted_component intercepted_cname = 
    let intercepted_struct : component_structure = failwith "TODO get intercepted_struct" in


    let interception_states = [] in 
    let interception_callbacks = [] in

    (* Input ports and bridges *)
    let intercepted_input_ports = List.filter (
        function 
        |{value=Port _} -> true | _ -> false) intercepted_struct.body in

    let interception_ports = List.map (function |{place; value=Port {value=p,_; place=p_port}} -> 
        auto_fplace (Port (auto_fplace ({
            name = Atom.fresh ("interceptor_pinput_"^(Atom.to_string intercepted_cname)^"_"^(Atom.to_string p.name));
            input = p.input;
            expecting_st = p.expecting_st; (* FIXME if not anonymous add the identity propagation ?? *)
            callback = failwith "TODO generate callback for interception port" 
            (*
            - select interceptor
            - wrap interceptor
            *)
        }, auto_fplace EmptyMainType)))    
    ) intercepted_input_ports in
    
    (* TODO rewrite bridges types *)
    


    (* Output ports and bridges *)
    (* FIXME TODO Receive case ??? -> should have been rewritten or smth else *)

    (interception_states, interception_ports, interception_callbacks)


let make_interceptor place = function
| ComponentAssign {name; value={value=(AppCExpr ({value=VarCExpr functorname, _}, args)), _}} when Atom.hint functorname = "MakeInterceptor" && Atom.is_builtin functorname-> begin
    (* Ad-hoc functor since we do not have meta programming capabilities *)
    match args with
    | [{value=VarCExpr interceptor_name,_;}; {value=UnboxCExpr {value=BlockExpr (List, component_types), _}, _}] 
    when List.fold_left (function flag -> function | {value=BoxCExpr {value=VarCExpr _,_}, _} -> true | _ -> false) true component_types ->
        let spawned_component_types = List.map (function
            |{value=BoxCExpr {value=VarCExpr cname, _}, _} -> cname
        ) component_types in

        let base_interceptor : component_structure = failwith "TODO extract base interceptor (i.e. user defined or not)" in
        (* base_interceptor must be component not a functor *)
        assert(base_interceptor.args = []);

        let base_onstartup = Option.map (function {value=Method m} -> m) (List.find_opt (function | {value=Method m} -> m.value.on_startup | _ -> false) base_interceptor.body) in 
        let m_interceptors = List.filter (function |
        {value=Method m} -> List.mem Intercept m.value.annotations | _ -> false) base_interceptor.body in
        let other_citems =List.filter (function |
        {value=Method m} -> Bool.not (List.mem Intercept m.value.annotations) | _ -> true) base_interceptor.body in


        let onstartup_args = 
            [] @
            match base_onstartup with
            | None -> []
            | Some m -> m.value.args 
        in
        let onstartup_body = 
            [] @
            match base_onstartup with
            | None -> []
            | Some m -> m.value.body 
        in
        let onstartup = auto_fplace (Method (auto_fplace {
            annotations = (match base_onstartup with | None -> [] | Some m -> m.value.annotations);
            ghost = false;
            ret_type = mtype_of_ft TVoid;
            name = Atom.fresh "interceptor_onstartup";
            args = onstartup_args;
            body = onstartup_body; 
            contract_opt = (match base_onstartup with | None -> None | Some m -> m.value.contract_opt);
            on_startup = true;
            on_destroy = false;
        })) in (* TODO we need to add logic here ?? *)

        let tmp = List.map make_citem_for_intercepted_component spawned_component_types in
        let interception_states = List.flatten (List.map (function (x,_,_) -> x) tmp) in 
        let interception_ports = List.flatten (List.map (function (_,y,_) -> y) tmp) in 
        let interception_callbacks = List.flatten (List.map (function (_,_,z) -> z) tmp) in 


        let structure = {
            target_name = base_interceptor.target_name; 
            annotations = base_interceptor.annotations; (* NB. remove annotations that have been consumed *)
            name = name; 
            args = []; (* Not a functor *)
            body = 
                onstartup ::
                other_citems @
                interception_states @
                interception_ports @
                interception_callbacks;
        }
        in

        (Component (auto_fplace (ComponentStructure structure)))
    | _ -> Error.error place "Functor [MakeInterceptor] expect two arguments : the Interceptor component and a list of Component type that should be intercepted" 
end