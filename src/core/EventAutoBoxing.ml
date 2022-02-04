open IR
open Easy_logging
open Utils
open AstUtils

let logger = Logging.make_logger "_1_ compspec" Debug [];;
let fplace = (Error.forge_place "EventAutoBoxing" 0 0) 
let auto_fplace smth = {place = fplace; value=smth}
include AstUtils2.Mtype.Make(struct let fplace = fplace end)

module TypeInference = IRCompilationPass.Make(TypeInference)

(* TODO define it outside *)
module MTHashtbl = Hashtbl.Make(
    struct  
        type t = main_type
        let equal x y = equal_mtype x y 
        let hash mt = 
            let base_ft = 2. in
            let base_ct = 3. in
            let base_st = 5. in
            let base_cmt = 7. in
            let base_mt = 11. in

            let rec hash_ft ft = base_ft ** float_of_int (Hashtbl.hash ft)  
            and _hash_ct _ = function
            | TActivationRef mt -> (base_ct ** 1.) :: hash_mt mt
            | TArrow (mt1, mt2) -> (base_ct ** 2.) :: (hash_mt mt1) @ (hash_mt mt2)
            | TVar x -> (base_ct ** 3.) :: [float_of_int (Hashtbl.hash x)]
            | TFlatType ft -> (base_ct ** 4.) :: [hash_ft ft]
            | TArray mt -> (base_ct ** 5.) :: hash_mt mt
            | TDict (mt1, mt2) -> (base_ct ** 6.) :: (hash_mt mt1) @ (hash_mt mt2)
            | TList mt -> (base_ct ** 7.) :: (hash_mt mt)
            | TTuple mts -> (base_ct ** 11.) :: (List.flatten (List.map hash_mt mts))
            and hash_ct ct = map0_place _hash_ct ct
            and _hash_st _ = function
            | STEnd -> [ base_st ** 1.]
            | STVar x -> (base_st ** 2.) :: [float_of_int (Hashtbl.hash x)]
            and hash_st st = map0_place _hash_st st
            and _hash_cmt _ = function
            | CompTUid x -> (base_cmt ** 1.)  :: [float_of_int (Hashtbl.hash x)]
            and hash_cmt cmt = map0_place _hash_cmt cmt
            and _hash_mt _ = function 
            | EmptyMainType -> [0.]
            | CType ct -> (base_mt ** 1.) :: hash_ct ct 
            | SType st -> (base_mt ** 2.) :: hash_st st 
            | CompType cmt -> (base_mt ** 3.) :: hash_cmt cmt
            | ConstrainedType _ -> failwith "hash_cmt" 
            and hash_mt mt = map0_place _hash_mt mt in

            Hashtbl.hash (hash_mt mt)
        end)

            


(************************************ Program *****************************)

let hashtbl_mt2event = MTHashtbl.create 16 
let events = Hashtbl.create 16

let mt2event mt = 
    match MTHashtbl.find_opt hashtbl_mt2event mt with
    | Some (_, e) -> e 
    | None -> 
        let event = Atom.fresh "auto_boxed_type" in
        MTHashtbl.add hashtbl_mt2event mt (mt, event);
        event

let rec _autobox_st _ st = 
match st with
| STEnd | STVar _ | STInline _ -> st
| STSend (t_msg, st_continuation) | STRecv (t_msg, st_continuation) -> begin
    let st_continuation = autobox_st st_continuation in
    let t_msg = 
        mtype_of_var
        (match t_msg.value with
        (* event *)
        | CType {value = TVar x } when Hashtbl.find_opt events x <> None -> x
        (* Not an event *)
        | _ -> 
            logger#debug "auto_boxing %s" (show__main_type t_msg.value);
            mt2event t_msg
        )
    in

    match st with
    | STSend _ -> STSend (t_msg, st_continuation)
    | STRecv _ -> STRecv (t_msg, st_continuation)
end
| STBranch branches | STSelect branches -> begin
    let branches = 
        List.map 
            (function (label, st_branch, opt) -> (label, autobox_st st_branch, opt))
            branches
    in

    match st with
    | STBranch _ -> STBranch branches
    | STSelect _ -> STSelect branches
end
| STRec (x, st) -> STRec (x, autobox_st st)
| STDual st -> STDual (autobox_st st)
and autobox_st st = map_place _autobox_st st

let generate_eventdefs () : term list = 
    List.of_seq (
        Seq.map 
            (function  (mt, e) -> 
                auto_fplace (Typedef (auto_fplace (EventDef (e, [mt], ()))))
            )
            (MTHashtbl.to_seq_values hashtbl_mt2event)
    )

let autobox_program program : IR.program = 
    (*** Hydrates events ***)
    let event_selector = function
    | Typedef {value = EventDef (e, _, _)} -> Hashtbl.add events e (); false
    | _ -> false
    in
    collect_term_program false event_selector (function _ -> failwith "") program;

    (*** Update st types everywhere ***)
    let selector = function 
        | SType st -> true
        | _ -> false
    in
    let rewritor = function 
        | SType st -> SType (autobox_st st) 
    in

    let program = rewrite_type_program selector rewritor program in

    (*** Need to recompute all types ***)
    let program = TypeInference.apply program in

    (*** Auto-box expr (event creation/destruction) ***)
    (* TODO *)


    (*** Add events def ***)
    (* TODO
        auto boxing can works for user-defined types since we insert generate_eventdefs before definitions of those types 
    *)
    generate_eventdefs () @ program


(**********************************************************)
let displayed_pass_shortdescription = "non event msg have been boxed into events"
let displayed_ast_name = "autto-boxed IR"
let show_ast = true
let precondition program = program
let postcondition program = program
let apply_program = autobox_program

