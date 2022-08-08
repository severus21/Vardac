open Core
open IR
open Easy_logging
open Utils
open AstUtils
open IRMisc

let fplace = (Error.forge_place "EventAutoBoxing" 0 0) 
let auto_fplace smth = {place = fplace; value=smth}
include AstUtils2.Mtype.Make(struct let fplace = fplace end)

module TypeInference1 = IRCompilationPass.Make(TypeInference.Make())
module TypeInference2 = IRCompilationPass.Make(TypeInference.Make())

(* TODO define it outside *)
module MTHashtbl = Hashtbl.Make(
    struct  
        type t = main_type
        let equal = [%equal: main_type] 
        let hash = [%hash: main_type]
    end)

            

module Make () = struct 
    let logger = make_log_of "EventAutoBoxing"

    let hashtbl_mt2event = MTHashtbl.create 16 
    let events = Hashtbl.create 16

    (* debug only *)
    let debug_print_mt2event () = 
        logger#debug "mt2event {\n%s}" (Error.show_list "\n" (fun out (x,_) ->
            Format.fprintf out "\t- %d (%s)" ([%hash: main_type] x) (show_main_type x)
        ) (List.of_seq(MTHashtbl.to_seq hashtbl_mt2event)))

    let mt2event flag_register mt = 
        match MTHashtbl.find_opt hashtbl_mt2event mt with
        | Some (_, e) -> e 
        | None -> 
            let event = Atom.fresh "auto_boxed_type" in
            logger#debug "auto-box %s -> %s" (show_main_type mt) (Atom.to_string event);
            (*if flag_register then MTHashtbl.add hashtbl_mt2event mt (mt, event);*)
            MTHashtbl.add hashtbl_mt2event mt (mt, event);
            debug_print_mt2event ();
            event
        
    let needs_autoboxing = function
        | {value=CType {value = TVar x }} -> 
            logger#debug "need auto-boxing for %s<%b>" (Atom.to_string x) (Hashtbl.find_opt events x = None);
            Hashtbl.find_opt events x = None
        (* BLabel is a "builtin" event *)
        | {value=CType {value = TFlatType TBLabel}} -> false
        | _ as ct -> 
            (*logger#debug "need auto-boxing for %s<true>" (show_main_type ct);*)
            true

    let rec _autobox_st flag_register _ st = 
    match st with
    | STEnd | STWildcard | STBottom | STVar _ | STInline _ -> st
    | STSend (t_msg, st_continuation) | STRecv (t_msg, st_continuation) -> begin
        let st_continuation = autobox_st flag_register st_continuation in
        let t_msg = 
            if needs_autoboxing t_msg then begin
                (*logger#debug "auto_boxing %s" (show__main_type t_msg.value);*)
                mtype_of_var (mt2event flag_register t_msg)
            end else t_msg
        in

        match st with
        | STSend _ -> STSend (t_msg, st_continuation)
        | STRecv _ -> STRecv (t_msg, st_continuation)
    end
    | STBranch branches | STSelect branches -> begin
        let branches = 
            List.map 
                (function (label, st_branch, opt) -> (label, autobox_st flag_register st_branch, opt))
                branches
        in

        match st with
        | STBranch _ -> STBranch branches
        | STSelect _ -> STSelect branches
    end
    | STRec (x, st) -> STRec (x, autobox_st flag_register st)
    | STDual st -> STDual (autobox_st flag_register st)
    and autobox_st flag_register st = map_place (_autobox_st flag_register) st

    let generate_eventdefs () : term list = 
        List.of_seq (
            Seq.map 
                (function  (mt, e) -> 
                    auto_fplace (auto_plgannot(Typedef (auto_fplace (EventDef (e, [mt], ())))))
                )
                (MTHashtbl.to_seq_values hashtbl_mt2event)
        )

    let autobox_program program : IR.program = 
        (*** Hydrates events ***)
        let event_selector = function
        | Typedef {value = EventDef (e, _, _)} -> Hashtbl.add events e (); false
        | _ -> false
        in

        collect_term_program false event_selector (function _ -> failwith "why a failwith") program;
        
        (*** Need to recompute all types ***)
        let program = TypeInference1.apply program in

        (*** Auto-box expr (event creation/destruction) ***)
        (* sending -> fire | incomming receive and inport callback *)
        let expr_selector : _expr -> bool = function
            | CallExpr ({value= (VarExpr x, _)}, args) when Atom.hint x = "fire" && Atom.is_builtin x -> true
            | CallExpr ({value= (VarExpr x, _)}, args) when Atom.hint x = "receive" && Atom.is_builtin x -> true 
            | _ -> false
        in
        let expr_rewritor mt e =
            match e with
            | CallExpr ({place; value= (VarExpr x, _)}, args) when Atom.hint x = "fire" && Atom.is_builtin x -> 
                logger#debug "fire auto-boxing";
                let [s; msg] = args in

                let e_msg, t_msg = msg.value in
                if needs_autoboxing t_msg then( 
                    logger#debug "needs auto-boxing_fire";
                    let event = mt2event true t_msg in
                    CallExpr ( e2var x, [
                        s;
                        e2_e (NewExpr(
                            e2var event,
                            [ e2_e e_msg ]
                        ))
                    ])
                )else e
            | CallExpr ({place; value= (VarExpr x, _)}, args) when Atom.hint x = "receive" && Atom.is_builtin x -> 
                logger#debug "receive auto-boxing";
                let [s] = args in

                let t_msg, st_continuation = msgcont_of_st (match (snd s.value).value with | SType st -> st) in

                (* tuple<event, s> -> tuple<unboxed, s>*)
                if needs_autoboxing t_msg then 
                    let param_res = Atom.fresh "res_autoboxing" in
                    let mt_res = mtype_of_ct (TTuple [t_msg; mtype_of_st st_continuation.value]) in
                    let e_param_res = auto_fplace(VarExpr param_res, mt_res) in

                    let autounbox = 
                        e2_e (LambdaExpr(
                            [ auto_fplace (mt_res, param_res) ],
                            e2_e(BlockExpr(
                                Tuple,
                                [
                                    (*unboxed msg*)
                                    e2_e (AccessExpr (
                                        e2_e(AccessExpr(
                                            e_param_res,
                                            e2var (Atom.builtin "_0")
                                        )),
                                        e2var (Atom.builtin "_0_")
                                    ));
                                    (* session preserved *)
                                    e2_e(AccessExpr(
                                        e_param_res,
                                        e2var (Atom.builtin "_1")
                                    )) 
                                ]
                            ))
                        ))
                    in

                    CallExpr(
                        autounbox,
                        [ auto_fplace (e, mt) ]
                    )
                else e
        in

        let program = rewrite_expr_program expr_selector expr_rewritor program in

        let inport_selector = function
        | Inport _ -> true
        | _ -> false
        in
        let inport_rewritor _ = function
        | Inport p as t -> 
            logger#debug "inport auto-boxing %s" (Atom.to_string (fst p.value).name);
            let t_msg, st_continuation = IRMisc.msgcont_of_st (match (fst p.value).expecting_st.value with | SType st -> st) in

            [
                if needs_autoboxing t_msg then
                    let event = mt2event true t_msg in

                    let param_msg = Atom.fresh "msg" in
                    let param_session = Atom.fresh "session" in

                    Inport (auto_fplace ({ (fst p.value) with
                        (* St type of p will be rewritten afterwards by the type rewritten pass *)
                        callback = 
                            e2_e(LambdaExpr (
                                [ 
                                    auto_fplace (mtype_of_var event, param_msg);
                                    auto_fplace (mtype_of_st st_continuation.value, param_session)
                                ],
                                e2_e(CallExpr(
                                    (fst p.value).callback,
                                    [
                                        e2_e (AccessExpr (
                                                e2var param_msg,
                                                e2var (Atom.builtin "_0_")
                                        ));
                                        e2var param_session 
                                    ]
                                ))
                            ))
                    }, auto_fplace EmptyMainType))
                else t
            ]
        in

        let program = rewrite_citem_program inport_selector inport_rewritor program in

        (*** Update st types everywhere ***)
        let selector = function 
            | SType st -> true
            | _ -> false
        in
        let rewritor = function 
            | SType st -> 
                (*logger#debug "scan st for auto-boxing\n%s" (show_session_type st);*)
                SType (autobox_st false st) (* False in order to avoid rewriting unused type annotation in the AST like the builtin signature of fire/receive *)
        in

        let program = rewrite_type_program selector rewritor program in

        (*** Add events def ***)
        (* TODO
            auto boxing can not works for user-defined types since we insert generate_eventdefs before definitions of those types 
        *)
        (* let program = generate_eventdefs () @ program in *)

        (* toplevel *)
        logger#debug "inserting generated event definitions for autoboxing";
        let program = insert_terms_into_lca [None] (generate_eventdefs ()) program in

        (*** Need to recompute all types ***)
        let program = TypeInference2.apply program in

        program



    (**********************************************************)
    let name = "EventAutoBoxing"
    let displayed_pass_shortdescription = "non event msg have been boxed into events"
    let displayed_ast_name = "auto-boxed IR"
    let show_ast = true

    (* This pass needs the global_at_most_once_guarante since
        it insert new events for each detection
        TODO: Check if the pass is idempotent: true -> false
            I don't think so
    *)
    let global_at_most_once_apply = true



    let precondition program = program
    let postcondition program = program
    let apply_program = autobox_program
end