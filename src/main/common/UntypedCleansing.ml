(*
    To be run before TypeInference/...
    - remove type aliasing
        - inline all session types (i.e get ride of session types aliasing + resolve STInline)
    - session type 
        - dual elimination
*)
open Core
open Utils
open Error
open Easy_logging
open AstUtils
open TypingUtils
open IRMisc
open IR
 

let logger = Logging.make_logger "_1_ compspec.core" Debug [];;
let fplace = (Error.forge_place "UntypedCleansing" 0 0) 
let auto_fplace smth = {place = fplace; value=smth}
include AstUtils2.Mtype.Make(struct let fplace = fplace end)

module Make () = struct
    let typedefs = Hashtbl.create 16

    let hydrate_typedefs program = 
        (* NB each type binder introduce an unique identity *)
        let select = function 
            | Component {value=ComponentAssign {name;}} |  Component {value=ComponentStructure {name;}} -> (* N.B. each component schema defines a type *)
                Hashtbl.add typedefs name (mtype_of_cvar name); true
            | Typedef {value=ProtocolDef (x,mt)} -> 
                Hashtbl.add typedefs x mt; true
            | Typedef {value=EventDef (x,args, ())} -> 
                (* Event type is not aliasing *)
                false
            | Typedef {value=VPlaceDef x} -> 
                Hashtbl.add typedefs x (mtype_of_ct(TTuple [])); true
            | Typedef {value=ClassicalDef (x, args, ())} -> 
                (* Inductive type is not aliasing *)
                false
            | Typealias (x, body) -> begin 
                match body with 
                | None -> true (* TODO why an option for type alias*)
                | Some mt -> Hashtbl.add typedefs x mt; true 
            end
            | _ -> false
        in

        let collect = fun _ _ _ -> []  in

        collect_term_program true select collect program;

        (* debug only
        logger#debug "collected typedefs %d" (Hashtbl.length typedefs);
        Hashtbl.iter (fun k v -> 
            logger#debug "- %s" (Atom.to_string k);    
        ) typedefs;*)
        ()

    let type_aliasing_elimiation program = 

        let alias_selector = function 
            | CType {value=TVar _} -> true 
            | _ -> false
        in

        let rec unalias mt0 =
            let rec _alias_rewriter already_seen place = function
                | CType {place; value=TVar x} as mt0-> begin 
                    (* Cycle detection *)
                    if Atom.Set.mem x already_seen then
                        Error.error place "cyclic type alias detected"
                    else ();

                    let already_seen = Atom.Set.add x already_seen in

                    match Hashtbl.find_opt typedefs x with
                    | Some mt -> 
                        (* Recursive cleansing*)
                        (unalias (alias_rewriter already_seen mt)).value
                    | None -> mt0  
                end
                | SType{place; value=STInline x} -> begin 
                    try
                        let mt = Hashtbl.find typedefs x in 
                        (unalias (alias_rewriter already_seen mt)).value
                    with Not_found -> raise (PlacedDeadbranchError (place, Printf.sprintf "Unalias - STInline: type [%s] is unknown" (Atom.to_string x))) 
                end
                (* Propagation *)
                | mt -> (rewrite_type_mtype alias_selector (_alias_rewriter already_seen place) {place; value=mt}).value
            and alias_rewriter already_seen = map_place (_alias_rewriter already_seen)
            in

            alias_rewriter Atom.Set.empty mt0
        in

        (* Unalias typedefs *)
        Hashtbl.filter_map_inplace (fun _ v -> Some (unalias v)) typedefs; 


        (* Un-alias types*)
        let unalias_mt_select = function 
            | CType {value=TVar _} -> true
            | _ -> false
        in
        let unalias_mt_rewrite mt = (unalias (auto_fplace mt)).value
        in

        rewrite_type_program unalias_mt_select unalias_mt_rewrite program

    (* 
        * STInline elimination
        * STDual elimination
    *)
    let rec stype_cleansing program : program =
        let stype_selector = function 
            | STInline _ -> true 
            | STDual _ -> true
            | _ -> false
        in

        let rec stype_rewriter st0 : _session_type =
            let rec _aux_stype_rewriter already_seen place : _session_type -> _session_type = function
                | STDual st -> stype_rewriter (dual st).value
                | STInline x -> begin 
                    try
                        let mt = Hashtbl.find typedefs x in 

                        match mt.value with
                        | SType st -> (stype_rewriter (aux_stype_rewriter already_seen st).value)
                        | CType{value=TVar _} -> raise (DeadbranchError "STVar remains in typedefs when cleaning stypes")
                    with Not_found -> raise (PlacedDeadbranchError (place, Printf.sprintf "Unalias - STInline: type [%s] is unknown" (Atom.to_string x))) 
                end
                (* Propagation *)
                | st -> 
                    (rewrite_stype_stype stype_selector 
                        (_aux_stype_rewriter already_seen place) 
                        {place; value=st}
                    ).value
            and aux_stype_rewriter already_seen : session_type -> session_type = map_place (_aux_stype_rewriter already_seen)
            in

            (aux_stype_rewriter Atom.Set.empty (auto_fplace st0)).value
        in

        rewrite_stype_program stype_selector stype_rewriter program

    (*****************************************************)
    let name = "UntypedCleansing"
    let displayed_pass_shortdescription = "types has been cleaned from IR"
    let displayed_ast_name = "IR type cleansing"
    let show_ast = true
    let global_at_most_once_apply = false

    let precondition program = program 

    let postcondition program = 
        (* Ensures there are no remaining Typealias *)
        collect_term_program true (function | Typealias _ -> true | _-> false) (fun _ place _ -> Error.error place "Typealias remains after UntypeCleansing") program;

        (* FIXME collect_type is not recursive yet *)
        collect_stype_program Atom.Set.empty (function | STInline _ -> true | _-> false) (fun _ _ {place; _} -> Error.error place "STInline remains after STInline") program;

        (* FIXME collect_type is not recursive yet *)
        collect_stype_program Atom.Set.empty (function | STDual _ -> true | _-> false) (fun _ _ {place; _} -> Error.error place "STDual remains after STInline") program;

        program 

    let apply_program program : program = 
        hydrate_typedefs program;

        program
        |> type_aliasing_elimiation
        |> stype_cleansing
end