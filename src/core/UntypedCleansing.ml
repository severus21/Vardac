(*
    To be run before TypeInference/...
    - remove type aliasing -> TODO
        - inline all session types (i.e get ride of session types aliasing + resolve STInline)
    - session type 
        - dual elimination
*)

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

let type_aliasing_elimiation program = 
    (* NB each type binder introduce an unique identity *)

    let typedefs = Hashtbl.create 16 in

    let select = function 
        | Typedef {value=ProtocolDef (x,mt)} -> 
            Hashtbl.add typedefs x mt; true
        | Typedef {value=VPlaceDef x} -> 
            Hashtbl.add typedefs x (mtype_of_ct(TTuple [])); true
        | Typedef {value=ClassicalDef (x, args, ())} -> 
            Hashtbl.add typedefs x (mtype_of_ct(TTuple args)); true
        | Typealias (x, body) -> begin 
            match body with 
            | None -> true (* TODO why an option for type alias*)
            | Some mt -> Hashtbl.add typedefs x mt; true 
        end
        | _ -> false
    in

    let collect = fun _ _ _ -> []  in

    (* Collect typedefs*)
    collect_term_program true select collect;

    let alias_selector = function 
        | CType {value=TVar _} -> true 
        | SType {value=STInline _} -> true
        | _ -> false
    in

    let unalias =
        (* Cycle detection *)
        let already_seen = Hashtbl.create 16 in 


        let rec _alias_rewriter place = function
            | CType {place; value=TVar x} -> begin 
                if Hashtbl.find_opt already_seen x <> None then
                    Error.error place "cyclic type alias detected"
                else Hashtbl.add already_seen x ();

                try
                    let mt = Hashtbl.find typedefs x in
                    (alias_rewriter mt).value
                with Not_found ->  raise (PlacedDeadbranchError (place, Printf.sprintf "Unalias TVar: type [%s] is unknown" (Atom.to_string x))) 
            end
            | SType{place; value=STInline x} -> begin 
                try
                    let mt = Hashtbl.find typedefs x in 
                    (alias_rewriter mt).value
                with Not_found -> raise (PlacedDeadbranchError (place, Printf.sprintf "Unalias - STInline: type [%s] is unknown" (Atom.to_string x))) 
            end
            (* Propagation *)
            | mt -> (rewrite_type_mtype alias_selector (_alias_rewriter place) {place; value=mt}).value
        and alias_rewriter mt = map_place _alias_rewriter mt
        in

        alias_rewriter
    in

    (* Un-alias types*)
    let unalias_mt_select = function 
        | CType {value=TVar _} -> true
        | SType { value=STInline _} -> true
        | _ -> false
    in
    let unalias_mt_rewrite mt = (unalias (auto_fplace mt)).value
    in

    rewrite_type_program unalias_mt_select unalias_mt_rewrite program

let rec dual_elimination program = 
    let select = function
        | SType st -> true
        | _ -> false
    in

    let rec _dual_elim place = function 
        | STEnd -> STEnd
        | STInline x -> STInline x 
        | STDual st -> (dual st).value
        | STBranch entries -> 
            STBranch (List.map dual_elim_branch entries) 
        | STRec (x, st) -> STRec (x, dual_elim st) 
        | STRecv (mt, st) -> 
            STRecv (
                rewrite_type_mtype select rewrite mt, 
                dual_elim st)
        | STSelect entries -> 
            STSelect (List.map dual_elim_branch entries) 
        | STSend (mt, st) -> 
            STSend (
                rewrite_type_mtype select rewrite mt, 
                dual_elim st)
        | STVar x -> STVar x
        | STPolyVar x -> STPolyVar x
    and dual_elim_branch (x, st, ac_opt) = (x, dual_elim st, Option.map (rewrite_type_aconstraint select rewrite) ac_opt)
    and dual_elim st = map_place _dual_elim st
    and rewrite (SType st) = SType (dual_elim st)
    in




    rewrite_type_program select rewrite program 


(*****************************************************)

let displayed_pass_shortdescription = "recv has been eliminated from IR"
let displayed_ast_name = "IR recvelim"
let show_ast = true

let precondition program = program 

let postcondition program = 
    (* Ensures there are no remaining Typealias *)
    collect_term_program true (function | Typealias _ -> true | _-> false) (fun _ place _ -> Error.error place "Typealias remains after UntypeCleansing") program;

    (* FIXME collect_type is not recursive yet *)
    collect_type_program Atom.Set.empty (function | SType {value=STInline _} -> true | _-> false) (fun _ _ {place; _} -> Error.error place "STInline remains after STInline") program;

    (* FIXME collect_type is not recursive yet *)
    collect_type_program Atom.Set.empty (function | SType {value=STDual _} -> true | _-> false) (fun _ _ {place; _} -> Error.error place "Dual remains after STInline") program;

    program 

let apply_program program = 
    program
    |> type_aliasing_elimiation
    |> dual_elimination