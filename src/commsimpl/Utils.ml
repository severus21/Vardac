open Core
open IR
open Easy_logging
open Utils
open AstUtils
open IRMisc

(*@param where res - name let res = receive*)
let compute_intermediate_args remaining_stmts res_opt = 
    let _, intermediate_args = List.fold_left_map free_vars_stmt Atom.Set.empty remaining_stmts in
    let intermediate_args = List.flatten intermediate_args in

    (* Safety check *)
    List.iter (function |({value=EmptyMainType}, x) -> logger#error "%s" (Atom.to_string x);assert(false) | _ -> ()) intermediate_args;

    (* Remove components *)
    let intermediate_args = List.filter (function (_, x) -> 
        (Str.string_match (Str.regexp "^[A-Z].*") (Atom.hint x) 0) = false) intermediate_args 
    in

    (* Case "let res = receive(..)"
        res = Tuple.of(e, session); res must not be loaded from an intermediate state
    *)
    let intermediate_args = 
        (
            match res_opt with 
            | None -> Fun.id
            | Some res -> List.filter (function (_, x) -> x <> res) 
        )
        intermediate_args 
    in

    Utils.deduplicate (function (_,x) -> Atom.to_string x) intermediate_args