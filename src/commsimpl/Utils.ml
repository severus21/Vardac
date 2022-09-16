open Core
open IR
open Easy_logging
open Utils
open AstUtils
open IRMisc

(* rcev -> toplevel let or toplevel expression statement (return etc ...) or nested let .. =recv in block (if, ..)*)
    let rec to_X_form token place stmt : stmt list =
        let fplace = (Error.forge_place "Core.Rewrite.to_X_form" 0 0) in
        let auto_place smth = {place = place; value=smth} in
        let auto_fplace smth = {place = fplace; value=smth} in

        (* 
            extract_recv "f(s.recv(...))"
            =>
            [ "fresh_name = s.recv(...)"], "f(fresh_name)"
        *)
        let recv_selector = function
            | (CallExpr ({value=(VarExpr x, _)}, _)) when Atom.is_builtin x && Atom.hint x = token -> true
            | _ -> false
        in

        let stmt_exclude = function
        | LetStmt (_, _, {value=(CallExpr ({value=(VarExpr x, _)}, _),_)}) as stmt  when Atom.is_builtin x && Atom.hint x = token ->
            logger#warning ">>>> to_X_form -> detect %s" token;
            true 
        | _ -> false 
        in

        let recv_rewriter parent_opt mt_e = function
            | (CallExpr ({value=(VarExpr x, _)}, _) as e) when Atom.is_builtin x && Atom.hint x = token ->
                logger#warning ">>>> extract_%s -> detect %s %s" token token (Error.show place);
                assert(mt_e.value <> EmptyMainType);
                let tmp = Atom.fresh (Printf.sprintf "tmp_%s" token) in
                let recv = auto_place (e, mt_e) in 

                let let_stmt =  auto_fplace (LetStmt (
                        mt_e,
                        tmp, 
                        recv)
                    ) in
                assert(stmt_exclude let_stmt.value);

                [ let_stmt ], (VarExpr tmp, mt_e )
        in

        rewrite_exprstmts_stmt ~recurse:true None stmt_exclude recv_selector recv_rewriter {place; value=stmt}


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