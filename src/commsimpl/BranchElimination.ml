open Core
open IR
open Easy_logging
open Utils
open AstUtils
open IRMisc
 

let fplace = (Error.forge_place "BranchElimination" 0 0) 
let auto_fplace smth = {place = fplace; value=smth}
include AstUtils2.Mtype.Make(struct let fplace = fplace end)

let branch_selector = function 
    | BranchStmt _ -> true
    | _ -> false

let branch_collector msg parent_opt place _ = 
    let parent = match parent_opt with | None -> "Toplevel" | Some p -> Atom.to_string p in
    Error.perror place "%s. Parent = %s" msg parent

(***************************************************)

module type Sig = sig
    include IRCompilationPass.Pass
end

module Make () : Sig = struct
    let logger = make_log_of "BanchElimination"
    (***************************************************)

    let elim_branch mt_st e_local_label e_local_s {branch_label; branch_s; body} = 
        let st_branch = st_branch_of mt_st branch_label in

        (* if label == branch_label => then use this branch *)
        IfStmt(
            e2_e (BinopExpr(e_local_label, Equal, e2lit branch_label)),
            auto_fplace (BlockStmt ([

                (* localy set to the branch *)
                (* s_branch = select(local_s, branch_label); *)
                auto_fplace(LetStmt(
                    mtype_of_st st_branch.value,
                    branch_s, (* Branch introduce a binder *)
                    e2_e ( UnopExpr(
                        UnpackOrPropagateResult,
                        e2_e (CallExpr(
                            e2var (Atom.builtin "select"),
                            [ 
                                e_local_s;
                                e_local_label 
                            ]
                        ))
                    ))
                ));
            ] @ [ body ])
            ),
            None
        )


    let rewritor place = function 
    | BranchStmt {s; label; branches} -> begin
        let mt_st = snd s.value in

        let local_res = Atom.fresh "res" in
        let mt_local_res = 
            mtype_of_ct (TTuple [
                mtype_of_ft TBLabel;
                mt_st 
            ]) 
        in
        let e_local_res = auto_fplace (VarExpr local_res, mt_local_res) in
        let local_label = Atom.fresh "label" in 
        let mt_local_label = mtype_of_ft TBLabel in
        let e_local_label = auto_fplace (VarExpr local_label, mt_local_label) in

        let local_s = Atom.fresh "s" in 
        let e_local_s = auto_fplace (VarExpr local_s, mt_st) in

        (*** Headers ***)
        [
            (* tuple<blabel, ...> tmp = receive(s); *)
            LetStmt(
                mt_local_res,
                local_res,
                {place = place @fplace; value=(CallExpr(
                    e2var (Atom.builtin "receive"),
                    [ s ]
                )), auto_fplace EmptyMainType}
            );
            (* blabel label = tmp._0); *)
            LetStmt(
                mt_local_label,
                local_label,
                e2_e (AccessExpr(
                    e_local_res,
                    e2var (Atom.builtin "_0")
                ))
            );
            (* ... local_s = tmp._1; *)
            LetStmt(
                mt_st,
                local_s,
                e2_e (AccessExpr(
                    e_local_res, 
                    e2var (Atom.builtin "_1")
                ))
            );
        ]
        (*** Compile away each branch ***)
        @ (List.map (elim_branch mt_st e_local_label e_local_s) branches) 
    end
    | _ -> raise (Error.DeadbranchError "selector prevents accessing this branch")

    (*****************************************************)
    let name = "Commsimpl.BranchElimination"
    let displayed_pass_shortdescription = "branch has been eliminated from IR"
    let displayed_ast_name = "IR branchelim"
    let show_ast = true
    let global_at_most_once_apply = false


    let precondition program = program
    let postcondition program = 
        (* Check: no more branch *)
        ignore (collect_stmt_program branch_selector (branch_collector "branch() remains in IR after Rewriting") program);
        program

    let apply_program =
        rewrite_stmt_program true branch_selector rewritor
end