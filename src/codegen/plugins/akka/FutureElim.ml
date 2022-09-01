(*
    Can not use future.get(timeout) in Akka block the threat and the other async methods ...
*)
open Core
open Utils
open AstUtils
open Easy_logging
open Fieldslib
open Misc
open IRI

module Make () = struct

    let pass_name = "Akka.FutureElim"
    let logger = make_log_of pass_name 

    let fplace = (Error.forge_place pass_name 0 0) 
    let auto_fplace smth = {place = fplace; value=smth}

    include AstUtils2.Mtype.Make(struct let fplace = fplace end)

    (* The source calculus. *)
    module S = IRI 
    (* The target calculus. *)
    module T = IRI 


    let resolved_result_dispatchers = Hashtbl.create 16
    let intermediate_futures_tbl = Hashtbl.create 16


    (*
        to_X_form + split
        stmts A
        wait_future()
        stmts B
        for wait_future and for function calling function with wait_future inside ....
    *)

    let selected = ref 0 
    let select_new_future = function
    | LetStmt (_, _, {value=CallExpr ({value=VarExpr x,_}, []), _}) ->
        selected := 2;
        Atom.is_builtin x && Atom.hint x = "future"
    | _ when !selected > 0 -> 
        decr selected;
        true
    | _ -> false

    let rewrite_new_future parent_opt place = function
        (*
            LetStmt(..., f, future());
            add2dict(..., ..., f);
            return f;

            becomes [ return new_id ]


        *)
    | ReturnStmt _ -> 
        [ ReturnStmt (e2_lit (IntLit (Atom.identity (Atom.fresh "continuation_id")))) ]
    | _ -> [ EmptyStmt ]


    let select_complete_future= function
    | CallExpr ({value=VarExpr x, _}, _) -> Atom.is_builtin x && Atom.hint x = "complete_future"
    | _ -> false

    let rewrite_complete_future  parent_opt mt = function
        (*
            complete_future(get2dict(..., id), value)

            becomes ! ResolvedResult(id, value)
        *)
    | CallExpr (_, [{value=CallExpr (_, [this_intermediate_futures; id]), _}; value]) ->
        CallExpr(
            e2_e( RawExpr "this.tell"),
            [
                e2_e(CallExpr(
                    e2_e(RawExpr "new ResolvedResult"),
                    [id; value]
                ))

            ] 
        )


    let select_wait_future = function
    | CallExpr ({value=VarExpr f, _}, _) ->  Atom.hint f = "wait_future" && Atom.is_builtin f
    | _ -> false

    let rec rewrite_wait_future parent acc_stmts = function
        (*
            ret = wait_future(f()?, timeout)  
            stmts

            becomes

            add2dict(this.futures, s_id, ret -> { stmts })

            where ret is now the continuation_id
        *)
    | {value=LetStmt(_, _, ({value=CallExpr ({value=VarExpr x, _}, _), _} as continuation_id))}::stmts when Atom.is_builtin x && Atom.hint x = "wait_future" ->

        let intermediate_futures = Hashtbl.find intermediate_futures_tbl  parent in

        let continuation_name = Atom.fresh ((Atom.to_string parent)^"_future_continuation") in
        let continuation = {
            annotations = [];
            ghost = false;
            ret_type = mtype_of_ft TVoid ;
            name = continuation_name;
            args = []; (*TODO*)
            body = AbstractImpl stmts; (* TODO recursive call on this method until no wait_future *)
            contract_opt = None;
            on_destroy = false;
            on_startup = false;
        } in

        stmt2_e(
            CallExpr(
                e2var (Atom.builtin "add2dict"),
                [
                    e2_e (AccessExpr( e2_e This, e2var (intermediate_futures)));
                    continuation_id;
                    e2_e(AccessExpr(e2_e This, e2var continuation_name))
                ]
            )
        )
    | stmt::stmts -> 
        rewrite_wait_future parent (stmt::acc_stmts) stmts
    
    let rewrite_global_component parent_opt place (cstruct:component_structure) = 
        (* 
            onReceive(
                ResolvedResult.class,
                (id,ret) -> this.futures[id](ret)
            )
        *)
        let resolved_result_dispatcher = Atom.fresh "resolved_result_dispatcher" in
        Hashtbl.add resolved_result_dispatchers cstruct.name resolved_result_dispatcher;

        let intermediate_futures_set = 
        List.fold_left (fun acc -> function item ->
            let _, xs, _ = collect_expr_component_item 
                (Some cstruct.name)
                Atom.Set.empty
                (function |VarExpr x -> Atom.hint x = "intermediate_futures")
                (fun _ _ -> function |{value=VarExpr x, _} -> [x])
                item
            in
            Atom.Set.union acc (Atom.Set.of_list xs)
                
            )
            Atom.Set.empty
            cstruct.body
        in
        assert(Atom.Set.cardinal intermediate_futures_set = 1);

        Hashtbl.add intermediate_futures_tbl cstruct.name (Atom.Set.min_elt intermediate_futures_set);

        failwith "TODO add it in receiver  next pass"

            

    (*****************************************************)
    let name = pass_name 
    let displayed_pass_shortdescription = Printf.sprintf "Codegen: Eliminate future" 
    let displayed_ast_name = "pre-Akka AST without futures"
    let show_ast = true
    let global_at_most_once_apply = true 

    let precondition (program:IRI.program) = 
        program

    let postcondition program = 
        program

    let apply_program program =
        program
        |> rewrite_component_program (function _ -> true) rewrite_global_component (* hydrate shared state *)
        |> rewrite_stmt_program true select_new_future rewrite_new_future 
        |> rewrite_expr_program select_complete_future rewrite_complete_future
        |> rewrite_expr_program select_wait_future rewrite_wait_future 
end