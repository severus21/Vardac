(**
    Annotate and transformate AST to add dynamic reflexivity information
    - access list of ports
    - ??
    - TODO maybe add here intermediate_states = { ... }

    Warning:
        Should be one of the last Varda transformation pass, in order not to miss anything
*)

open Core
open AstUtils
open IR
 
open Easy_logging

let logger = Logging.make_logger ("_1_ vardac.InlineElim") Debug [];;

let fplace = (Error.forge_place "InlineElim" 0 0) 
let auto_fplace smth = {place = fplace; value=smth}
include AstUtils2.Mtype.Make(struct let fplace = fplace end)

module Make () = struct
    (* B inlineable in A => "B->Set{A::..}"*)
    let is_inlineable_in = Hashtbl.create 256

    let rec extract_ainline = function
        | [] -> []
        | InlinableIn schemas::xs -> schemas@(extract_ainline xs)

    let select_component_with_inlinable = function
        | Component {value=ComponentStructure cstruct} -> 
            extract_ainline cstruct.annotations <> []
        | _ -> false
    let select_spawn_with_inline_in = function
        | Spawn {inline_in = Some _} -> true
        | _ -> false

    let eliminate_static_inlinable program = 
        let rewriter place = function
            | Component {place; value=ComponentStructure cstruct} -> 
                logger#info "static inline %s in " (Atom.to_string cstruct.name);
                let schemas = extract_ainline cstruct.annotations in

                (* register in is_inlineable_in *)
                begin
                    let schemas_in = Atom.Set.of_seq (List.to_seq schemas) in
                    match Hashtbl.find_opt is_inlineable_in cstruct.name with
                        | None -> Hashtbl.add is_inlineable_in cstruct.name schemas_in
                        | Some xs -> Hashtbl.add is_inlineable_in cstruct.name (Atom.Set.union schemas_in xs)
                end;

                (* derive a "class" [InlineB15A] *)
                failwith "TODO update atom in body to preserve atom unicity";
                let cl = Class {
                    annotations = cstruct.annotations;
                    name = cstruct.name;
                    body = List.flatten (List.map (map_places(map_plgannots(function place -> function 
                        | Contract item -> [CLContract item]
                        | Method item -> [CLMethod item] 
                        | State item -> [CLState item]
                        | Term _ -> Error.perror place "component with inner term can not be inlined yet!"
                        | _ -> [] 
                    ))) cstruct.body);
                }
                in
                
                (* add headers to A *)

                (* add method [spawn_B15] in A *)

                (* add state [instances_B15] in A *)

                (* add inports [p_B15_pb15name] in A + routing to the corresponding instance in [instances_B15] *)
                
                (* add outports [p_B15_pb15name] in A *)

                (* add eports [p_B15_pb15name] in A + routing to **all** instances in [instances_B15]*)

                []
        in

        rewrite_term_program select_component_with_inlinable rewriter program
    let eliminate_dynamic_inline_in program = 

        let rewriter mt = function
            | Spawn {inline_in = Some {place; value=e, {value=CType{value=TActivationRef{value=CompType {value=CompTUid schema_in}}}}}} -> begin 
                let schema = match mt with
                    |{value=CType{value=TActivationRef{value=CompType {value=CompTUid schema}}}}
                    |{value=CType{value=TActivationRef{value=CompType {value=TStruct (schema,_)}}}}  -> schema
                    | _ -> Error.perror mt.place "spawn is ill-typed! %s" (show_main_type mt)
                in

                logger#debug "spawn inline %s in %s" (Atom.to_string schema) (Atom.to_string schema_in);


                (* That it is a valid inline according to static_elim *)
                begin
                    try 
                        let possible_schemas_in = Hashtbl.find is_inlineable_in schema in
                        ignore (Atom.Set.find schema_in possible_schemas_in)
                    with Not_found -> Error.error "Can not inline %s in %s ! Use @inline_in annotations." (Atom.to_string schema) (Atom.to_string schema_in)
                end;

                failwith "TODO"
            end
            | Spawn {inline_in = Some {place; value=_, mtt} } -> Error.perror place "inline_in is ill-typed! %s" (show_main_type mtt) 
        in

        rewrite_expr_program select_spawn_with_inline_in rewriter program

    let rewrite_program program =  
        program
        |> eliminate_static_inlinable
        |> eliminate_dynamic_inline_in
        |> failwith "TODO inline"
        
    (*********************************************************)
    let name = "InlineElim"
    let displayed_pass_shortdescription = "inlining(s) have been eliminated"
    let displayed_ast_name = "IR without inline"
    let show_ast = true 
    let global_at_most_once_apply = false 

    let precondition program = program

    let postcondition program = 
        let error_expr_collector msg parent_opt env e = 
            let parent = match parent_opt with | None -> "Toplevel" | Some p -> Atom.to_string p in
            Error.perror e.place "%s. Parent = %s" msg parent
        in
        let error_term_collector msg parents = 
            let parent = match parents with 
                | [] -> "Toplevel" 
                | xs ->
                    Format.fprintf Format.str_formatter "%a" (Error.pp_list "::" (fun out x -> Format.fprintf out "%s" (Atom.to_string x)));
                    Format.flush_str_formatter ()
            in
            Error.error "%s. Parent = %s" msg parent
        in
        (* Check: no more spawn with inline_in  *)
        ignore (collect_expr_program Atom.Set.empty select_spawn_with_inline_in (error_expr_collector "Spawn with inline_in remains in IR after InlineElim") program);
        (* Check: no more inlinable_in *)
        ignore (collect_term_program true select_component_with_inlinable (error_term_collector "Spawn with inlinable_in remains in IR after InlineElim") program);
        program 

    let apply_program = rewrite_program
end