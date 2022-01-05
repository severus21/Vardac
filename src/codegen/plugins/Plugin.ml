open Core
open Core.Utils

open Akka
open Java
open Jingoo

(* The source calculus. *)
module S = IRI

(** Runtime plugin interface *)
module type S_Ast = sig
    type program
end;;

module type Rt_plg = sig
    (*val name: string ifwe need the name we will have to load it from module Desc.name*)
    module Ast : S_Ast

    module Finish: sig 
        type collected_state
        module Make : functor () -> sig
            val cstate : Finish.collected_state ref
            val finish_program : S.program -> Ast.program
        end
    end
end
module type Lg_plg = sig
    (*val name: string*)
    module Ast : S_Ast

    module Output: sig
        val output_program : string -> Fpath.t -> Ast.program -> unit 
    end
end

module type Cg_plg = sig
    val name: string
    val logger: Easy_logging.Logging.logger

    module Rt : Rt_plg
    module Lg : Lg_plg 

    val templates_location : string
    val externals_location : string

    (* TODO places could be just field of Plug in order not to have to proapgate it and to recompute it 
    
    Same for target

    with a Plug.Make(target)(places)
    
    *)
    module MakeRt2Lg : functor (Arg : sig
        val target:Core.Target.target 
        val cstate:Rt.Finish.collected_state
    end) -> sig 
        val cstate : Rt.Finish.collected_state ref
        val finish_program : Rt.Ast.program -> ((string * Fpath.t) * Lg.Ast.program) list 
    end

    type plgstate
    val plgstate: plgstate ref
    
    val finish_ir_program : Core.Target.target -> S.program -> ((string * Fpath.t) * Lg.Ast.program) list 
    val output_program : Core.Target.target -> Fpath.t -> S.program -> unit

    val custom_template_rules : Core.Target.target -> (Fpath.t * (string * Jingoo.Jg_types.tvalue) list * Fpath.t) list
    val custom_external_rules : unit -> (Fpath.t * Fpath.t) list
    val jingoo_env : Core.Target.target -> plgstate -> Core.IR.vplace list -> (string * Jingoo.Jg_types.tvalue) list
end

module type Plug = sig
    include Cg_plg

    val init_build_dir : Core.Target.target -> Fpath.t -> Fpath.t -> unit
    val resolve_templates : Core.IR.vplace list -> Core.Target.target -> Fpath.t -> Fpath.t -> unit
end

module Make (Plg: Cg_plg) = struct 
    (* Generic template handling *)
    include Plg

(*
    lg4dc/
    - externals
        - auto
        - custom
    - templates 
        - auto
        - custom

    project_name/ (optional)
    - externals 
    ...
    - templates
    ...
    
    precedence rules
    - project_name [externals/templates] overrides lg4dc
    - custom overides auto in project_name and lg4dc (independently)

*)
let filter_custom root path =
    match Bos.OS.Path.exists path with
    | Rresult.Ok true -> true
    | Rresult.Ok false when root <> None -> false
    | Rresult.Ok false -> logger#error "plugin %s requires that %s exists" name (Fpath.to_string path); exit 1(* TODO handle it with a custom exception ?? *) 
    | Rresult.Error _ -> failwith "some error occurs when checking external/template existence"

(******************************** External **********************************)
    let auto_externals_dir = function
    | None -> List.fold_left Fpath.add_seg (Fpath.v externals_location) [name; "auto"]
    | Some root -> (* Load project specialized externals *)
        List.fold_left Fpath.add_seg root ["externals"; name; "auto"]

    let custom_externals_dir = function
    | None -> List.fold_left Fpath.add_seg (Fpath.v externals_location) [name; "custom"]
    | Some root -> (* Load project specialized externals *)
        List.fold_left Fpath.add_seg root ["externals"; name; "custom"]


    let preprocess_auto_external root build_dir external0 : (Fpath.t * Fpath.t) = 
        let destfile = match Fpath.relativize (auto_externals_dir root) external0 with
        | Some destfile -> destfile
        | None -> external0 (* already relative to auto_external_dirs*) 
        in
        let destfile = Fpath.append build_dir  destfile in

        (external0, destfile)

    let preprocess_custom_external root (build_dir:Fpath.t) (external0, destfile) : (Fpath.t * Fpath.t) =
        let external0 = Fpath.append (custom_externals_dir root) external0 in
        let destfile = Fpath.append build_dir  destfile in
        (external0, destfile)
    let process_externals root build_dir = 
        (* Since dune-site provide links and not files we need to manualy copy the content, we can not rely only on [FileUtil.cp]*)
        let rec copy_external (src, dst) : unit = 
            let src = FileUtil.readlink (Fpath.to_string src) in 
            match (FileUtil.stat src).kind with
            | FileUtil.File -> FileUtil.cp ~recurse:true [src] (Fpath.to_string dst)
            | FileUtil.Dir -> begin
                FileUtil.mkdir (Fpath.to_string (snd(preprocess_auto_external root build_dir (Fpath.v src))));
                FileUtil.ls src 
                |> List.map Fpath.v
                |> List.map (preprocess_auto_external root build_dir)
                |> List.iter copy_external;
            end
        in
        (* auto_externals *)
        let _auto_externals_dir = auto_externals_dir root in
        begin
            match Bos.OS.Dir.exists _auto_externals_dir with 
            | Rresult.Ok true ->  copy_external (preprocess_auto_external root build_dir _auto_externals_dir)
            | Rresult.Ok false -> ()
            | Rresult.Error _ -> failwith "error filesystem TODO"
        end;

        (* custom_externals *)
        custom_external_rules ()
        |> List.map (preprocess_custom_external root build_dir)
        |> List.filter (function (path,_) -> filter_custom root path) 
        |> List.iter copy_external

(******************************** Templates **********************************)

    (* resolve and generate [auto] templates *)
    let auto_templates_dir = function
    | None -> List.fold_left Fpath.add_seg (Fpath.v templates_location) [name; "auto"]
    | Some root -> (* Load project specialized templates *)
        List.fold_left Fpath.add_seg root ["templates"; name; "auto"]
    let custom_templates_dir  = function
    | None -> List.fold_left Fpath.add_seg (Fpath.v templates_location) [name; "custom"]
    | Some root -> (* Load project specialized templates *)
        List.fold_left Fpath.add_seg root ["templates"; name; "custom"]

    let resolve_template (template, env, destfile) : unit =
        Utils.create_directory_hierarchy (Fpath.parent destfile);

        let res = Jg_template.from_file (Fpath.to_string template) ~models:env in  
        let oc = open_out (Fpath.to_string destfile) in
        Printf.fprintf oc "%s" res; 
        close_out oc

    let preprocess_auto_template places target root build_dir template : (Fpath.t * (string * Jingoo.Jg_types.tvalue) list * Fpath.t) = 
        let destfile = match Fpath.relativize (auto_templates_dir root) template with
        | Some destfile -> destfile
        | None -> template (* already relative to auto_template_dirs*) 
        in
        let destfile = (
            if Fpath.has_ext "j2" template then 
                Fpath.rem_ext destfile
            else
                destfile
        ) in
        let destfile = Fpath.append build_dir  destfile in

        (template, jingoo_env target !plgstate places, destfile)

    let preprocess_custom_template root build_dir (template, env, destfile) : (Fpath.t * (string * Jingoo.Jg_types.tvalue) list * Fpath.t) =
        let template = Fpath.append (custom_templates_dir root) template in
        let destfile = Fpath.append build_dir destfile in
        (template, env, destfile)

    (** @param root - empty for lg4dc or projec_name directory *)
    let process_templates places target root build_dir = 
        (* auto_templates *)
        let _auto_templates_dir = auto_templates_dir root in


        (* Collect templates and create intermediate directories *)
        let rec explore path= 
            match Bos.OS.Dir.exists (Fpath.v path) with 
            | Rresult.Ok true ->
                Bos.OS.Dir.create (Fpath.v path);
                List.flatten (List.map explore (FileUtil.ls path)) 
            | Rresult.Ok false -> [path]
        in

        begin
            match Bos.OS.Dir.exists _auto_templates_dir with 
            | Rresult.Ok true -> begin 
                FileUtil.ls (Fpath.to_string _auto_templates_dir)
                |> function x -> List.flatten (List.map explore x)
                |> List.map Fpath.v
                |> List.map (preprocess_auto_template places target root build_dir)
                |> List.iter resolve_template;
            end
            | Rresult.Ok false -> ()
            | Rresult.Error _ -> failwith "error filesystem TODO"
        end;

        (* custom_templates *)
        custom_template_rules target 
        |> List.map (preprocess_custom_template root build_dir)
        |> List.filter (function (path,_,_) -> filter_custom root path) 
        |> List.iter resolve_template

    let init_build_dir target (project_dir:Fpath.t) (build_dir: Fpath.t) : unit = 
        process_externals None build_dir; (* Plugin wide *)
        process_externals (Some project_dir) build_dir (* Project specific *)

    let resolve_templates places target (project_dir:Fpath.t) (build_dir: Fpath.t) : unit = 
        process_templates places target None build_dir; (* Plugin wide *)
        process_templates places target (Some project_dir) build_dir (* Project specific *)
end
