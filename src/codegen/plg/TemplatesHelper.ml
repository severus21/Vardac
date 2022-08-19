open Core
open Jingoo


let default_jingoo_models = 
    [
    ("compiler_version", Jg_types.Tstr Config.version);
    ("compiler_debug", Jg_types.Tbool (Config.debug ()));
    ("compiler_keep_ghost", Jg_types.Tbool (Config.keep_ghost ()));
    ("project_name", Jg_types.Tstr (Config.project_name ()));
    ("author", Jg_types.Tstr (Config.author ()));
]


module Make(Arg:sig 
    val logger: Easy_logging.Logging.logger
    val name : string
    val build_dir : Fpath.t 
    val templates_location : string
    val custom_template_rules : (Fpath.t * (string * Jingoo.Jg_types.tvalue) list * Fpath.t) list
end) = struct
    include Arg

    let filter_custom root path =
        match Bos.OS.Path.exists path with
        | Rresult.Ok true -> true
        | Rresult.Ok false when root <> None -> false
        | Rresult.Ok false -> logger#error "plugin %s requires that %s exists" name (Fpath.to_string path); exit 1(* TODO handle it with a custom exception ?? *) 
        | Rresult.Error _ -> failwith "some error occurs when checking external/template existence"

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

        let models = default_jingoo_models @ env in

        let res = Jg_template.from_file (Fpath.to_string template) ~models:models in  
        let oc = open_out (Fpath.to_string destfile) in
        Printf.fprintf oc "%s" res; 
        close_out oc

    let preprocess_auto_template root jingoo_models template : (Fpath.t * (string * Jingoo.Jg_types.tvalue) list * Fpath.t) = 
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

        (template, jingoo_models, destfile)

    let preprocess_custom_template root (template, env, destfile) : (Fpath.t * (string * Jingoo.Jg_types.tvalue) list * Fpath.t) =
        let template = Fpath.append (custom_templates_dir root) template in
        let destfile = Fpath.append build_dir destfile in
        (template, env, destfile)

    (** @param root - empty for varda or projec_name directory *)
    let process_templates root jingoo_models = 
        (* auto_templates *)
        let _auto_templates_dir = auto_templates_dir root in


        (* Collect templates and create intermediate directories *)
        let rec explore path= 
            match Bos.OS.Dir.exists (Fpath.v path) with 
            | Rresult.Ok true ->
            begin
                (match Bos.OS.Dir.create (Fpath.v path) with
                | Rresult.Ok _ -> ()
                | Rresult.Error (`Msg msg) -> Error.error "Can not create templates directory in build dir\n\t%s" msg );

                List.flatten (List.map explore (FileUtil.ls path)) 
            end
            | Rresult.Ok false -> [path]
        in

        begin
            match Bos.OS.Dir.exists _auto_templates_dir with 
            | Rresult.Ok true -> begin 
                FileUtil.ls (Fpath.to_string _auto_templates_dir)
                |> function x -> List.flatten (List.map explore x)
                |> List.map Fpath.v
                |> List.map (preprocess_auto_template root jingoo_models)
                |> List.iter resolve_template;
            end
            | Rresult.Ok false -> ()
            | Rresult.Error _ -> failwith "error filesystem TODO"
        end;

        (* custom_templates *)
        custom_template_rules 
        |> List.map (preprocess_custom_template root) 
        |> List.filter (function (path,_,_) -> filter_custom root path) 
        |> List.iter resolve_template

end