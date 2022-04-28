module Make(Arg:sig 
    val logger: Easy_logging.Logging.logger
    val name : string
    val build_dir : Fpath.t 
    val externals_location : string
end) = struct
    include Arg

    let auto_externals_dir = function
    | None -> List.fold_left Fpath.add_seg (Fpath.v externals_location) [name; "auto"]
    | Some root -> (* Load project specialized externals *)
        List.fold_left Fpath.add_seg root ["externals"; name; "auto"]

    let custom_externals_dir = function
    | None -> List.fold_left Fpath.add_seg (Fpath.v externals_location) [name; "custom"]
    | Some root -> (* Load project specialized externals *)
        List.fold_left Fpath.add_seg root ["externals"; name; "custom"]


    let preprocess_auto_external root external0 : (Fpath.t * Fpath.t) = 
        let destfile = match Fpath.relativize (auto_externals_dir root) external0 with
        | Some destfile -> destfile
        | None -> external0 (* already relative to auto_external_dirs*) 
        in
        let destfile = Fpath.append build_dir  destfile in

        (external0, destfile)

    let preprocess_custom_external root (external0, destfile) : (Fpath.t * Fpath.t) =
        let external0 = Fpath.append (custom_externals_dir root) external0 in
        let destfile = Fpath.append build_dir  destfile in
        (external0, destfile)

    let filter_custom root path =
        match Bos.OS.Path.exists path with
        | Rresult.Ok true -> true
        | Rresult.Ok false when root <> None -> false
        | Rresult.Ok false -> logger#error "plugin %s requires that %s exists" name (Fpath.to_string path); exit 1(* TODO handle it with a custom exception ?? *) 
        | Rresult.Error _ -> failwith "some error occurs when checking external/template existence"

    let process_externals root custom_external_rules = 
        (* Since dune-site provide links and not files we need to manualy copy the content, we can not rely only on [FileUtil.cp]*)
        let rec copy_external (src, dst) : unit = 
            let src = FileUtil.readlink (Fpath.to_string src) in 
            match (FileUtil.stat src).kind with
            | FileUtil.File -> FileUtil.cp ~recurse:true [src] (Fpath.to_string dst)
            | FileUtil.Dir -> begin
                FileUtil.mkdir (Fpath.to_string (snd(preprocess_auto_external root (Fpath.v src))));
                FileUtil.ls src 
                |> List.map Fpath.v
                |> List.map (preprocess_auto_external root)
                |> List.iter copy_external;
            end
            | _ -> Core.Error.error "Externals location [%s] is neither a file nor a directory !!" src
        in
        (* auto_externals *)
        let _auto_externals_dir = auto_externals_dir root in
        begin
            match Bos.OS.Dir.exists _auto_externals_dir with 
            | Rresult.Ok true ->  copy_external (preprocess_auto_external root _auto_externals_dir)
            | Rresult.Ok false -> ()
            | Rresult.Error _ -> failwith "error filesystem TODO"
        end;

        (* custom_externals *)
        custom_external_rules
        |> List.map (preprocess_custom_external root)
        |> List.filter (function (path,_) -> filter_custom root path) 
        |> List.iter copy_external
end