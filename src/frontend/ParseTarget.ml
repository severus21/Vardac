open Core
open RawTarget

open Easy_logging

let logger = Logging.make_logger "_1_ compspec.codegen" Debug [];;

let read_targets (filename_target:string) =
    if Sys.file_exists filename_target then 
       Rresult.R.get_ok (Yaml_unix.of_file Fpath.(v filename_target))
    else(
      logger#error "Targets file : %s does not exists.\n" filename_target;
       exit 1)

let tableof t = 
    let table = Hashtbl.create (List.length t) in
    List.iter (function (k,v) -> Hashtbl.add table k v) t;
    table

let prepare values  = 
    List.flatten (List.map (function |`O xs -> xs | _-> failwith "TODO") values)

(* TODO refactor code*)
let dedup_targets = Hashtbl.create 16

let rec _parse_targets filename current_target (v : Yaml.value) : target list =
    let mock_place : Error.place = Error.forge_place filename 0 0 in
    match v with 
    |`A t -> List.flatten (List.map (_parse_targets filename current_target) t)
    |`O body -> begin
        let table = tableof body in

        let name = 
            try 
            begin
                match Hashtbl.find table "target" with 
                | `String x -> x 
                | _ -> Error.perror mock_place "Illformed target\n" 
            end
            with  Not_found -> Error.perror mock_place "target has no attribut [target]"
            in
       
        (* Needed when doing creating jar target for Akka *)
        if String.length name < 2 then Error.perror mock_place "target name [%s] must have a length greater (or equal) than 2" name;

        (* Check target names are defined exaclty once *)
        begin 
            match Hashtbl.find_opt dedup_targets name with 
            | None -> Hashtbl.add dedup_targets name ()
            | Some _ -> Error.perror mock_place "target [%s] is defined multiple times" name;
        end;

        let codegen = match Hashtbl.find_opt table "codegen" with 
            | None -> Error.perror mock_place "Codegen attribut must be declared for target [%s]\n" current_target
            | Some `O body-> ( 
            let table = tableof body in

            let language : string = 
                try
                begin
                    match Hashtbl.find table "language" with 
                    | `String x -> x 
                    | _ -> Error.perror mock_place "Syntax error in [language] definition of target [%s]\n" name 
                end
                with Not_found -> Error.perror mock_place "Target [%s], codegen.language is missing" name
            in

            let runtime : string = 
                try
                begin
                    match Hashtbl.find table "runtime" with
                    | `String x -> x 
                    | _ -> Error.perror mock_place "Syntax error in [runtime] definition of target [%s]\n" name 
                end
                with Not_found ->  Error.perror mock_place "Target [%s], codegen.runtime is missing" name
            in 

            let interface : string = 
                try
                begin
                    match Hashtbl.find table "interface" with
                    | `String x -> x 
                    | _ -> Error.perror mock_place "Syntax error in [interface] definition of target [%s]\n" name 
                end
                with Not_found ->  Error.perror mock_place "Target [%s], interface is missing" name
            in 

            let seen_no_main = ref [] in 
            let seen_laststage = ref [] in 
            let mains = 
                try Hashtbl.find table "mains" 
                with Not_found -> Error.perror mock_place "Target [%s], mains is missing" name
            in

            let mains : RawTarget.maindef list = 
                match mains with
                |`O mains_body -> begin
                    let mains_table = tableof mains_body in

                    (* Check main names are defined exaclty once *)
                    let dedup_mains = Hashtbl.create (Hashtbl.length mains_table) in
                    Hashtbl.iter (fun k _ ->
                        match Hashtbl.find_opt dedup_mains k with 
                        | None -> Hashtbl.add dedup_mains k ()
                        | Some _ -> Error.perror mock_place "main [%s] is defined multiple times inside target [%s]" k name;
                    ) mains_table;
                    logger#info "%d mains have been collected for target %s" (Hashtbl.length mains_table) name;



                    let build_main main_name main_body build_mains = 
                        match main_body with
                        |`O _body -> begin 
                            let _table = tableof _body in

                            let bootstrap : string = try begin
                                match Hashtbl.find _table "bootstrap" with
                                    |`String x -> x 
                                    |_ -> Error.perror mock_place "Syntax error in [bootstrap] definition of mains of target [%s]\n" name 
                            end with Not_found -> Error.perror mock_place "Target [%s] do not have [bootstrap] definition for main [%s]\n" name main_name 
                            in 

                            let entrypoint : string = try begin
                                match Hashtbl.find _table "entrypoint" with
                                    |`String x -> x 
                                    |_ -> Error.perror mock_place "Syntax error in [entrypoint] definition of mains of target [%s]\n" name 
                            end with Not_found -> Error.perror mock_place "Target [%s] do not have [entrypoint] definition for main [%s]\n" name main_name 
                            in 
                            
                            if entrypoint = "no_main" then seen_no_main := main_name :: !seen_no_main;
                            if bootstrap = "laststage" then seen_laststage := main_name :: !seen_laststage;

                            {RawTarget.name=main_name; bootstrap; entrypoint} :: build_mains
                        end
                        | _ -> Error.perror mock_place "Syntax error in [mains] definition of target [%s]\n" name 
                    in

                    Hashtbl.fold build_main mains_table [];
                end
                |_ -> Error.perror mock_place "Syntax error in [mains] definition of target [%s]\n" name 
            in 
            
            (* Check that no_main entrypoint is used at most once *)
            if List.length !seen_no_main > 1 then 
                Error.perror mock_place "magic entrypoint [no_main] has been used more than once inside target %s, culprits mains are:@ [%a]" name (Error.pp_list ", " (fun out x -> Format.fprintf out "%s" x)) !seen_no_main;
            
            (* Check that laststage boostrap is used at most once *)
            if List.length !seen_laststage > 1 then 
                Error.perror mock_place "magic bootstrap [laststage] has been used more than once inside target %s, culprits mains are:@ [%a]" name (Error.pp_list ", " (fun out x -> Format.fprintf out "%s" x)) !seen_laststage;

                {
                    RawTarget.language_plg=language;
                    runtime_plg=runtime;
                    interface_plg=interface;
                    mains=mains}  
            )
            |_ -> Error.perror mock_place "Ill-formed target [%s]\n" name                
        in
        [{  Core.AstUtils.place=mock_place; 
            Core.AstUtils.value=
                {name=name; codegen=codegen}
        }]
  end
  |_ -> Error.perror mock_place "Syntax error in targets definition of target [%s]\n" current_target                

let parse_targets filename : RawTarget.targets= 
    filename
    |> read_targets
    |> _parse_targets filename "top_level"
