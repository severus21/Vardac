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
let rec _parse_targets filename current_target (v : Yaml.value) : target list =
    let mock_place : Error.place = Error.forge_place filename 0 0 in
    match v with 
    |`A t -> List.flatten (List.map (_parse_targets filename current_target) t)
    |`O body -> begin
        let table = tableof body in

        let name = match Hashtbl.find table "target" with `String x -> x |_-> Error.error mock_place "Illformed target\n" in

        (* TODO capture Not_found exception when calling Hashtbl.find *)
        let codegen = match Hashtbl.find_opt table "codegen" with 
            | None -> Error.error mock_place "Codegen attribut must be declared for target [%s]\n" current_target
            | Some `O body-> ( 
            let table = tableof body in

            let language : string = match Hashtbl.find table "language" with 
                | `String x -> x 
                |_ -> Error.error mock_place "Syntax error in [language] definition of target [%s]\n" name 
            in

            let runtime : string = match Hashtbl.find table "runtime" with
                |`String x -> x 
                |_ -> Error.error mock_place "Syntax error in [runtime] definition of target [%s]\n" name 
            in 
            
            let mains : Core.Target.maindef list = match Hashtbl.find table "mains" with
                |`A ms -> begin 
                    let build_main = function
                    |`O _body -> begin 
                        let _table = tableof _body in

                        let name : string = match Hashtbl.find _table "name" with 
                            | `String x -> x 
                            |_ -> Error.error mock_place "Syntax error in [name] definition of mains of target [%s]\n" name 
                        in

                        let component : string = match Hashtbl.find _table "component" with
                            |`String x -> x 
                            |_ -> Error.error mock_place "Syntax error in [component] definition of mains of target [%s]\n" name 
                        in 

                        {Core.Target.name=name; component}  
                    end
                    |_ -> Error.error mock_place "Syntax error in [mains] definition of target [%s]\n" name 
                    in
                    List.map build_main ms 
                end
                |_ -> Error.error mock_place "Syntax error in [mains] definition of target [%s]\n" name 
            in 

                {Core.Target.language_plg=language; runtime_plg=runtime; mains=mains}  
            )
            |_ -> Error.error mock_place "Ill-formed target [%s]\n" name                
        in
        [{  Core.AstUtils.place=mock_place; 
            Core.AstUtils.value=
                {name=name; codegen=codegen}
        }]
  end
  |_ -> Error.error mock_place "Syntax error in targets definition of target [%s]\n" current_target                

let parse_targets filename : RawTarget.targets= 
    filename
    |> read_targets
    |> _parse_targets filename "top_level"
