open Core
open Ast

let read_vplaces (filename_place:string) =
    if Sys.file_exists filename_place then 
       Rresult.R.get_ok (Yaml_unix.of_file Fpath.(v filename_place))
    else(
       Printf.fprintf stderr "Places file : %s does not exists.\n" filename_place;
       exit 1)

let tableof t = 
    let table = Hashtbl.create (List.length t) in
    List.iter (function (k,v) -> Hashtbl.add table k v) t;
    table

let prepare values  = 
    List.flatten (List.map (function |`O xs -> xs | _-> failwith "TODO") values)

let parse_nbr_instances filename mock_place name str = 
match Parse.parse filename str with
| [ term ] -> begin
    match term.value with 
    | Ast.Stmt stmt -> begin
        match stmt.value with 
        | Ast.ExpressionStmt e -> e
        | _ -> Error.error mock_place "Syntax error in [nbr_instances] of place [%s]: it must be a valid expression!!\n" name
    end
        | _ -> Error.error mock_place "Syntax error in [nbr_instances] of place [%s]: it must be a valid expression!!\n" name
end
| _ -> Error.error mock_place "Syntax error in [nbr_instances] of place [%s]: it must be a valid expression!!\n" name

let parse_features mock_place name table = 
    let features = Hashtbl.create 10 in
    match Hashtbl.find_opt table "features" with 
    | None -> features 
    | Some `O body -> 
        let table = tableof body in

        Hashtbl.iter (fun k -> function 
        | `Bool b -> Hashtbl.add features k (string_of_bool b)
        | `Float f -> Hashtbl.add features k (string_of_float f)
        | `String s ->  Hashtbl.add features k s
        | `A _ | `Null | `O _  -> Error.error mock_place "Syntax error in [features] definition of place [%s]\n" name 
        ) table; 
        features
    | _ -> Error.error mock_place "Syntax error in [features] definition of place [%s]\n" name 

let rec _parse_vplaces filename current_place (v : Yaml.value) : Ast.vplace list =
let mock_place : Error.place = Error.forge_place filename 0 0 in
match v with 
|`A t -> List.flatten (List.map (_parse_vplaces filename current_place) t)
|`O body ->begin
    let table = tableof body in

    let name = match Hashtbl.find table "place" with `String x -> x |_-> Error.error mock_place "Illformed place\n" in

    let nbr_instances : Ast.expr = 
        match Hashtbl.find_opt table "nbr_instances" with 

        | None -> parse_nbr_instances filename mock_place name "1;"
        | Some `String x -> parse_nbr_instances filename mock_place name (x^";")
        |_-> Error.error mock_place "Illformed place\n" 
    in

    let features =  parse_features mock_place name table in 

    let children = match Hashtbl.find_opt table "children" with 
        | None -> [] 
        | Some `A children ->  List.map (_parse_vplaces filename name) children  
        |_ -> Error.error mock_place "Syntax error in [children] definition of place [%s]\n" name                
    in  

    [{  name; nbr_instances; features; children=List.flatten children}
    ]
end
|_ -> Error.error mock_place "Syntax error in places definition of place [%s]\n" current_place                

let parse_vplaces filename =
    filename
    |> read_vplaces
    |> _parse_vplaces filename "top_level"

