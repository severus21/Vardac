open Core 
open Utils
open AstUtils

open Easy_logging

let logger = Logging.make_logger "_1_ compspec.frontend" Debug [];;

let to_ir places filename =
    filename
    |> Parse.read
    |> function x-> logger#sinfo "Main spec file has been read"; x
    |> function x-> logger#sinfo "AST is built";x
    |> dump "Ast" Ast.show_program
    |> Resolve.resolve_program   
    |> function x-> logger#sinfo "AST is resolved";x
    |> dump "ResolveAst" Ast.show_program  
    |> Cook.cook_program places
    |> function x-> logger#sinfo "AST is cooked, IR has been generated";x
    |> dump "IR" IR.show_program 

let process_place (filename:string) =
    filename
    |> ParsePlace.parse_vplaces
    |> function x-> logger#sinfo "Place file has been parsed";x
    |> dump "RawPlace" Ast.show_vplaces  
    |> CookPlace.cook_vplaces   
    |> function x-> logger#sinfo "PlaceAST has been coocked";x
    |> dump "Place" IR.show_vplaces  