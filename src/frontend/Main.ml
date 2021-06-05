open Core 
open Utils
open AstUtils

open Easy_logging

let logger = Logging.make_logger "_1_ compspec.frontend" Debug [];;

let to_ir places filename =
    filename
    |> Parse.read
    |> function ast -> logger#sinfo "Main spec file has been read"; ast 
    |> function ast -> logger#sinfo "AST is built"; ast 
    |> dump "Ast" Ast.show_program
    |> Resolve.resolve_program   
    |> function ast -> logger#sinfo "AST is resolved"; ast 
    |> dump "ResolveAst" Ast.show_program  
    |> Cook.cook_program places
    |> function ast -> logger#sinfo "AST is cooked, IR has been generated"; ast 
    |> dump ~print:(Config.debug_cook ()) "IR" IR.show_program

let process_place (filename:string) =
    filename
    |> ParsePlace.parse_vplaces
    |> function x-> logger#sinfo "Place file has been parsed";x
    |> dump "RawPlace" Ast.show_vplaces  
    |> CookPlace.cook_vplaces   
    |> function x-> logger#sinfo "PlaceAST has been coocked";x
    |> dump "Place" IR.show_vplaces  