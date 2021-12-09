open Core 
open Utils
open AstUtils

open Easy_logging
let logger = Logging.make_logger "_1_ compspec.frontend" Debug [];;

(* Export AST *)
module Ast = Ast 

let process_target ir (filename:string) =
  filename
  |> ParseTarget.parse_targets
  |> function x-> logger#sinfo "Target file has been parsed"; x
  |> dump "RawTarget" RawTarget.show_targets
  |> CookTarget.cook_targets ir  
  |> function x-> logger#sinfo "Targets has been cooked"; x
  |> dump "Target" Target.show_targets 

let to_ast places filename = 
    filename
    |> Parse.read
    |> function ast -> logger#sinfo "Main spec file has been read"; ast 
    |> function ast -> logger#sinfo "AST is built"; ast 
    |> dump "Ast" Ast.show_program

let to_ir places filename =
    to_ast places filename
    |> Resolve.resolve_program   
    |> function ast -> logger#sinfo "AST is resolved"; ast 
    |> dump "ResolveAst" Ast.show_program  
    |> PairedAnnotation.apair_program 
    |> dump "PairedAnnotationAst" Ast.show_program  
    |> Cook.cook_program places
    |> function (gamma, ast) -> logger#sinfo "AST is cooked, IR has been generated"; (gamma, ast) 
    |> function (gamma, ast) -> dump ~print:(Config.debug_cook ()) "IR" IR.show_program ast; (gamma, ast)

let process_place (filename:string) =
    filename
    |> ParsePlace.parse_vplaces
    |> function x-> logger#sinfo "Place file has been parsed";x
    |> dump "RawPlace" Ast.show_vplaces  
    |> CookPlace.cook_vplaces   
    |> function x-> logger#sinfo "PlaceAST has been coocked";x
    |> dump "Place" IR.show_vplaces  

let to_impl targets filename program = 
    filename
    |> ParseImpl.read
    |> function ast -> logger#sinfo "Main impl file has been read"; ast 
    |> CookImpl.cook_program
    |> function ast -> logger#sinfo "Impl AST is built"; ast 
    |> dump "Impl" Impl.show_program
    |> PairedImpl.paired_program targets program
    |> function ast -> logger#sinfo "AST_impl is cooked, Impl has been generated"; ast 
    |> dump "IRI - IR-with-implemented" IRI.show_program