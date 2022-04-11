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

module Resolve = AstCompilationPass.Make(Resolve)

let to_ir places filename =
    let module Cookk = Cook.Make(struct 
        let _places = places 
        let gamma = Cook.empty_gamma ()
        let gamma_types = Cook.empty_gamma ()
    end) in 
    let module Cook = Ast2IRCompilationPass.Make(Cookk) in

    to_ast places filename
    |> Resolve.apply  
    |> PairedAnnotation.apair_program 
    |> dump "PairedAnnotationAst" Ast.show_program  
    |> function program -> let ir = Cook.apply program in Cookk.gamma, Cookk.gamma_types, Cookk.sealed_envs, ir

let process_place (filename:string) =
    filename
    |> ParsePlace.parse_vplaces
    |> function x-> logger#sinfo "Place file has been parsed";x
    |> dump "RawPlace" Ast.show_vplaces  
    |> CookPlace.cook_vplaces   
    |> function x-> logger#sinfo "PlaceAST has been coocked";x
    |> dump "Place" IR.show_vplaces  

let to_impl gamma gamma_types sealed_envs targets filename program = 
    let module PairedImpl = PairedImpl.Make(struct 
        let sealed_envs = sealed_envs 
        let gamma = gamma
        let gamma_types = gamma_types
    end) in 

    filename
    |> ParseImpl.read
    |> function ast -> logger#sinfo "Main impl file has been read"; ast 
    |> CookImpl.cook_program
    |> function ast -> logger#sinfo "Impl AST is built"; ast 
    |> dump "Impl" Impl.show_program
    |> PairedImpl.paired_program targets program
    |> function (headers, program) -> dump "IRI - IR-with-implemented" IRI.show_program program; (headers, program)