open Core 
open Utils
open AstUtils

open Easy_logging

(* Export AST *)
module Ast = Ast 

module Make () = struct 
    let logger = Core.Utils.make_log_of "Frontend" 

    module CookTarget = CookTarget.Make()
    module CookImpl = Ast2ImplCompilationPass.Make(CookImpl.Make())
    module PairedAnnotation = PairedAnnotation.Make()
    let process_target ir (filename:string) =
    filename
    |> ParseTarget.parse_targets
    |> function x-> logger#sinfo "Target file has been parsed"; x
    |> dump_selected "RawTarget" "RawTarget" RawTarget.show_targets RawTarget.targets_to_yojson
    |> CookTarget.cook_targets ir  
    |> function x-> logger#sinfo "Targets has been cooked"; x
    |> dump_selected "Target" "Target" Target.show_targets Target.targets_to_yojson 

    let to_ast places filename = 
        filename
        |> Parse.read
        |> function ast -> logger#sinfo "Main spec file has been read"; ast 
        |> function ast -> logger#sinfo "AST is built"; ast 
        |> dump_selected "Ast" "Ast" Ast.show_program Ast.program_to_yojson

    module Resolve = AstCompilationPass.Make(Resolve)
    module Reduce = AstCompilationPass.Make(Reduce)

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
        |> dump_selected "PairedAnnotationAst" "PairedAnnotationAst" Ast.show_program Ast.program_to_yojson 
        |> Reduce.apply
        |> function program -> let ir = Cook.apply program in Cookk.gamma, Cookk.gamma_types, Cookk.sealed_envs, ir

    let process_place (filename:string) =
        filename
        |> ParsePlace.parse_vplaces
        |> function x-> logger#sinfo "Place file has been parsed";x
        |> dump_selected "RawPlace" "RawPlace" Ast.show_vplaces Ast.vplaces_to_yojson
        |> CookPlace.cook_vplaces   
        |> function x-> logger#sinfo "PlaceAST has been coocked";x
        |> dump_selected "Place" "Place" IR.show_vplaces IR.vplaces_to_yojson 

    let to_impl gamma gamma_types sealed_envs eliminline_env targets filenames program = 
        let module PairedImpl = PairedImpl.Make(struct 
            let sealed_envs = sealed_envs 
            let gamma = gamma
            let gamma_types = gamma_types
            let eliminline_env = eliminline_env
        end) in 

        (* Semantics: 
            f is defined in both a.vimpl and b.vimpl
            and ```vardac ... --impl a.vimpl --impl b.vimpl```
            then the definition of b.vimpl (the last of the list is used)
            This behaviour is controlled by List.rev filenames

            headers and dependencies are merged
        *)

        (List.flatten(List.map ParseImpl.read (List.rev filenames))) 
        |> function ast -> logger#sinfo "Main impl file has been read"; ast 
        |> dump_selected "ParseImpl" "ParseImpl" Ast_impl.show_program Ast_impl.program_to_yojson
        |> CookImpl.apply
        |> function ast -> logger#sinfo "Impl AST is built"; ast 
        |> dump_selected "Impl" "Impl" Impl.show_program Impl.program_to_yojson
        |> PairedImpl.paired_program targets program
        |> function (headers, program) -> headers, (dump_selected "IRI" "IRI - IR-with-implemented" IRI.show_program IRI.program_to_yojson program)

    let unittests = Test.unittests 
end