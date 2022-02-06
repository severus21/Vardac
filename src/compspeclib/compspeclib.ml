(* -------------------------------------------------------------------------- *)
open Core
open Utils
open Codegen
open Easy_logging

let logger = Logging.make_logger "_1_ compspec" Debug [];;

let process_impl filename =
    Frontend.to_impl filename

(* Static passes *)
module Clean = IRCompilationPass.Make(Core.Clean)
module Derive = IRCompilationPass.Make(Derive)
module Intercept = IRCompilationPass.Make(Intercept)
module PartialEval = IRCompilationPass.Make(Core.PartialEval)
module Reduce = IRCompilationPass.Make(Core.Reduce)
module TypeChecking = IRCompilationPass.Make(Core.TypeChecking)
module TypeInference1 = IRCompilationPass.Make(Core.TypeInference.Make())
module TypeInference2 = IRCompilationPass.Make(Core.TypeInference.Make())
module TypeInference3 = IRCompilationPass.Make(Core.TypeInference.Make())
module EventAutoBoxing = IRCompilationPass.Make(Core.EventAutoBoxing)

let process_check build_dir places_file filename = 
    let build_dir = Utils.refresh_or_create_build_dir build_dir in
    let project_dir = Fpath.parent (Fpath.v filename) in

    let places = Frontend.process_place places_file in

    let (gamma, ir) = Frontend.to_ir places filename in
    ir
    |> PartialEval.apply
    |> Check.check_program project_dir build_dir
    (* TODO incoporate type checking as check*)

let process_compile (build_dir: Fpath.t) places_file targets_file impl_filename filename = 
    (* Prepare dir *)
    let build_dir = Utils.refresh_or_create_build_dir build_dir in
    Printf.eprintf "Codegeneration directory is \"%s\":\n" (Fpath.to_string build_dir);
    let project_dir = Fpath.parent (Fpath.v filename) in


    let places = Frontend.process_place places_file in


    let (gamma, ir) = Frontend.to_ir places filename in
    let ir1 =
        ir
        |> Reduce.apply
        |> TypeInference1.apply
        (*|> TypeChecking.apply*)
        |> PartialEval.apply
    in

    (* extract targets definitions from file *)
    let targets = Frontend.process_target ir targets_file in

    let module RecvElimination = ((Core.RecvElimination.Make((struct let gamma = gamma let targets = targets end))):Core.RecvElimination.Sig) in
    let module RecvElimination = Core.IRCompilationPass.Make(RecvElimination) in

    let module ImplicitElimination = ((Core.ImplicitElimination.Make((struct let gamma = gamma let targets = targets end))):Core.ImplicitElimination.Sig) in
    let module ImplicitElimination = Core.IRCompilationPass.Make(ImplicitElimination) in

    let ir2 = ir1 
        |> ImplicitElimination.apply
    in

    let ir3 = ir2
        |> PartialEval.apply
        |> Derive.apply

        (* TODO FIXME 
            0. Annots derived expression with types (and not just EmptyMainType)
            1. Check that derivation to not introduced bugs that can be detected by type-checking
        |> TypeInference2.apply
        |> TypeChecking.apply
        *)

        (* Clean derived code *)
        |> Core.PartialEval.peval_program
        |> ImplicitElimination.apply
        
        |> RecvElimination.apply (* Transform receive to async + ports *) 

        (* Every pass that change ports and components should be performed before runngin the Intercept transformation *)
        |> Intercept.apply
        |> TypeInference3.apply (*Needed since we introduce new constructions *)
        (*|> TypeChecking.apply*)
        |> PartialEval.apply
        |> RecvElimination.apply (* Intercept introduce recv for onboarding *) 
        |> Clean.apply


        |> EventAutoBoxing.apply
    in

    ir3
    |> Frontend.to_impl targets impl_filename  
    |> Codegen.codegen project_dir build_dir places targets;

    (* Before rewriting *)
    let module TopologyPrinter = ((Core.Topology.Make((struct let component2target = (Codegen.make_component2target ()) end))):Core.Topology.Sig) in (* Warning make_component2target can not be called before spliting until split.ml was improved*)
    TopologyPrinter.generate_static_logical_topology build_dir ir1;
    ()

let process_stats places_file targets_file impl_filename filename =
    let places = Frontend.process_place places_file in

    let ir = Frontend.to_ast places filename in

    ir
    |> Statistic.analyze_program