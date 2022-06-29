(* -------------------------------------------------------------------------- *)
open Core
open Utils
open Codegen
open Easy_logging
open Common

let logger = Logging.make_logger "_1_ vardac" Debug [];;

let process_impl filename =
    Frontend.to_impl filename

(* Static passes *)
module Clean = IRCompilationPass.Make(Common.Clean)
module Derive = IRCompilationPass.Make(Derive)
module Intercept = IRCompilationPass.Make(Intercept)
module PartialEval = IRCompilationPass.Make(Common.PartialEval)
module Reduce = IRCompilationPass.Make(Common.Reduce)
module Reflexivity = IRCompilationPass.Make(Common.Reflexivity)
module UntypedCleansing = IRCompilationPass.Make(Common.UntypedCleansing.Make())
module TypeChecking = IRCompilationPass.Make(Common.TypeChecking)
module TypeInference1 = IRCompilationPass.Make(Common.TypeInference.Make())
module TypeInference2 = IRCompilationPass.Make(Common.TypeInference.Make())
module TypeInference3 = IRCompilationPass.Make(Common.TypeInference.Make())
module TypeInference4 = IRCompilationPass.Make(Common.TypeInference.Make())
module TypeInference5 = IRCompilationPass.Make(Common.TypeInference.Make())
module EventAutoBoxing = IRCompilationPass.Make(Common.EventAutoBoxing.Make())
module ClassicalAutoBoxing = IRCompilationPass.Make(Common.ClassicalAutoBoxing.Make())
module InlineElim = IRCompilationPass.Make(Common.InlineElim.Make())

let process_check build_dir places_file filename = 
    Utils.refresh_or_create_dir build_dir;
    let project_dir = Fpath.parent (Fpath.v filename) in

    let places = Frontend.process_place places_file in

    let (_, _, _, ir) = Frontend.to_ir places filename in
    ir
    |> PartialEval.apply
    |> Check.check_program project_dir build_dir
    (* TODO incoporate type checking as check*)

let process_compile (build_dir: Fpath.t) places_file targets_file impl_filename filename = 
    (* Prepare dir *)
    Utils.refresh_or_create_dir build_dir;
    Printf.eprintf "Codegeneration directory is \"%s\":\n" (Fpath.to_string build_dir);
    let project_dir = Fpath.parent (Fpath.v filename) in


    let places = Frontend.process_place places_file in


    let (gamma, gamma_types, sealed_envs, ir) = Frontend.to_ir places filename in
    let ir1 =
        ir
        |> Reduce.apply
        |> UntypedCleansing.apply
        |> TypeInference1.apply
        (*|> TypeChecking.apply*)
        |> ClassicalAutoBoxing.apply
        |> PartialEval.apply
    in

    (* extract targets definitions from file *)
    let targets = Frontend.process_target ir targets_file in

    let module CommSimpl = Core.IRCompilationPass.Make(Commsimpl) in

    let module ImplicitElimination = ((Common.ImplicitElimination.Make((struct let gamma = gamma let targets = targets end))):Common.ImplicitElimination.Sig) in
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
        |> PartialEval.apply
        |> ImplicitElimination.apply

        (* TODO FIXME put before CommSimpl to avoid issue with intermediate_state added twice *)
        |> InlineElim.apply (* FIXME can not intercept A if B is inlined inside*)
        |> TypeInference3.apply (*Needed since we introduce new constructions *)

        |> CommSimpl.apply (* Transform receive to async + ports *) 

        (* Every pass that change ports and components should be performed before runngin the Intercept transformation *)
        |> Intercept.apply
        |> TypeInference4.apply (*Needed since we introduce new constructions *)
        (*|> TypeChecking.apply*)
        |> PartialEval.apply
        |> CommSimpl.apply (* Intercept introduce recv for onboarding *) 
        |> Clean.apply


        |> EventAutoBoxing.apply
        |> TypeInference5.apply (*Needed since we introduce new constructions *)
        |> CommSimpl.apply (* Transform receive to async + ports *) 


        (* Last cleansing *)
        |> PartialEval.apply

        |> Reflexivity.apply
    in

    ir3
    |> Frontend.to_impl gamma gamma_types sealed_envs targets impl_filename  
    |> Codegen.codegen project_dir build_dir places targets;

    (* Before rewriting *)
    let module TopologyPrinter = ((Common.Topology.Make((struct let component2target = (Codegen.make_component2target ()) end))):Common.Topology.Sig) in (* Warning make_component2target can not be called before spliting until split.ml was improved*)
    TopologyPrinter.generate_static_logical_topology build_dir "ir1" ir1;
    TopologyPrinter.generate_static_logical_topology build_dir "ir2" ir2;
    TopologyPrinter.generate_static_logical_topology build_dir "ir3" ir3;
    ()

let process_stats places_file targets_file impl_filename filename =
    let places = Frontend.process_place places_file in

    let ir = Frontend.to_ast places filename in

    ir
    |> Statistic.analyze_program