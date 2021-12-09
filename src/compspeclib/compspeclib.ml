(* -------------------------------------------------------------------------- *)
open Core
open Utils
open Codegen
open Easy_logging

let logger = Logging.make_logger "_1_ compspec" Debug [];;

let process_impl filename =
    Frontend.to_impl filename


let process_check build_dir places_file filename = 
    let build_dir = Utils.refresh_or_create_build_dir build_dir in
    let project_dir = Fpath.parent (Fpath.v filename) in

    let places = Frontend.process_place places_file in

    let (gamma, ir) = Frontend.to_ir places filename in
    ir
    |> Core.PartialEval.peval_program
    |> function x-> logger#sinfo "IR has been partially evaluated";x
    |> Core.AstUtils.dump "pevaled IR" IR.show_program
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
        |> Core.Reduce.reduce_program 
        |> function x-> logger#sinfo "IR has been reduced"; x
        |> Core.TypeInference.tannot_program
        |> function x-> logger#sinfo "IR has been annotated with types (type reconstruction only)"; x
        (*|> Core.TypeChecking.tcheck_program 
        |> function x-> logger#sinfo "IR has been typed checked successfully"; x*)
        |> Core.AstUtils.dump "annotated IR (with types)" IR.show_program
        |> Core.PartialEval.peval_program
        |> function x-> logger#sinfo "IR has been partially evaluated"; x
        |> Core.AstUtils.dump "pevaled IR" IR.show_program
    in

    (* extract targets definitions from file *)
    let targets = Frontend.process_target ir targets_file in

    let module Rewrite = ((Core.Rewrite.Make((struct let gamma = gamma let targets = targets end))):Core.Rewrite.Sig) in
    let module ImplicitElimination = ((Core.ImplicitElimination.Make((struct let gamma = gamma let targets = targets end))):Core.Rewrite.Sig) in
    let ir2 = ir1 
        |> ImplicitElimination.rewrite_program
        |> function x-> logger#sinfo "Implicit have been removed and turned to explicit";x
        |> Core.AstUtils.dump "explicit IR" IR.show_program
    in

    let ir3 = ir2
        |> Core.PartialEval.peval_program
        |> Derive.derive_program
        |> function x-> logger#sinfo "Derives has been applied to IR";x
        |> Core.AstUtils.dump "derived IR" IR.show_program

        (* TODO FIXME 
            0. Annots derived expression with types (and not just EmptyMainType)
            1. Check that derivation to not introduced bugs that can be detected by type-checking
        |> Core.TypeInference.tannot_program
        |> Core.TypeChecking.tcheck_program 
        *)

        (* Clean derived code *)
        |> Core.PartialEval.peval_program
        |> ImplicitElimination.rewrite_program
        
        |> Rewrite.rewrite_program (* Transform receive to async + ports *) 
        |> function x-> logger#sinfo "IR has been rewritten";x
        |> Core.AstUtils.dump "rewritten IR" IR.show_program

        (* Every pass that change ports and components should be performed before runngin the Intercept transformation *)
        |> Intercept.rewrite_program
        |> function x-> logger#sinfo "Interception in IR compiled away";x
        |> Core.AstUtils.dump "interception-less IR" IR.show_program

        |> Core.Clean.clean_program
        |> function x-> logger#sinfo "IR has been cleaned";x
        |> Core.AstUtils.dump "cleaned IR" IR.show_program
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