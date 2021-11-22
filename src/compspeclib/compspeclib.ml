(* -------------------------------------------------------------------------- *)
open Core
open Utils
open Codegen
open Easy_logging

let logger = Logging.make_logger "_1_ compspec" Debug [];;

let process_impl filename =
    Frontend.Main.to_impl filename


let process_check build_dir places_file filename = 
    let build_dir = Utils.refresh_or_create_build_dir build_dir in
    let project_dir = Fpath.parent (Fpath.v filename) in

    let places = Frontend.Main.process_place places_file in

    let (gamma, ir) = Frontend.Main.to_ir places filename in
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


    let places = Frontend.Main.process_place places_file in


    let (gamma, ir) = Frontend.Main.to_ir places filename in
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
    let targets = Frontend.Main.process_target ir targets_file in

    let module Rewrite = ((Core.Rewrite.Make((struct let gamma = gamma let targets = targets end))):Core.Rewrite.Sig) in
    let module ImplicitElimination = ((Core.ImplicitElimination.Make((struct let gamma = gamma let targets = targets end))):Core.Rewrite.Sig) in
    let ir2 = ir1 
        |> ImplicitElimination.rewrite_program
        |> function x-> logger#sinfo "Implicit have been removed and turned to explicit";x
        |> Core.AstUtils.dump "explicit IR" IR.show_program
        |> Rewrite.rewrite_program
        |> function x-> logger#sinfo "IR has been rewritten";x
        |> Core.AstUtils.dump "rewritten IR" IR.show_program
    in

    ir2
    |> Frontend.Main.to_impl targets impl_filename  
    |> Codegen.codegen project_dir build_dir places targets;

    (* Before rewriting *)
    let module TopologyPrinter = ((Core.Topology.Make((struct let component2target = (Codegen.make_component2target ()) end))):Core.Topology.Sig) in (* Warning make_component2target can not be called before spliting until split.ml was improved*)
    TopologyPrinter.generate_static_logical_topology build_dir ir1;
    

(* -------------------------------------------------------------------------- *)
(* Sanitize libs *)

(*let sanitize_libs libs=
  let res = List.map (function x -> Sys.file_exists x,x) libs in
  List.iter (function false,x -> Printf.eprintf "Library %s not found." x; exit 1  |true,_ -> ()) res  
 *)