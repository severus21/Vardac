let action = ref ""
let ir_file = ref ""
let options = ref []

let common_options = 
[
    "--ir_file", Arg.Set_string ir_file, "Load a YAML representing an AST (IR)"; 
]
let options_outline = common_options


let record a= 
    action := a;
    match !action with
    | "outline" -> options := options_outline
    | "playground" -> ()
    | _ ->  raise (Arg.Bad "This action is undefined, allowed actions are [outline]")

let usage =
    Printf.sprintf "Usage: <action> %s <options> <filename>" Sys.argv.(0)

let logger = Easy_logging.Logging.make_logger "vardac.DebugBox" Debug [Cli Debug]

let () =
    Printexc.record_backtrace false;
    Arg.parse_dynamic options record usage;

    match !action with
    | "outline" -> begin
        let json = Yojson.Safe.from_file !ir_file in
        match Core.IR.program_of_yojson json with 
        | Ppx_deriving_yojson_runtime.Result.Ok program -> 
            Outline.cartography_program program 
        | Ppx_deriving_yojson_runtime.Result.Error err -> Core.Error.error "Json deserialization error: \n%s" err 
    end
    | "playground" -> begin
        Playground.run ()
    end
    | "" -> Core.Error.error "%s" usage  