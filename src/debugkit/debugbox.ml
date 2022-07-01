open Easy_logging
open Core
let logger = Logging.make_logger "_1_ vardac.DebugBox" Debug []

let () = 
    Core.Config._debug := true;

    print_string "Debug box !!!"