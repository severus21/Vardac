open Easy_logging
open Core
let logger = Logging.make_logger "vardac.DebugBox" Debug []

let () = 
    Core.Config._debug := true;

    print_string "Debug box !!!"