open Ast

open Core
open AstUtils

let fplace = (Error.forge_place ("plg.Java.JavaUtils") 0 0) 
let auto_fplace smth = {place = fplace; value=smth}

(* TODO Infer it here (hack), we should port the TypeInference path both on IR and IRI and run it after prepare and future elim in Akka plg *)
let mtype_of_fun2 targs ret_type = 
    let targs = match targs with 
        | [] -> [auto_fplace (TAtomic "void")]
        | _ -> targs
    in

    List.fold_right (fun mt1 mt2 -> auto_fplace (ClassOrInterfaceType  ( auto_fplace (TAtomic "Function"),  [mt1; mt2]))) targs ret_type
let mtype_of_fun args ret_type = 
    mtype_of_fun2 (List.map (function param -> fst param) args) ret_type 

let jtype_of_lambda params e=
    mtype_of_fun params (snd e.value)