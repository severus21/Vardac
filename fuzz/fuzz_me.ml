open Core
open IR
open AstUtils
open IRMisc

let fplace = (Error.forge_place "Fuzz" 0 0) 
let auto_fplace smth = {place = fplace; value=smth}
include AstUtils2.Mtype.Make(struct let fplace = fplace end)

let gen_atom = Crowbar.map [Crowbar.bytes] Atom.fresh 
let gen_st_branches st_gen = 
    Crowbar.list (Crowbar.map [gen_atom; st_gen] (fun label st -> 
        (label, st, None)    
    ))

let gen_st : session_type Crowbar.gen = 
    Crowbar.fix (fun a_gen -> 
        Crowbar.choose[
            Crowbar.const (auto_fplace STEnd);
            Crowbar.const (auto_fplace STWildcard);
            Crowbar.const (auto_fplace STBottom);

            Crowbar.map [a_gen] (function st -> auto_fplace (STRecv (mtype_of_ft TStr, st)));
            Crowbar.map [a_gen] (function st -> auto_fplace (STSend (mtype_of_ft TStr, st)));

            Crowbar.map [gen_st_branches a_gen] (function branches -> auto_fplace (STBranch branches));
            Crowbar.map [gen_st_branches a_gen] (function branches -> auto_fplace (STSelect branches));

            (* TODO STVar and STRec and STInline and STPolyVar  and STDual *)
        ]
    )

let test_dual (st:session_type) =
    Crowbar.check_eq 
        ~pp:pp_session_type
        ~cmp:(fun a b -> if equal_stype a b then 0 else 1) (dual (dual st)) st

let identity (x:int) =
    Crowbar.check_eq x (if x = 2 then 0 else x)

let () = 
    (*Crowbar.(add_test ~name:"identity function" [int] (fun i -> identity i));*)
    Crowbar.(add_test ~name:"dual function" [gen_st] (fun i -> test_dual i))