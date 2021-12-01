(*
    IR_common depends of AstUtils
    AstUtils2 depends of IR_common
*)

open AstUtils
open IR

module Mtype = struct
    module type Params = sig
        val fplace : Error.place
    end
    module Make (Args:Params) = struct 
        include Args

        let auto_fplace smth = {place = fplace; value=smth}

        let mtype_of_var x = 
            auto_fplace(CType(auto_fplace (TVar x)))
        let mtype_poly_of_var x = 
            auto_fplace(CType(auto_fplace (TPolyVar x)))

        let mtype_of_svar x = 
            auto_fplace(SType(auto_fplace (STInline x)))
        let mtype_poly_of_svar x = 
            auto_fplace(SType(auto_fplace (STPolyVar x)))
        let mtype_of_cvar x = 
            auto_fplace(CompType(auto_fplace (CompTUid x)))
        let mtype_poly_of_cvar x = 
            auto_fplace(CompType(auto_fplace (TPolyCVar x)))
        let mtype_of_ct ct = 
            auto_fplace(CType(auto_fplace ct))
        let mtype_of_st st = 
            auto_fplace(SType(auto_fplace st))
        let mtype_of_ft ft = 
            mtype_of_ct (TFlatType ft)


        let mtype_of_fun args ret_type = 
            List.fold_right (fun {value=(mt1,_)} mt2 -> mtype_of_ct (TArrow (mt1, mt2))) args ret_type
    end
end