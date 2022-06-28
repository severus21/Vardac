(**
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

        (* Type *)
        let mtype_of_var x = 
            auto_fplace(CType(auto_fplace (TVar x)))
        let mtype_poly_of_var x = 
            auto_fplace(CType(auto_fplace (TPolyVar x)))
        let mtype_of_raw x = 
            auto_fplace(TRaw x)

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
        let mtype_of_cmt cmt = 
            auto_fplace(CompType(auto_fplace cmt))
        let mtype_of_st st = 
            auto_fplace(SType(auto_fplace st))
        let mtype_of_ft ft = 
            mtype_of_ct (TFlatType ft)


        let mtype_of_fun2 targs ret_type = 
            List.fold_right (fun mt1 mt2 -> mtype_of_ct (TArrow (mt1, mt2))) targs ret_type
        let mtype_of_fun args ret_type = 
            mtype_of_fun2 (List.map (function param -> fst param.value) args) ret_type 



        (* Expression *)
        let e2_e ?(mt=auto_fplace EmptyMainType) e =  auto_fplace (e, mt)
        let e2var ?(mt=auto_fplace EmptyMainType) x =  e2_e ~mt:mt (VarExpr x)
        let e2lit ?(mt=auto_fplace EmptyMainType) lit =  e2_e ~mt:mt (LitExpr (lit))
        let e2_lit ?(mt=auto_fplace EmptyMainType) lit =  e2lit ~mt:mt (auto_fplace lit)

        (* Stmt *)
        let stmt2e e = auto_fplace (ExpressionStmt e)
        let stmt2_e e = auto_fplace (ExpressionStmt (e2_e e))

        (* CExpression *)
        let ce2_ce ce = auto_fplace (ce, auto_fplace EmptyMainType) 
        let ce2var x = ce2_ce (VarCExpr x)
    end
end