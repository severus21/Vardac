open AAst

module type Params = sig
    val fplace : Core.Error.place
end

module Make (Args:Params) = struct 
    include Args

    let auto_fplace smth = {Core.AstUtils.place = fplace; value=smth}

    (* Expression *)
    let e2_e e =  auto_fplace (e, auto_fplace TUnknown)
    let e2var x =  e2_e (VarExpr x)
    let e2lit lit =  e2_e (LitExpr (lit))
    let e2_lit lit =  e2lit (auto_fplace lit)

    (* Stmt *)
    let stmt2e e = auto_fplace (ExpressionStmt e)
    let stmt2_e e = auto_fplace (ExpressionStmt (e2_e e))
end