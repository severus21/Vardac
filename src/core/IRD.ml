(* De Bruijn *)

module IRDParams : (AstUtils.IRParams with type Variable.t = DeBruijn.t)= struct
    module Variable = DeBruijn 
end 
