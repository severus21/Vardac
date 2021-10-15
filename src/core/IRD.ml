(* De Bruijn *)

module IRDParams : (IR_common.IRParams with type Variable.t = DeBruijn.t)= struct
    module Variable = DeBruijn 
end 
