val name: string

module Make (Arg: Plugin.CgArgSig) : sig 
    include Plugin.Cg_plg
end