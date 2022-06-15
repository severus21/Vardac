open Plg

val name: string
val version: string
module Rt : Plugin.Rt_plg
module Lg : Plugin.Lg_plg 

module Make (Arg: Plugin.CgArgSig) : sig 
    include Plugin.Cg_plg
end