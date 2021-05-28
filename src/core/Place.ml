type network = {addr: string; port: int}
and caracteristics = {cpus: int} 
and place = 
  | Nil
  | Var of Atom.atom (* reference to a defined place *)    
  | Place of Atom.atom * network option * caracteristics option * place list     
  | Root of place list (*artificial root*)                                                              
[@@deriving show { with_path = false }]

