
signature T = sig
    field a : int
    field b : int invariant 1
end

component Z (x:unit) = struct
end

component X (C: channel &{l:!int.; ll:!float?int.}) (x: int) (Y: component T) = struct
    field a = 1
end


