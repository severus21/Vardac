type label_node = 
    | AnyNode of string option
    | AnyRecNode of string option
    | StringNode of string
and label = label_node list (** node1::node2::node3 -> Label(["node1", "node2", "node3"]) *)
[@@deriving show { with_path = false }]

module Label =
    struct
        type t = label
        let compare t1 t2 = Stdlib.compare t1 t2
    end

module LabelSet = struct
    include Set.Make(Label)
    let pp = function x -> failwith "TODO labelset_pp"
end
module LabelMap = Map.Make(Label)  
type label_set = LabelSet.t