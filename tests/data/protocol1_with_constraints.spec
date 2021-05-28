type action_protocol = µ x. 
&{ (* non deterministic fire *)
    "get" { | true } : !K ?Result<V, Error> . x; (* recursif *)
    "put": !Tuple<K,V> ?Result<unit, Error> . x { | true }; (* recursif *)
    "abort": !Abort ?Ack . ; (* non-recursif *)
    "commit": !Commit ?Result<unit, Error> . (* non-recursif *)
};