type action_protocol = µ x. 
&{ (* non deterministic fire *)
    "get": !K ?Result<V, Error> . x; (* recursif *)
    "put": !Tuple<K,V> ?Result<unit, Error> . x; (* recursif *)
    "abort": !Abort ?Ack . ; (* non-recursif *)
    "commit": !Commit ?Result<unit, Error> . (* non-recursif *)
};

type tc_protocol = ?TXInfo (inline action_protocol);
type full_tx_protocol = !BeginTX (inline tc_protocol);