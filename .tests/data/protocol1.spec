event k of;
event v of;
event abord of;
event commit of;
event ack of;

protocol action_protocol = µ x. 
&{ (* non deterministic fire *)
    "get": !k ?v - x; (* recursif *)
    "put": !k!v ?ack - x; (* recursif *)
    "abort": !abort ?ack . ; (* non-recursif *)
    "commit": !commit ?ack . (* non-recursif *)
};

event txinfo of;
event begintx of;
protocol tc_protocol = ?txinfo (inline action_protocol);
protocol full_tx_protocol = !begintx (inline tc_protocol);