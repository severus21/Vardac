event k of;
event v of;
event abord of;
event commit of;
event ack of;

protocol action_protocol = µ x. 
&{ (* non deterministic fire *)
    l_get {| true }: !k ?v{|true} - x; (* recursif *)
    l_put: !k!v ?ack - x; (* recursif *)
    l_abort: !abort ?ack . ; (* non-recursif *)
    l_commit: !commit ?ack . (* non-recursif *)
};

event txinfo of;
event begintx of;
protocol tc_protocol = ?txinfo (inline action_protocol);
protocol full_tx_protocol = !begintx (inline tc_protocol);