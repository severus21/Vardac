package com.lg4dc.timers;

import java.time.Duration;
import java.util.UUID;

import com.bmartin.CborSerializable;
import com.lg4dc.ASTStype;
import com.lg4dc.ActivationRef;


public class AckDeadSession extends SessionTimer {
    public AckDeadSession (UUID session_id, ActivationRef<CborSerializable> replyTo){
        super(session_id, replyTo);
    }
}
