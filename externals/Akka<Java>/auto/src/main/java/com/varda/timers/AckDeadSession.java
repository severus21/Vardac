package com.varda.timers;

import java.time.Duration;
import java.util.UUID;

import com.bmartin.CborSerializable;
import com.varda.ASTStype;
import com.varda.ActivationRef;


public class AckDeadSession extends SessionTimer {
    public AckDeadSession (UUID session_id, ActivationRef<CborSerializable> replyTo){
        super(session_id, replyTo);
    }
}
