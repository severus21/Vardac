package com.lg4dc.timers;

import java.time.Duration;
import java.util.UUID;

import com.bmartin.CborSerializable;
import com.lg4dc.ASTStype;
import com.lg4dc.ActivationRef;

// Time lower bound
public class LBSessionTimer extends SessionTimer {
    public LBSessionTimer (UUID session_id, ActivationRef<CborSerializable> replyTo){
        super(session_id, replyTo);
    }
}