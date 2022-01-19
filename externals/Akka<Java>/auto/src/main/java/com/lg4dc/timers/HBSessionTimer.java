package com.lg4dc.timers;

import java.time.Duration;
import java.util.UUID;

import akka.actor.typed.ActorRef;

import com.bmartin.CborSerializable;
import com.lg4dc.ASTStype;
import com.lg4dc.ActivationRef;

// Timer higher bound i.e timeout
public class HBSessionTimer extends SessionTimer {
    public HBSessionTimer (UUID session_id, ActivationRef<CborSerializable> replyTo){
        super(session_id, replyTo);;
    }
}