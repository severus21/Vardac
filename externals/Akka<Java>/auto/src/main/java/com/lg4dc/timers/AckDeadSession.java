package com.lg4dc.timers;

import java.time.Duration;
import java.util.UUID;

import akka.actor.typed.ActorRef;

import com.bmartin.CborSerializable;
import com.lg4dc.ASTStype;


public class AckDeadSession extends SessionTimer {
    public AckDeadSession (UUID session_id, ActorRef<CborSerializable> replyTo){
        super(session_id, replyTo);
    }
}