package com.lg4dc.timers;

import java.time.Duration;
import java.util.UUID;

import akka.actor.typed.ActorRef;

import com.bmartin.CborSerializable;
import com.lg4dc.ASTStype;

// Time lower bound
public class LBSessionTimer extends SessionTimer {
    public LBSessionTimer (UUID session_id, ActorRef<CborSerializable> replyTo){
        super(session_id, replyTo);
    }
}