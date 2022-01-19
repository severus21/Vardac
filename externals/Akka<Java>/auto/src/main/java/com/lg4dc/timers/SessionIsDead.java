package com.lg4dc.timers;

import java.time.Duration;
import java.util.UUID;

import akka.actor.typed.ActorRef;

import com.bmartin.CborSerializable;
import com.lg4dc.ASTStype;
import com.lg4dc.ActivationRef;


public class SessionIsDead extends SessionTimer {
    public SessionIsDead (UUID session_id, ActivationRef replyTo){
        super(session_id, replyTo);
    }
}