package com.varda.timers;

import java.time.Duration;
import java.util.UUID;

import akka.actor.typed.ActorRef;

import com.bmartin.CborSerializable;
import com.varda.ASTStype;
import com.varda.ActivationRef;


public class SessionIsDead extends SessionTimer {
    public SessionIsDead (UUID session_id, ActivationRef replyTo){
        super(session_id, replyTo);
    }
}