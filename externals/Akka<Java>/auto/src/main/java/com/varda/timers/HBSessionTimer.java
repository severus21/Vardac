package com.varda.timers;

import java.time.Duration;
import java.util.UUID;

import akka.actor.typed.ActorRef;

import com.bmartin.CborSerializable;
import com.varda.ASTStype;
import com.varda.ActivationRef;

// Timer higher bound i.e timeout
public class HBSessionTimer extends SessionTimer {
    public HBSessionTimer (UUID session_id, ActivationRef<CborSerializable> replyTo){
        super(session_id, replyTo);;
    }
}