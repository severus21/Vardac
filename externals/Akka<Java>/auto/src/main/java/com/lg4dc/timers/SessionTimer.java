package com.lg4dc.timers;

import java.time.Duration;
import java.util.UUID;

import akka.actor.typed.ActorRef;

import com.bmartin.CborSerializable;
import com.lg4dc.ASTStype;

public abstract class SessionTimer implements CborSerializable {
    public UUID session_id;
    public ActorRef<CborSerializable> replyTo;

    SessionTimer (UUID session_id, ActorRef<CborSerializable> replyTo){
        this.session_id = session_id;
        this.replyTo = replyTo;
    }
}