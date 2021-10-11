package com.lg4dc;

import java.util.UUID;

import akka.actor.typed.ActorRef;
import akka.actor.typed.javadsl.ActorContext;
import akka.actor.typed.javadsl.TimerScheduler;

import com.bmartin.*;

public class Session {
    public UUID bridge_id;
    public ActorRef<CborSerializable> left;
    public ActorRef<CborSerializable> right;
    public UUID session_id;
    public ASTStype.Base st;

    public Session(
        UUID bridge_id,
        ActorRef<CborSerializable> left, //TODO remove Cbor and Cast
        ActorRef<CborSerializable> right,
        ASTStype.Base st) {
        this.bridge_id = bridge_id;
        this.session_id = UUID.randomUUID();
        this.left = left;
        this.right = right;

        this.st = st;
    }
    
    public void set_id(UUID id){
        this.session_id = id;
    }

    public UUID get_id(){
        return this.session_id;
    }
    
    public <E extends Event> Session fire(E e,  ActorContext context, TimerScheduler contextTimers) {
        assert(this.st.continuations.size() == 1);

        e.hydrate(this.bridge_id,  this.session_id,  this.left, this.st, new NoMetadata());
        this.right.tell(e);

        ASTStype.TimerHeader.apply_headers(context, contextTimers, this);

        this.st = this.st.continuations.get(0)._3;
        return this;
    }
}