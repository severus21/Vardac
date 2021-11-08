package com.lg4dc;

import java.util.Set;
import java.util.UUID;

import akka.actor.typed.ActorRef;
import akka.actor.typed.javadsl.ActorContext;
import akka.actor.typed.javadsl.TimerScheduler;

import com.bmartin.*;
import com.lg4dc.timers.*;

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
    
    public <E extends Event> Session fire(
        E e,  
        ActorContext context, 
        TimerScheduler contextTimers, 
        Set<UUID> frozen_sessions, 
        Set<UUID> dead_sessions
    ){
        assert(this.st.continuations.size() == 1);
        if(! Handlers.is_session_alive( context, this.left, frozen_sessions, dead_sessions, this.session_id, this.right)){
            context.getLog().warn( String.format("Can not send message from [%s] to [%s] : session %s is dead", this.left.toString(), this.right.toString(), this.session_id));
            return this; //TODO return Err() and Ok(This)
        }

        e.hydrate(this.bridge_id,  this.session_id,  this.left, this.st, new NoMetadata());
        this.right.tell(e);
        context.getLog().debug(String.format("Message send from %s to %s", context.getSelf().path().toString(), this.right.path().toString()));

        ASTStype.TimerHeader.apply_headers(context, contextTimers, frozen_sessions, dead_sessions, this);

        this.st = this.st.continuations.get(0)._3;
        return this;
    }
}