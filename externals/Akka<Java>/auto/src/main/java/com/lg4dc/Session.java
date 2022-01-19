package com.lg4dc;

import java.util.Set;
import java.util.List;
import java.util.UUID;

import akka.actor.typed.javadsl.ActorContext;
import akka.actor.typed.javadsl.TimerScheduler;

import io.vavr.*;

import com.bmartin.*;
import com.lg4dc.timers.*;

public class Session implements CborSerializable {
    public UUID bridge_id;
    public ActivationRef left;
    public ActivationRef right;
    public UUID session_id;
    public ASTStype.Base st;

    public Session(
        UUID bridge_id,
        ActivationRef left,
        ActivationRef right,
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
        this.right.actorRef.tell(e);
        context.getLog().debug(String.format("Message %s send from %s to %s", e.toString(), context.getSelf().path().toString(), this.right.actorRef.path().toString()));

        ASTStype.TimerHeader.apply_headers(context, contextTimers, frozen_sessions, dead_sessions, this);

        this.st = this.st.continuations.get(0)._3;
        return this;
    }

    public Session select(
        String label,  
        ActorContext context, 
        TimerScheduler contextTimers, 
        Set<UUID> frozen_sessions, 
        Set<UUID> dead_sessions
    ){
        assert(this.st.continuations.size() > 0);
        if(! Handlers.is_session_alive( context, this.left, frozen_sessions, dead_sessions, this.session_id, this.right)){
            context.getLog().warn( String.format("Can not select from [%s] to [%s] : session %s is dead", this.left.toString(), this.right.toString(), this.session_id));
            return this; //TODO return Err() and Ok(This)
        }

        this.right.actorRef.tell(new LabelEvent(label));
        context.getLog().debug(String.format("Select %s from %s to %s", label, context.getSelf().path().toString(), this.right.actorRef.path().toString()));

        ASTStype.TimerHeader.apply_headers(context, contextTimers, frozen_sessions, dead_sessions, this);


        //TODO list to map ?? (perf)
        //search continuation
        ASTStype.MsgT msgT = new ASTStype.MsgT(label);
        for(Tuple3<ASTStype.MsgT, List<ASTStype.TimerHeader>, ASTStype.Base> branch : this.st.continuations){
            if(branch._1.equals(msgT)){
                this.st = branch._3;
                return this;
            }
        }

        context.getLog().error( String.format("Can not select [%s] from [%s] to [%s] : label is unknown in ST", label, this.left.toString(), this.right.toString(), this.session_id));
        assert(false);
        return this;
    }
}