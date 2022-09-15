package com.varda;

import java.util.Set;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

import akka.actor.typed.javadsl.ActorContext;
import akka.actor.typed.javadsl.TimerScheduler;

import io.vavr.*;
import io.vavr.control.*;


import com.bmartin.*;
import com.varda.timers.*;
import com.varda.metadata.*;
import com.varda.OutPort;

public class Session implements CborSerializable {
    public UUID bridge_id;
    public ActivationRef left;
    public ActivationRef right;
    public UUID session_id;
    public ASTStype.Base st;

    public Boolean init_stage;
    public SerializableOptional<ActivationRef> hidden_right; //e.g. non anonymous redirection
    
    public AbstractPort init_port;

    public Session(
        UUID bridge_id,
        ActivationRef left,
        ActivationRef right,
        ASTStype.Base st,
        Boolean init_stage,
        SerializableOptional<ActivationRef> hidden_right,
        AbstractPort init_port //used for RecvElim for now 
        ) {

        assert(bridge_id != null);
        assert(right != null);
        assert(left != null);
        assert(st != null);
        assert(init_stage != null);
        assert(hidden_right != null);
        assert(init_port != null);

        this.bridge_id = bridge_id;
        this.session_id = UUID.randomUUID();
        this.left = left;
        this.right = right;

        this.st = st;
        this.init_stage = init_stage;
        this.hidden_right = hidden_right;

        this.init_port = init_port;
    }
    
    public void set_id(UUID id){
        this.session_id = id;
    }

    public UUID get_id(){
        return this.session_id;
    }
    
    public <E extends Event> Either<Error, Session> fire(
        E e,  
        ActorContext context, 
        TimerScheduler contextTimers, 
        Set<UUID> frozen_sessions, 
        Set<UUID> dead_sessions
    ){
        assert(this.st.continuations.size() == 1);
        if(! Handlers.is_session_alive( context, this.left, frozen_sessions, dead_sessions, this.session_id, this.right)){
            context.getLog().warn( String.format("Can not send message from [%s] to [%s] : session %s is dead", this.left.toString(), this.right.toString(), this.session_id));
            return Either.left(new Error("Dead session"));
        }

        e.hydrate(this.bridge_id,  this.session_id,  this.left, this.st, this.init_stage, this.hidden_right, new NoMetadata());
        this.right.actorRef.tell(e);
        context.getLog().debug(String.format("Message %s send from %s to %s", e.toString(), context.getSelf().path().toString(), this.right.actorRef.path().toString()));

        ASTStype.TimerHeader.apply_headers(context, contextTimers, frozen_sessions, dead_sessions, this);

        this.st = this.st.continuations.get(0)._3;
        this.init_stage = false;

        return Either.right(this);
    }

    public Either<Error, Session> apply_select(
        LabelEvent label,  
        ActorContext context 
    ){
        return apply_select(label.value, context);
    }

    public Either<Error, Session> apply_select(
        String label,  
        ActorContext context 
    ){

        assert(this.st.continuations.size() > 0);

        //TODO list to map ?? (perf)
        //search continuation
        ASTStype.MsgT msgT = new ASTStype.MsgT(label);
        for(Tuple3<ASTStype.MsgT, List<ASTStype.TimerHeader>, ASTStype.Base> branch : this.st.continuations){
            if(branch._1.equals(msgT)){
                this.st = branch._3;
                return Either.right(this);
            }
        }

        context.getLog().error( String.format("Can not select [%s] from [%s] to [%s] : label is unknown in ST", label, this.left.toString(), this.right.toString(), this.session_id));

        this.init_stage = false;
        return Either.left(new Error("Unknown label"));
    }

    public Either<Error, Session> select(
        String label,  
        ActorContext context, 
        TimerScheduler contextTimers, 
        Set<UUID> frozen_sessions, 
        Set<UUID> dead_sessions
    ){
        assert(this.st.continuations.size() > 0);
        if(! Handlers.is_session_alive( context, this.left, frozen_sessions, dead_sessions, this.session_id, this.right)){
            context.getLog().warn( String.format("Can not select from [%s] to [%s] : session %s is dead", this.left.toString(), this.right.toString(), this.session_id));
            return Either.left(new Error("Dead session"));
        }

        LabelEvent e = new LabelEvent(label);
        e.hydrate(this.bridge_id,  this.session_id,  this.left, this.st, this.init_stage, this.hidden_right, new NoMetadata());
        this.right.actorRef.tell(e);
        context.getLog().debug(String.format("Select %s from %s to %s", label, context.getSelf().path().toString(), this.right.actorRef.path().toString()));

        ASTStype.TimerHeader.apply_headers(context, contextTimers, frozen_sessions, dead_sessions, this);

        return apply_select(label, context);
    }

    public Either<Error, Session> select(
        LabelEvent label,  
        ActorContext context, 
        TimerScheduler contextTimers, 
        Set<UUID> frozen_sessions, 
        Set<UUID> dead_sessions
    ){
        return this.select(label.value, context, contextTimers, frozen_sessions, dead_sessions);
    }
}