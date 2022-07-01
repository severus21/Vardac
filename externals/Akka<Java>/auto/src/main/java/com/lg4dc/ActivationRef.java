package com.lg4dc;

import akka.actor.typed.ActorRef;

import com.bmartin.*;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.Optional;

public class ActivationRef<Command> implements CborSerializable, JsonSerializable, java.io.Serializable {
    @JsonProperty("componentSchema")
    public String componentSchema;

    @JsonProperty("actorRef")
    public ActorRef<Command> actorRef;


    /* Interception states
        If the interception is anonymous
            this.interceptedActivationRef_opt.isEmpty() && this.isInterceptor
        If interception exposed intercepted actor identity
            this.interceptedActivationRef_opt.isPresent() && this.isInterceptor
        If no interception
            this.interceptedActivationRef_opt.isEmpty() && !this.isInterceptor
    */
    @JsonProperty("interceptedActorRef")
    public Optional<ActivationRef<Command>> interceptedActivationRef_opt;

    @JsonProperty("isInterceptor")
    public Boolean isInterceptor; 

    @JsonCreator 
    public ActivationRef (String componentSchema, ActorRef<Command> actorRef, Boolean isInterceptor, Optional<ActivationRef<Command>> interceptedActivationRef_opt){
        this.actorRef = actorRef;
        this.componentSchema = componentSchema;
        this.interceptedActivationRef_opt = interceptedActivationRef_opt;
        this.isInterceptor = isInterceptor; 
    }

    public ActivationRef(ActivationRef<Command> a_ref, Optional<ActivationRef<Command>> interceptedActivationRef_opt){
        this.actorRef = a_ref.actorRef;
        this.componentSchema = a_ref.componentSchema;
        this.interceptedActivationRef_opt = interceptedActivationRef_opt;
        this.isInterceptor = true; 
    }

    public String toString(){
        return this.actorRef.toString();
    }

    public String activationId(){
        // TODO discuss with Benoit
        if(this.interceptedActivationRef_opt.isPresent())
            assert(false); //TODO how to for mocked version ?? static_id?

        return this.actorRef.path().toSerializationFormat();
    }

    public boolean equals(Object obj) {
        if (obj == this) {
            return true;
        }

        if (!(obj instanceof ActivationRef)) {
            return false;
        }   

        ActivationRef<Command> b = (ActivationRef<Command>) obj;
        if(this.actorRef.equals(b.actorRef) && this.isInterceptor.equals(b.isInterceptor) && this.interceptedActivationRef_opt.equals(b.interceptedActivationRef_opt)){
            assert(this.componentSchema.equals(b.componentSchema));
            return true;
        }else{
            return false;
        }
    }
    
}
