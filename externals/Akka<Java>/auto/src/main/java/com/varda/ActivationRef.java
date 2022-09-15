package com.varda;

import akka.actor.typed.ActorRef;

import com.bmartin.*;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.Optional;
import java.util.UUID;

public class ActivationRef<Command> implements CborSerializable, JsonSerializable, java.io.Serializable {
    @JsonProperty("mockec_uuid")
    public UUID mocked_uuid; //used when ActivationRef does not point to an actor, but rather to a local object


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
    public SerializableOptional<ActivationRef<Command>> interceptedActivationRef_opt;

    @JsonProperty("isInterceptor")
    public Boolean isInterceptor; 

    @JsonCreator 
    public ActivationRef (String componentSchema, ActorRef<Command> actorRef, Boolean isInterceptor, SerializableOptional<ActivationRef<Command>> interceptedActivationRef_opt, UUID mocked_uuid){
        this.actorRef = actorRef;
        this.componentSchema = componentSchema;
        this.interceptedActivationRef_opt = interceptedActivationRef_opt;
        this.isInterceptor = isInterceptor; 
        this.mocked_uuid = mocked_uuid;
    }

    public ActivationRef (String componentSchema, ActorRef<Command> actorRef, Boolean isInterceptor, SerializableOptional<ActivationRef<Command>> interceptedActivationRef_opt){
        this.actorRef = actorRef;
        this.componentSchema = componentSchema;
        this.interceptedActivationRef_opt = interceptedActivationRef_opt;
        this.isInterceptor = isInterceptor; 
    }

    public ActivationRef(ActivationRef<Command> a_ref, SerializableOptional<ActivationRef<Command>> interceptedActivationRef_opt){
        this.actorRef = a_ref.actorRef;
        this.componentSchema = a_ref.componentSchema;
        this.interceptedActivationRef_opt = interceptedActivationRef_opt;
        this.isInterceptor = true; 
    }

    //Mocked activation ref, mainly used for inlining
    public ActivationRef(){
        this.mocked_uuid = UUID.randomUUID();
    }
    public ActivationRef(UUID mocked_uuid){
        this.mocked_uuid = mocked_uuid;
    }

    public String toString(){
        assert(this.check_integrity());
        if(this.is_mocked())
            return this.mocked_uuid.toString();
        else if(this.interceptedActivationRef_opt.isPresent())
            return "Intercepted: \n\t-"+this.interceptedActivationRef_opt.get().toString()+"\nBehind\n\t-"+this.actorRef.toString();
        else 
            return this.actorRef.toString();
    }

    public boolean is_mocked(){
        assert(this.check_integrity());
        return this.mocked_uuid != null;
    }

    public String activationId(){
        assert(this.check_integrity());

        if(this.is_mocked())
            return this.mocked_uuid.toString();

        //UUID derived from (id_interceptor, id_intercepted)
        if(this.interceptedActivationRef_opt.isPresent())
            return UUIDCreator.getSha1Uuid(
                this.actorRef.path().toSerializationFormat()
                + this.interceptedActivationRef_opt.get().activationId()
            ).toString();

        return this.actorRef.path().toSerializationFormat();
    }

    public boolean check_integrity(){
        //1) an activation_ref can not be mocked and interced/classical one
        boolean c1 = mocked_uuid == null || (componentSchema == null && actorRef == null && interceptedActivationRef_opt == null && isInterceptor == null);

        return c1;
    }

    public boolean equals(Object obj) {
        if (obj == this) {
            return true;
        }

        if (!(obj instanceof ActivationRef)) {
            return false;
        }   

        ActivationRef<Command> b = (ActivationRef<Command>) obj;

        assert(this.check_integrity());
        assert(b.check_integrity());
       
        return this.activationId() == b.activationId();
    }

    @Override
    public int hashCode() {
        return activationId().hashCode();
    }
    
}
