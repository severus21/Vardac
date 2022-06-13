package com.lg4dc;

import java.util.UUID;

import akka.actor.typed.ActorRef;
import akka.actor.typed.javadsl.ActorContext;
import akka.actor.typed.receptionist.Receptionist;
import akka.actor.typed.receptionist.ServiceKey;

import com.bmartin.*;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonAutoDetect;
import com.fasterxml.jackson.annotation.JsonAutoDetect.Visibility;
import com.fasterxml.jackson.annotation.JsonCreator;

import akka.actor.typed.ActorRef;

import java.util.*;
import java.util.function.*;
import java.time.Duration;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionStage;
import akka.actor.typed.javadsl.AskPattern;

import io.vavr.*;
import io.vavr.control.*;


public final class Bridge<P extends Protocol> implements CborSerializable, JsonSerializable, java.io.Serializable {
    @JsonProperty("id")
    UUID id;
    
    @JsonProperty("protocol")
    P protocol;

    public Bridge (P protocol){
        assert(protocol != null);
        this.id = UUID.randomUUID();
        this.protocol = protocol;
    }

    @JsonCreator 
    public Bridge (P protocol, UUID id){
        assert(protocol != null);
        assert(id != null);

        this.id = id;
        this.protocol = protocol;
    }
    
    //Same key => Same id
    public Bridge (P protocol, String key){
        assert(protocol != null);
        assert(key != null);

        this.id = UUIDCreator.getSha1Uuid(key);
        this.protocol = protocol;
    }

    public UUID get_id(){
        return this.id;
    }
    public String toString(){
        return "Bridge<UUID="+this.id+">";
    }

    public boolean equals(Object obj) {
        if (obj == this) {
            return true;
        }

        if (!(obj instanceof Bridge)) {
            return false;
        }   

        Bridge b = (Bridge) obj;
        return this.id.equals(b.id) && this.protocol.equals(b.protocol);
    }

    public ServiceKey<SpawnProtocol.Command> leftServiceKey(){
        return ServiceKey.create(SpawnProtocol.Command.class, AbstractSystem.NAME+"_bridge_left_"+this.id.toString());
    } 

    public Either<Error, Boolean> leftRegister(ActorContext context, ActivationRef a){
        // register to receptionist to allow reflexivity
        context.getSystem().receptionist().tell(
            Receptionist.register(
                this.leftServiceKey(), a.actorRef));
        return Either.right(true);
    }

    public ServiceKey<SpawnProtocol.Command> rightServiceKey(){
        return ServiceKey.create(SpawnProtocol.Command.class, AbstractSystem.NAME+"_bridge_right_"+this.id.toString());
    } 

    public Either<Error, Boolean> rightRegister(ActorContext context, ActivationRef a){
        // register to receptionist to allow reflexivity
        context.getSystem().receptionist().tell(
            Receptionist.register(
                this.rightServiceKey(), a.actorRef));
        return Either.right(true);
    }

    public Either<Error, Set<ActivationRef>> activationsOf(ActorContext context, ActorRef<SpawnProtocol.Command> guardian, Function<ActorRef, SpawnProtocol.Command> msg){
        assert( context != null);
        assert( guardian != null);

        CompletionStage<WrappedActorRefs> ask = AskPattern.ask(
            guardian,
            replyTo -> msg.apply(replyTo),
            Duration.ofSeconds(10),
            context.getSystem().scheduler());

        try{
            // blocking call
            WrappedActorRefs tmp = ask.toCompletableFuture().get();
            Set<ActivationRef> res = new HashSet<>();
            for(ActorRef a : tmp.response){
                res.add(new ActivationRef(
                    "TODO", //can not retrieve schema from receptionnist - workaround use distributed data
                    a,
                    false,
                    Optional.empty()));
            }
            return Either.right(res);
        } catch (Exception e){
            System.out.println(e);
            return Either.left(new Error(e.toString()));
        }
    }

    public Either<Error, Set<ActivationRef>> leftActivations(ActorContext context, ActorRef<SpawnProtocol.Command> guardian){
        return activationsOf(
            context, guardian,
            replyTo -> new SpawnProtocol.LeftActivationsOf(this.id, replyTo)
        );
    }

    public Either<Error, Set<ActivationRef>> rightActivations(ActorContext context, ActorRef<SpawnProtocol.Command> guardian){
        return activationsOf(
            context, guardian,
            replyTo -> new SpawnProtocol.RightActivationsOf(this.id, replyTo)
        );
    }

}