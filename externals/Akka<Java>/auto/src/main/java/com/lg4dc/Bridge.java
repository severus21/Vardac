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
import akka.cluster.ddata.ORMultiMapKey;
import akka.cluster.typed.ClusterSingleton;
import akka.cluster.typed.ClusterSingletonSettings;
import akka.cluster.typed.SingletonActor;


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

    public String leftServiceKey(){
        return Bridge.leftServiceKey(this.id);
    } 

    static public String leftServiceKey(UUID id){
        return AbstractSystem.NAME+"_bridge_left_"+id.toString();
    } 

    private Either<Error, Boolean> register(ActorContext context, ActorRef<SpawnProtocol.Command> guardian, ActivationRef a, boolean left){
        ActorRef<ReplactedReceptionist.Command> receptionist = ReplactedReceptionist.getReceptionist(context); 
        receptionist.tell(new ReplactedReceptionist.Register(left ? this.leftServiceKey() : this.rightServiceKey(), a));

        //Wait that for visible registered activation
        for(int i = 0; i < 10; i++){ //10 retry max
            Set<ActivationRef> activations = 
                left ? this.leftActivations(context, guardian).get() : this.rightActivations(context, guardian).get();
            if(activations.contains(a)){
                if(left)
                    context.getLog().info("leftRegister "+a.toString()+" at left of "+this.id.toString());
                else
                    context.getLog().info("rightRegister "+a.toString()+" at right of "+this.id.toString());

                return Either.right(true);
            } else {
                try{
                    Thread.sleep(500);
                } catch( Exception e){
                    System.out.println(e);
                }
            }
        }

        context.getLog().error("not registered, timeout");
        return Either.left(new Error("timeout before seeing its own update"));
    }

    public Either<Error, Boolean> leftRegister(ActorContext context, ActorRef<SpawnProtocol.Command> guardian, ActivationRef a){
        return this.register(context, guardian, a, true); 
    }

    public String rightServiceKey(){
        return Bridge.rightServiceKey(this.id);
    } 
    static public String rightServiceKey(UUID id){
        return AbstractSystem.NAME+"_bridge_right_"+id.toString();
    } 

    public Either<Error, Boolean> rightRegister(ActorContext context, ActorRef<SpawnProtocol.Command> guardian, ActivationRef a){
        return this.register(context, guardian, a, false); 
    }

    public Either<Error, Set<ActivationRef>> activationsAt(ActorContext context, boolean left){
        assert( context != null);
        ActorRef<ReplactedReceptionist.Command> receptionist = ReplactedReceptionist.getReceptionist(context); 

        CompletionStage<Set<ActivationRef>> ask = AskPattern.ask(
            receptionist,
            replyTo -> new ReplactedReceptionist.GetValue(left ? this.leftServiceKey() : this.rightServiceKey(), replyTo),
            Duration.ofSeconds(10),
            context.getSystem().scheduler());

        try{
            // blocking call
            Set<ActivationRef> tmp = ask.toCompletableFuture().get();
            context.getLog().info("Find at right/left "+tmp.size());
            return Either.right(tmp);
        } catch (Exception e){
            context.getLog().error(e.toString());
            return Either.left(new Error(e.toString()));
        }
    }

    public Either<Error, Set<ActivationRef>> leftActivations(ActorContext context, ActorRef<SpawnProtocol.Command> guardian){
        return activationsAt( context, true);
    }

    public Either<Error, Set<ActivationRef>> rightActivations(ActorContext context, ActorRef<SpawnProtocol.Command> guardian){
        return activationsAt( context, false);
    }

}