package com.lg4dc;

import akka.actor.Address;
import akka.actor.typed.ActorRef;
import akka.actor.typed.Behavior;
import akka.actor.typed.Props;
import akka.actor.typed.javadsl.ActorContext;
import akka.actor.typed.javadsl.Behaviors;
import akka.actor.typed.receptionist.Receptionist;
import akka.actor.typed.receptionist.ServiceKey;
import akka.cluster.typed.Join;
import akka.actor.typed.javadsl.ActorContext;
import akka.actor.typed.javadsl.AskPattern;

import java.security.AccessControlContext;
import java.time.Duration;
import java.util.*;
import java.util.concurrent.CompletionStage;
import java.util.concurrent.ExecutionException;

import com.bmartin.SpawnProtocol;

public class PlaceDiscovery {

    public static ServiceKey<SpawnProtocol.Command> serviceKeyOf(Place place){
        return serviceKeyOf(place.address);
    }

    public static ServiceKey<SpawnProtocol.Command> serviceKeyOf(Address addr){
        return ServiceKey.create(SpawnProtocol.Command.class, AbstractSystem.NAME+addr.toString());
    } 


    public static ServiceKey activationsServiceKeyOf(Place place){
        return activationsServiceKeyOf(place.address);
    }

    public static ServiceKey activationsServiceKeyOf(Address addr){
        return ServiceKey.create(SpawnProtocol.Command.class, AbstractSystem.NAME+"_activations_"+addr.toString());
    }

    public static <_T> ActorRef<_T> spawnAt(ActorContext context, ActorRef<SpawnProtocol.Command> guardian,  SpawnProtocol.SerializableRunnable<_T> runnable, String name, Props props, Place at){
        assert( context != null);
        assert( guardian != null);
        assert( runnable != null);
        assert( name != null);
        assert( at != null);
        assert( at.address != null);
        context.getLog().info("PlaceDiscovery::spawnAt");

        CompletionStage<WrappedActorRef<_T>> ask = AskPattern.ask(
            guardian,
            replyTo -> new SpawnProtocol.SpawnAt(runnable, name, props, at.address, replyTo),
            Duration.ofSeconds(10),
            context.getSystem().scheduler());

        context.getLog().info("waiting PlaceDiscovery::spawnAt");
        try{
            WrappedActorRef<_T> tmp = ask.toCompletableFuture().get();
            return tmp.response; // blocking call
        } catch (Exception e){
            System.out.println(e);
        }

        return null;
    }
    public static Set<ActorRef> componentsAt(ActorContext context, ActorRef<SpawnProtocol.Command> guardian, Place at){
        assert( context != null);
        assert( guardian != null);
        assert( at != null);
        assert( at.address != null);
        context.getLog().info("PlaceDiscovery::componentAt");

        CompletionStage<WrappedActorRefs> ask = AskPattern.ask(
            guardian,
            replyTo -> new SpawnProtocol.ComponentsAt(at.address, replyTo),
            Duration.ofSeconds(10),
            context.getSystem().scheduler());

        context.getLog().info("waiting PlaceDiscovery::componentAt");
        try{
            WrappedActorRefs tmp = ask.toCompletableFuture().get();
            return tmp.response; // blocking call
        } catch (Exception e){
            System.out.println(e);
        }

        return null;
    }
}
