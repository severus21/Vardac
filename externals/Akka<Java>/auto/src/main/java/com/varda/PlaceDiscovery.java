package com.varda;

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
import akka.actor.typed.javadsl.AskPattern;
import akka.cluster.ddata.ORMultiMapKey;
import akka.cluster.typed.ClusterSingleton;
import akka.cluster.typed.ClusterSingletonSettings;
import akka.cluster.typed.SingletonActor;


import javax.sound.sampled.spi.AudioFileReader;


import io.vavr.*;
import io.vavr.control.*;

import com.bmartin.SpawnProtocol;

public class PlaceDiscovery {

    public static ServiceKey<SpawnProtocol.Command> serviceKeyOf(Place place) {
        return serviceKeyOf(place.address);
    }

    public static ServiceKey<SpawnProtocol.Command> serviceKeyOf(Address addr) {
        return ServiceKey.create(SpawnProtocol.Command.class, AbstractSystem.NAME);// +"_"+addr.toString());
    }

    public static String activationsServiceKeyOf(Place place) {
        return activationsServiceKeyOf(place.address);
    }

    public static String activationsServiceKeyOf(Address addr) {
        return AbstractSystem.NAME + "_activations_" + addr.toString();
    }

    public static <_T> ActorRef<_T> spawnAt(ActorContext context, ActorRef<SpawnProtocol.Command> guardian,
            SpawnProtocol.SerializableRunnable<_T> runnable, String name, Props props, Place at) {
        assert (context != null);
        assert (guardian != null);
        assert (runnable != null);
        assert (name != null);
        assert (at != null);
        assert (at.address != null);
        context.getLog().debug("Requesting for spawn " + context.getSelf().toString());
        context.getLog().debug("PlaceDiscovery::spawnAt at " + at.toString());

        if (at.equals(Place.currentPlace(context))) {
            // Local spawn, the current actor is the parent of the child actor
            SpawnProtocol.Spawn<_T> spawn = new SpawnProtocol.Spawn(runnable, name, props, null);
            ActorRef<_T> actorRef = AbstractSystem.applySpawn(context, spawn);
            context.getLog().debug("spawnAt has been converted into local spawn");
            return actorRef;
        } else {
            CompletionStage<WrappedActorRef<_T>> ask = AskPattern.ask(
                    guardian,
                    replyTo -> new SpawnProtocol.SpawnAt(runnable, name, props, at.address, replyTo),
                    Duration.ofSeconds(10),
                    context.getSystem().scheduler());

            context.getLog().debug("waiting PlaceDiscovery::spawnAt");
            try {
                // blocking call
                WrappedActorRef<_T> tmp = ask.toCompletableFuture().get();
                context.getLog().debug("end PlaceDiscovery::spawnAt");
                return tmp.response;
            } catch (Exception e) {
                System.out.println(e);
            }
            context.getLog().debug("end PlaceDiscovery::spawnAt");
        }

        return null;
    }

    public static Either<Error, Boolean> register(ActorContext context, Place at, ActivationRef a){
        return register(context, at.address, a);
    }
    public static Either<Error, Boolean> register(ActorContext context, Address addr, ActivationRef a){
        ActorRef<ReplactedReceptionist.Command> receptionist = ReplactedReceptionist.getReceptionist(context); 
        receptionist.tell(new ReplactedReceptionist.Register(activationsServiceKeyOf(addr), a));

        //Wait that for visible registered activation
        for(int i = 0; i < 10; i++){ //10 retry max
            Set<ActivationRef> activations = activationsAt(context, addr);
            if(activations.contains(a)){
                context.getLog().debug("register "+a.toString()+" at "+addr.toString());
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

    public static Set<ActivationRef> activationsAt(ActorContext context, Place at) {
        return activationsAt(context, at.address);
    }
    public static Set<ActivationRef> activationsAt(ActorContext context, Address addr) {
        assert (context != null);
        ActorRef<ReplactedReceptionist.Command> receptionist = ReplactedReceptionist.getReceptionist(context);

        CompletionStage<Set<ActivationRef>> ask = AskPattern.ask(
                receptionist,
                replyTo -> new ReplactedReceptionist.GetValue(activationsServiceKeyOf(addr), replyTo),
                Duration.ofSeconds(10),
                context.getSystem().scheduler());

        try {
            // blocking call
            Set<ActivationRef> tmp = ask.toCompletableFuture().get();
            return tmp;
        } catch (Exception e) {
            context.getLog().error(e.toString());
            return new HashSet<>();
        }
    }
}
