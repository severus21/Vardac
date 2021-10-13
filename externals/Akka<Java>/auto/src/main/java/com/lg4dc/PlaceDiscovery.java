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

    public static ActorRef<SpawnProtocol.Command> discover(ActorContext<Receptionist.Listing> context, Place place){
        return discover(context, place.address);
    }

    public static ActorRef<SpawnProtocol.Command> discover(ActorContext<Receptionist.Listing> context, Address addr){
        ServiceKey<SpawnProtocol.Command> serviceKey = serviceKeyOf(addr);

        //ActorRef<Receptionist.Listing> listingAdapter = context.messageAdapter(Receptionist.Listing.class, WrappedBackendResponse::new);
        CompletionStage<Receptionist.Listing> ask = AskPattern.ask(
            context.getSystem().receptionist(),
            replyTo -> Receptionist.find(serviceKey, replyTo),
            Duration.ofSeconds(3),
            context.getSystem().scheduler());
        try{
            Receptionist.Listing result = ask.toCompletableFuture().get(); // blocking call

            for(ActorRef<SpawnProtocol.Command> root : result.getServiceInstances(serviceKey)){
                return root; 
            }
        } catch (Exception e){
            System.out.println(e);
        }

        return null;
    } 

    public static <_T> _T spawnAt(ActorContext<Receptionist.Listing> context, SpawnProtocol.SerializableRunnable<_T> runnable, String name, Props props, Place at){
        ActorRef<SpawnProtocol.Command> root = discover(context, at);

        CompletionStage<_T> ask = AskPattern.ask(
            root,
            replyTo -> new SpawnProtocol.Spawn(runnable, name, props, replyTo),
            Duration.ofSeconds(10),
            context.getSystem().scheduler());
        try{
            _T result = ask.toCompletableFuture().get(); // blocking call
            return result;
        } catch (Exception e){
            System.out.println(e);
        }

        return null;
    } 
}
