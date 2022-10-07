package com.varda;

import akka.actor.typed.ActorSystem;
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

import java.rmi.activation.ActivationGroup;
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

import akka.cluster.Member;
import akka.cluster.MemberStatus;
import akka.cluster.typed.Cluster;


import javax.sound.sampled.spi.AudioFileReader;


import io.vavr.*;
import io.vavr.control.*;

import com.bmartin.SpawnProtocol;

public class PlaceDiscovery {

    // Service key only for System (i.e JVM registration)
    public static ServiceKey<SpawnProtocol.Command> serviceKeyOf(Place place) {
        return serviceKeyOf(place.address);
    }

    public static ServiceKey<SpawnProtocol.Command> serviceKeyOf(Address addr) {
        return ServiceKey.create(SpawnProtocol.Command.class, activationsServiceKeyOf(addr));
    }

    // For activation registration
    public static String activationsServiceKeyOf(Place place) {
        return activationsServiceKeyOf(place.address);
    }
    public static String activationsTypedServiceKeyOf(Class cl, Place place) {
        return activationsTypedServiceKeyOf(cl, place.address);
    }

    public static String activationsServiceKeyOf(Address addr) {
        return AbstractSystem.NAME + "_activations_" + addr.toString();
    }

    public static String activationsTypedServiceKeyOf(Class cl, Address addr) {
        return AbstractSystem.NAME + "_" + cl +"_activations_" + addr.toString();
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
            ActorRef<_T> actorRef = AbstractSystem.applySpawn(
                context,
                new SpawnProtocol.Spawn(runnable, name, props, context.getSelf())); 
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

    public static Either<Error, Boolean> register(ActorContext context, Class cl, Place at, ActivationRef a){
        return register(context, cl, at.address, a);
    }
    public static Either<Error, Boolean> register(ActorContext context, Class cl, Address addr, ActivationRef a){
        ActorRef<ReplacedReceptionist.Command> receptionist = ReplacedReceptionist.getReceptionist(context); 

        context.getLog().debug("PlaceDiscovery.register <"+activationsServiceKeyOf(addr)+">");
        context.getLog().debug("PlaceDiscovery.register <"+activationsTypedServiceKeyOf(cl, addr)+">");
        receptionist.tell(new ReplacedReceptionist.Register(activationsServiceKeyOf(addr), a));
        receptionist.tell(new ReplacedReceptionist.Register(activationsTypedServiceKeyOf(cl, addr), a));

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
       return activationsAt(context.getSystem(), addr);
    }

    public static Set<ActivationRef> activationsAt(ActorSystem sys, Address addr) {
        assert (sys != null);
        ActorRef<ReplacedReceptionist.Command> receptionist = ReplacedReceptionist.getReceptionist(sys);

        CompletionStage<Set<ActivationRef>> ask = AskPattern.ask(
                receptionist,
                replyTo -> new ReplacedReceptionist.GetValue(activationsServiceKeyOf(addr), replyTo),
                Duration.ofSeconds(10),
                sys.scheduler());

        try {
            // blocking call
            Set<ActivationRef> tmp = ask.toCompletableFuture().get();
            return tmp;
        } catch (Exception e) {
            sys.log().error(e.toString());
            return new HashSet<>();
        }
    }

    public static Set<ActivationRef> activationsAt(ActorSystem sys, Class cl, Address addr) {
        assert (sys != null);

        sys.log().debug("search activation of type "+cl.toString()+ " at "+addr.toString()+"\n\t<"+activationsTypedServiceKeyOf(cl, addr)+">");
        ActorRef<ReplacedReceptionist.Command> receptionist = ReplacedReceptionist.getReceptionist(sys);

        CompletionStage<Set<ActivationRef>> ask = AskPattern.ask(
                receptionist,
                replyTo -> new ReplacedReceptionist.GetValue(activationsTypedServiceKeyOf(cl, addr), replyTo),
                Duration.ofSeconds(10),
                sys.scheduler());

        try {
            // blocking call
            Set<ActivationRef> tmp = ask.toCompletableFuture().get();
            return tmp;
        } catch (Exception e) {
            sys.log().error(e.toString());
            return new HashSet<>();
        }
    }

    public static Set<ActivationRef> activationsAt(ActorSystem sys, Class cl){
        Iterable<Member> members = Cluster.get(sys).state().getMembers();

        Set<ActivationRef> res = new HashSet<>();
        for(Member member : members){
            Set<ActivationRef> tmp = activationsAt(sys, cl, member.address());
            res.addAll(tmp);
        }

        return res;
    }
}
