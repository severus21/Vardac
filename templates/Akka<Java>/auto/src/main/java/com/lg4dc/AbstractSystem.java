package com.lg4dc;

import akka.actor.Address;
import akka.actor.typed.ActorRef;
import akka.actor.typed.Behavior;
import akka.actor.typed.javadsl.ActorContext;
import akka.actor.typed.javadsl.Behaviors;
import akka.actor.typed.Props;
import akka.actor.typed.receptionist.Receptionist;
import akka.actor.typed.receptionist.ServiceKey;
import akka.actor.typed.javadsl.AbstractBehavior;
import akka.actor.typed.javadsl.ActorContext;
import akka.actor.typed.javadsl.Behaviors;
import akka.actor.typed.javadsl.Receive;
import akka.actor.typed.javadsl.AskPattern;
import akka.cluster.MemberStatus;
import akka.cluster.sharding.typed.ShardingEnvelope;
import akka.cluster.sharding.typed.javadsl.ClusterSharding;
import akka.cluster.sharding.typed.javadsl.Entity;
import akka.cluster.typed.Cluster;
import akka.cluster.typed.ClusterSingleton;
import akka.cluster.typed.Join;
import akka.cluster.typed.SingletonActor;
import akka.actor.typed.javadsl.TimerScheduler;


import java.time.Duration;
import java.util.*;
import java.util.concurrent.CompletionStage;
import java.util.concurrent.ExecutionException;

import com.bmartin.SpawnProtocol;

public abstract class AbstractSystem extends AbstractBehavior<SpawnProtocol.Command> {
    public static final String NAME = "{{system_name}}";
    public static final short MAX_CLUSTER_CONNECT_RETRY = 5;
    public static final ServiceKey<SpawnProtocol.Command> SERVICE_KEY
            = ServiceKey.create(SpawnProtocol.Command.class, NAME);


    public TimerScheduler<SpawnProtocol.Command> timers;
    public Receptionist.Listing current_listing; //Maintain a cache
    public ActorRef<Receptionist.Listing> receptionist_adapter;

    public AbstractSystem(ActorContext<SpawnProtocol.Command> context,  TimerScheduler<SpawnProtocol.Command> timers, String name, Wait wait){
        super(context);
        this.timers = timers;

        if (null != name && !name.isEmpty()) {
            context.setLoggerName(name);
        }
        context.getLog().debug(NAME + "::create()");

        // register to receptionist
        context.getSystem().receptionist().tell(Receptionist.register(SERVICE_KEY, context.getSelf()));

        // Cluster
        Cluster cluster = Cluster.get(context.getSystem());
        assert (null != cluster);

        context.getSystem().receptionist().tell(Receptionist.register(PlaceDiscovery.serviceKeyOf(cluster.selfMember().address()), context.getSelf()));

        /*
        // init cluster listener
        context.spawn(ClusterListener.create(cluster), "ClusterListener");

        // wait for cluster init. TODO: can this be done in a better way ?
        short i = 0;
        do {
            try {
                ++i;
                context.getLog().debug("Cluster state is " + cluster.selfMember().status() +
                        ". Retry " + i + "/" + MAX_CLUSTER_CONNECT_RETRY);

                if (MAX_CLUSTER_CONNECT_RETRY == i) {
                    throw new InstantiationException("Could not connect to the cluster.");
                }

                Thread.sleep(2000);
            } catch (InterruptedException e) {
                throw new InstantiationException("Thread sleep was interrupted.");
            }
        } while ((cluster.selfMember().status() != MemberStatus.up()));
    */
        context.getLog().debug("Joining cluster");
        cluster.manager().tell(Join.create(cluster.selfMember().address()));

    //    ClusterSingleton singleton = ClusterSingleton.get(context.getSystem());

        if (null != wait) {
            synchronized (wait) {
                wait.notify();
            }
        }

        this.receptionist_adapter = getContext().messageAdapter(Receptionist.Listing.class, SpawnProtocol.WrappedListing::new);
    }

    public static final class Wait {
    }
      
    private Behavior<SpawnProtocol.Command> onListing(SpawnProtocol.WrappedListing msg){
        getContext().getLog().info("onListing");
        this.current_listing=msg.response;
        return Behaviors.same();
    }

    private <_T> Behavior<SpawnProtocol.Command> onSpawnAt(SpawnProtocol.SpawnAt msg){
        try{
            getContext().getLog().info("onSpawnAt");
            Set<ActorRef<SpawnProtocol.Command>> roots = Set.of();
            if(this.current_listing == null){ //Only current node is enrolled/onboarded
                roots = Set.of(getContext().getSelf());
            } else {
                roots = this.current_listing.getServiceInstances(PlaceDiscovery.serviceKeyOf(msg.at));
            }

            assert(roots.size() == 1);
            ActorRef<SpawnProtocol.Command> root = null;
            for(ActorRef<SpawnProtocol.Command> _root : roots){
                root = _root; 
                break;
            }
            if(root == null)
                getContext().getLog().warn("No target system as been found at place " + msg.at.toString());
            else
                getContext().getLog().info("Target system at place [" + msg.at.toString()+"] is "+ root.toString());
            assert(root == getContext().getSelf());



            if(root == getContext().getSelf()){
                //Can not do ask with self in Akka, see https://stackoverflow.com/questions/22319660/akka-2-0-send-message-to-self
                SpawnProtocol.Spawn<_T> spawn = new SpawnProtocol.Spawn(msg.runnable, msg.name, msg.props, root);
                ActorRef<_T> actorRef = applySpawn(spawn); 
                getContext().getLog().info("replying  with actorref to "+msg.replyTo.toString());
                msg.replyTo.tell(new WrappedActorRef(actorRef));
                return Behaviors.same();
            }
            
            ActorRef<_T>  actorRef = null;
            if( root != null ){
                CompletionStage<ActorRef<_T>> ask = AskPattern.ask(
                    root,
                    replyTo -> new SpawnProtocol.Spawn(msg.runnable, msg.name, msg.props, replyTo),
                    Duration.ofSeconds(10),
                    getContext().getSystem().scheduler());
                try{
                    actorRef = ask.toCompletableFuture().get(); // blocking call
                } catch (Exception e){
                    System.out.println(e);
                }
            }

            if(actorRef == null)
                getContext().getLog().warn("Remote instanciation failed");

            msg.replyTo.tell(new WrappedActorRef(actorRef));
        } catch (Exception e){
            System.out.println(e);
        }
        return Behaviors.same();
    }

    private <_T> Behavior<SpawnProtocol.Command> onComponentsAt(SpawnProtocol.ComponentsAt msg){
        getContext().getLog().info("onComponentsAt");
        Set<ActorRef> activations = Set.of();
        if(this.current_listing == null){ //Only current node is enrolled/onboarded
            activations = Set.of(getContext().getSelf());
            //TODO collect local
        } else {
            activations = this.current_listing.getServiceInstances(PlaceDiscovery.activationsServiceKeyOf(msg.at));
        }

        getContext().getLog().info("replying  with actorref to "+msg.replyTo.toString());
        msg.replyTo.tell(new WrappedActorRefs(activations));
        return Behaviors.same();
    }
    
    private <_T> ActorRef<_T> applySpawn(SpawnProtocol.Spawn<_T> spawn){
        ActorContext ctx = getContext();
        ActorRef<_T> actorRef;
        if (null == spawn.name || spawn.name.isEmpty()) {
            // anonymous spawn
            actorRef = ctx.spawnAnonymous(Behaviors.setup(spawn.runnable::apply),
                    null == spawn.props ? Props.empty() : spawn.props);
        } else {
            actorRef = SpawnProtocol.spawnWithUniqueName(0, spawn.name, ctx, Behaviors.setup(spawn.runnable::apply),
                    null == spawn.props ? Props.empty() : spawn.props);
        }
        return actorRef;
    }

    private <_T> Behavior<SpawnProtocol.Command> onSpawn(SpawnProtocol.Spawn _spawn){
        try{
            getContext().getLog().info("onSpawn");
            SpawnProtocol.Spawn<_T> spawn = (SpawnProtocol.Spawn<_T>) _spawn;

            ActorRef<_T> actorRef = applySpawn(spawn); 
            spawn.replyTo.tell(new WrappedActorRef(actorRef));
        } catch (Exception e){
            System.out.println(e);
        }
        return Behaviors.same();
    }

    @Override 
    public Receive<SpawnProtocol.Command> createReceive() {
        return newReceiveBuilder()
            .onMessage(SpawnProtocol.WrappedListing.class, this::onListing)
            .onMessage(SpawnProtocol.SpawnAt.class, this::onSpawnAt)
            .onMessage(SpawnProtocol.ComponentsAt.class, this::onComponentsAt)
            .onMessage(SpawnProtocol.Spawn.class, this::onSpawn)
            .build()
        ;
    }
}