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
import java.util.stream.Collectors;

import com.bmartin.SpawnProtocol;

public abstract class AbstractSystem extends AbstractBehavior<SpawnProtocol.Command> {
    public static final String NAME = "{{system_name}}";
    public static final short MAX_CLUSTER_CONNECT_RETRY = 5;

    public TimerScheduler<SpawnProtocol.Command> timers;
    public Receptionist.Listing root_listing; // Maintain a cache
    public Receptionist.Listing activations_listing; // Maintain a cache
    public ActorRef<Receptionist.Listing> receptionist_adapter;

    public AbstractSystem(ActorContext<SpawnProtocol.Command> context, TimerScheduler<SpawnProtocol.Command> timers,
            String name, Wait wait) {
        super(context);
        this.timers = timers;

        if (null != name && !name.isEmpty()) {
            context.setLoggerName(name);
        }
        context.getLog().debug(NAME + "::create()");

        // Cluster
        Cluster cluster = Cluster.get(context.getSystem());
        assert (null != cluster);

        // register to receptionist
        context.getSystem().receptionist().tell(
                Receptionist.register(PlaceDiscovery.serviceKeyOf(cluster.selfMember().address()), context.getSelf())); // per
                                                                                                                        // place

        // init cluster listener
        // context.spawn(ClusterListener.create(cluster), "ClusterListener");

        // wait for cluster init. TODO: can this be done in a better way ?
        short i = 0;
        do {
            try {
                ++i;
                context.getLog().debug("Cluster state is " + cluster.selfMember().status() + ". Retry " + i + "/"
                        + MAX_CLUSTER_CONNECT_RETRY);

                if (MAX_CLUSTER_CONNECT_RETRY == i) {
                    throw new InstantiationException("Could not connect to the cluster.");
                }

                Thread.sleep(2000);
            } catch (Exception e) {
                context.getLog().error("Thread sleep was interrupted: " + e);
            }
        } while ((cluster.selfMember().status() != MemberStatus.up()));

        context.getLog().debug("Joining cluster");
        cluster.manager().tell(Join.create(cluster.selfMember().address()));

        // ClusterSingleton singleton = ClusterSingleton.get(context.getSystem());

        if (null != wait) {
            synchronized (wait) {
                wait.notify();
            }
        }

        // Receptionist
        this.receptionist_adapter = getContext().messageAdapter(Receptionist.Listing.class,
                SpawnProtocol.WrappedListing::new);
        context.getSystem().receptionist().tell(
            Receptionist.subscribe(PlaceDiscovery.serviceKeyOf(cluster.selfMember().address()), this.receptionist_adapter)
        ); // per
        context.getSystem().receptionist().tell(
            Receptionist.subscribe(PlaceDiscovery.activationsServiceKeyOf(cluster.selfMember().address()), this.receptionist_adapter)
        ); // per
        //TODO monitor Reachability and MemberEvent cf. ClusterListener
        forceUpdateListing(cluster.selfMember().address());
    }

    public static final class Wait {
    }

    private Behavior<SpawnProtocol.Command> onListing(SpawnProtocol.WrappedListing msg) {
        getContext().getLog().info("onListing "+msg.response.getKey().id());
        if(msg.response.getKey().id().contains("_activations_")){ //TODO FIXME fragile but there is no api to break down the key correctly
            this.activations_listing = msg.response;
        } else {
            this.root_listing = msg.response;
        }
        return Behaviors.same();
    }

    // for spawn only
    // componentsAT need to change the serviceKey
    private void forceUpdateListing(Address at) {
        getContext().getSystem().receptionist().tell(
            Receptionist.find(PlaceDiscovery.serviceKeyOf(at), this.receptionist_adapter)
        );
    }

    private <_T> Behavior<SpawnProtocol.Command> onSpawnAt(SpawnProtocol.SpawnAt msg) {
        assert(this.root_listing != null); //FIXME add somthing to wait if it is null
        try {
            getContext().getLog().info("onSpawnAt");
            Set<ActorRef<SpawnProtocol.Command>> roots = Set.of();
            roots = this.root_listing.getServiceInstances(PlaceDiscovery.serviceKeyOf(msg.at));
            
            roots = roots.stream()
            .filter(x -> {System.out.println("> collected path "+x.path().toString()); return msg.at.equals(Place.of_actor_ref(getContext(), x).address);})
            .collect(Collectors.toSet());

            assert (roots.size() == 1);
            ActorRef<SpawnProtocol.Command> root = null;
            for (ActorRef<SpawnProtocol.Command> _root : roots) {
                root = _root;
                break;
            }
            if (root == null)
                getContext().getLog().warn("No target system as been found at place " + msg.at.toString());
            else
                getContext().getLog().info("Target system at place [" + msg.at.toString() + "] is " + root.toString());

            if (root == getContext().getSelf()) {
                // Can not do ask with self in Akka, see
                // https://stackoverflow.com/questions/22319660/akka-2-0-send-message-to-self
                SpawnProtocol.Spawn<_T> spawn = new SpawnProtocol.Spawn(msg.runnable, msg.name, msg.props, root);
                ActorRef<_T> actorRef = applySpawn(getContext(), spawn);
                getContext().getLog().info("replying  with actorref to " + msg.replyTo.toString());
                msg.replyTo.tell(new WrappedActorRef(actorRef));
                return Behaviors.same();
            }

            ActorRef<_T> actorRef = null;
            if (root != null) {
                System.out.println("ghdghsdu");
                CompletionStage<WrappedActorRef<_T>> ask = AskPattern.ask(root,
                        replyTo -> new SpawnProtocol.Spawn(msg.runnable, msg.name, msg.props, replyTo),
                        Duration.ofSeconds(10), getContext().getSystem().scheduler());
                try {
                    actorRef = ask.toCompletableFuture().get().response; // blocking call
                } catch (Exception e) {
                    System.out.println(e);
                }
            }

            if (actorRef == null)
                getContext().getLog().warn("Remote instanciation failed");

            msg.replyTo.tell(new WrappedActorRef(actorRef));
        } catch (Exception e) {
            System.out.println(e);
        }
        return Behaviors.same();
    }

    private <_T> Behavior<SpawnProtocol.Command> onComponentsAt(SpawnProtocol.ComponentsAt msg) {
        assert(this.activations_listing != null);
        getContext().getLog().info("onComponentsAt");

        Set<ActorRef> activations = Set.of();
        activations = this.activations_listing.getServiceInstances(PlaceDiscovery.activationsServiceKeyOf(msg.at));
        activations = activations.stream()
        .filter(x -> {System.out.println("> collected path "+x.path().toString()); return msg.at.equals(Place.of_actor_ref(getContext(), x).address);})
        .collect(Collectors.toSet());

        getContext().getLog().info("replying with actorrefs "+activations.size()+" to "+msg.replyTo.toString());
        msg.replyTo.tell(new WrappedActorRefs(activations));
        return Behaviors.same();
    }

    private <_T> Behavior<SpawnProtocol.Command> onLeftActivationsOf(SpawnProtocol.LeftActivationsOf msg) {
        assert(this.activations_listing != null);
        getContext().getLog().info("onLeftActivationsOf");

        CompletionStage<Receptionist.Listing> result = AskPattern.ask(
            getContext().getSystem().receptionist(),
            (ActorRef<Receptionist.Listing> replyTo) -> Receptionist.find(Bridge.leftServiceKey(msg.bridge_id), replyTo),
            Duration.ofSeconds(10),
            getContext().getSystem().scheduler());

        try {
            Set<ActorRef<SpawnProtocol.Command>> _activations = result.toCompletableFuture().get().getServiceInstances(
                Bridge.leftServiceKey(msg.bridge_id));

            Set<ActorRef> activations = _activations.stream()
                .filter(x -> {
                    System.out.println("> collected path "+x.path().toString()); 
                    return true;
                })
                .map( x -> (ActorRef) x)
                .collect(Collectors.toSet());

            getContext().getLog().info("replying with leftactorrefs "+activations.size()+" to "+msg.replyTo.toString());

            msg.replyTo.tell(new WrappedActorRefs(activations));
        } catch (ExecutionException e) {
            throw new RuntimeException(e.getCause().toString());
        } catch (InterruptedException e) {
            throw new RuntimeException(e.toString());
        }

        return Behaviors.same();
    }

    private <_T> Behavior<SpawnProtocol.Command> onRightActivationsOf(SpawnProtocol.RightActivationsOf msg) {
        assert(this.activations_listing != null);
        getContext().getLog().info("onRightActivationsOf");

        CompletionStage<Receptionist.Listing> result = AskPattern.ask(
            getContext().getSystem().receptionist(),
            (ActorRef<Receptionist.Listing> replyTo) -> Receptionist.find(Bridge.rightServiceKey(msg.bridge_id), replyTo),
            Duration.ofSeconds(10),
            getContext().getSystem().scheduler());

        try {
            Set<ActorRef<SpawnProtocol.Command>> _activations = result.toCompletableFuture().get().getServiceInstances(
                Bridge.rightServiceKey(msg.bridge_id));

            Set<ActorRef> activations = _activations.stream()
                .filter(x -> {
                    System.out.println("> collected path "+x.path().toString()); 
                    return true;
                })
                .map( x -> (ActorRef) x)
                .collect(Collectors.toSet());

            getContext().getLog().info("replying with rightactorrefs "+activations.size()+" to "+msg.replyTo.toString());

            msg.replyTo.tell(new WrappedActorRefs(activations));
        } catch (ExecutionException e) {
            throw new RuntimeException(e.getCause().toString());
        } catch (InterruptedException e) {
            throw new RuntimeException(e.toString());
        }

        return Behaviors.same();
    }

    // Direct call from place discovery
    static public <_T> ActorRef<_T> applySpawn(ActorContext ctx, SpawnProtocol.Spawn<_T> spawn) {
        ActorRef<_T> actorRef;
        if (null == spawn.name || spawn.name.isEmpty()) {
            // anonymous spawn
            actorRef = ctx.spawnAnonymous(Behaviors.setup(spawn.runnable.apply(ctx.getSelf())::apply),
                    null == spawn.props ? Props.empty() : spawn.props);
        } else {
            actorRef = SpawnProtocol.spawnWithUniqueName(0, spawn.name, ctx, Behaviors.setup(spawn.runnable.apply(ctx.getSelf())::apply),
                    null == spawn.props ? Props.empty() : spawn.props);
        }
        return actorRef;
    }

    private <_T> Behavior<SpawnProtocol.Command> onSpawn(SpawnProtocol.Spawn _spawn) {
        try {
            getContext().getLog().info("onSpawn");
            SpawnProtocol.Spawn<_T> spawn = (SpawnProtocol.Spawn<_T>) _spawn;

            ActorRef<_T> actorRef = applySpawn(getContext(), spawn);
            if (actorRef == null)
                getContext().getLog().warn("Local instanciation failed, from remote command");

            spawn.replyTo.tell(new WrappedActorRef(actorRef));
        } catch (Exception e) {
            System.out.println(e);
        }
        return Behaviors.same();
    }

    @Override
    public Receive<SpawnProtocol.Command> createReceive() {
        return newReceiveBuilder().onMessage(SpawnProtocol.WrappedListing.class, this::onListing)
                .onMessage(SpawnProtocol.SpawnAt.class, this::onSpawnAt)
                .onMessage(SpawnProtocol.ComponentsAt.class, this::onComponentsAt)
                .onMessage(SpawnProtocol.Spawn.class, this::onSpawn)
                .onMessage(SpawnProtocol.LeftActivationsOf.class, this::onLeftActivationsOf)
                .onMessage(SpawnProtocol.RightActivationsOf.class, this::onRightActivationsOf)
                .build();
    }
}