package com.varda;

import akka.actor.typed.ActorRef;
import akka.actor.typed.Behavior;
import akka.actor.typed.Props;
import akka.actor.typed.javadsl.AbstractBehavior;
import akka.actor.typed.javadsl.ActorContext;
import akka.actor.typed.javadsl.AskPattern;
import akka.actor.typed.javadsl.Behaviors;
import akka.actor.typed.javadsl.Receive;
import akka.actor.typed.javadsl.TimerScheduler;
import akka.cluster.MemberStatus;
import akka.cluster.typed.Cluster;
import akka.cluster.typed.Join;
import com.bmartin.SpawnProtocol;
import java.time.Duration;
import java.util.concurrent.CompletionStage;

public abstract class AbstractSystem extends AbstractBehavior<SpawnProtocol.Command> {
    public static final String NAME = "{{system_name}}";
    public static final short MAX_CLUSTER_CONNECT_RETRY = 5;

    public TimerScheduler<SpawnProtocol.Command> timers;
    public ActorRef<SpawnProtocol.Command> guardian;

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
        PlaceDiscovery.register_jvm(context, cluster.selfMember().address(), getContext().getSelf());
        context.getLog().warn("register "+getContext().getSelf()+" at "+cluster.selfMember().address());
    }

    public static final class Wait {
    }

    private <_T> Behavior<SpawnProtocol.Command> onSpawnAt(SpawnProtocol.SpawnAt msg) {
        try {
            getContext().getLog().info("onSpawnAt");
            ActorRef<SpawnProtocol.Command> root = PlaceDiscovery.jvmAt(getContext().getSystem(), msg.at);
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

            if (root != null) {
                getContext().getLog().warn("Start remote instantiation");
                ActorRef<_T> actorRef = null;
                CompletionStage<WrappedActorRef<_T>> ask = AskPattern.ask(root,
                        replyTo -> new SpawnProtocol.Spawn(msg.runnable, msg.name, msg.props, replyTo),
                        Duration.ofSeconds(10), getContext().getSystem().scheduler());
                try {
                    actorRef = ask.toCompletableFuture().get().response; // blocking call
                } catch (Exception e) {
                    System.out.println(e);
                }

                if (actorRef == null){
                    getContext().getLog().error("Remote instantiation failed");
                    return Behaviors.same();
                }

                getContext().getLog().info("Remote instantiation Ok "+actorRef.toString());
                msg.replyTo.tell(new WrappedActorRef(actorRef));
                return Behaviors.same();
            }

        } catch (Exception e) {
            System.out.println(e);
        }
        getContext().getLog().warn("Instantiation failed");
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
        return newReceiveBuilder()
            .onMessage(SpawnProtocol.SpawnAt.class, this::onSpawnAt)
            .onMessage(SpawnProtocol.Spawn.class, this::onSpawn)
            .build();
    }
}