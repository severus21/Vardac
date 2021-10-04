package com.lg4dc;

import akka.actor.typed.ActorRef;
import akka.actor.typed.Behavior;
import akka.actor.typed.javadsl.Behaviors;
import akka.actor.typed.receptionist.Receptionist;
import akka.actor.typed.receptionist.ServiceKey;
import akka.cluster.MemberStatus;
import akka.cluster.sharding.typed.ShardingEnvelope;
import akka.cluster.sharding.typed.javadsl.ClusterSharding;
import akka.cluster.sharding.typed.javadsl.Entity;
import akka.cluster.typed.Cluster;
import akka.cluster.typed.ClusterSingleton;
import akka.cluster.typed.Join;
import akka.cluster.typed.SingletonActor;

import com.bmartin.SpawnProtocol;

public abstract class AbstractSystem {
    public static final String NAME = "system_{{project_name}}";
    public static final short MAX_CLUSTER_CONNECT_RETRY = 5;
    public static final ServiceKey<SpawnProtocol.Command> SERVICE_KEY
            = ServiceKey.create(SpawnProtocol.Command.class, NAME);

    public static final class Wait {
    }
        
    public static Behavior<SpawnProtocol.Command> create() {
        return AbstractSystem.create(null, null);
    }
    public static <K, V> Behavior<SpawnProtocol.Command> create(String name, Wait wait) {
        return Behaviors.setup(context -> {
            if (null != name && !name.isEmpty()) {
                context.setLoggerName(name);
            }
            context.getLog().debug(NAME + "::create()");

            // register to receptionist
            context.getSystem().receptionist().tell(Receptionist.register(SERVICE_KEY, context.getSelf()));
        /* TODO ask Benoit
            // Cluster
            Cluster cluster = Cluster.get(context.getSystem());
            assert (null != cluster);

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

            context.getLog().debug("Joining cluster");
            cluster.manager().tell(Join.create(cluster.selfMember().address()));

            ClusterSingleton singleton = ClusterSingleton.get(context.getSystem());
        */

            if (null != wait) {
                synchronized (wait) {
                    wait.notify();
                }
            }

            return SpawnProtocol.create();
        });
    }
}
