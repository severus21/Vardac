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

public abstract class AbstractComponent<T> extends AbstractBehavior<T> {
    public static final String NAME = "system_{{project_name}}_component_";
    public static final ServiceKey SERVICE_KEY
            = ServiceKey.create(String.class, NAME);

    public AbstractComponent(ActorContext<T> context){
        super(context);
        
        // Cluster
        Cluster cluster = Cluster.get(context.getSystem());
        assert (null != cluster);

        // register to receptionist to allow reflexivity : componentsat place -> actoref list
        context.getSystem().receptionist().tell(Receptionist.register(PlaceDiscovery.activationsServiceKeyOf(cluster.selfMember().address()), context.getSelf()));
    }
}
