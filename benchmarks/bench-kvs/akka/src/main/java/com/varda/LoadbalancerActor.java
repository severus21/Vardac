package com.varda;

import akka.actor.typed.ActorRef;
import akka.actor.typed.Behavior;
import akka.actor.typed.javadsl.*;
import akka.actor.typed.receptionist.Receptionist;
import akka.actor.typed.receptionist.ServiceKey;
import com.varda.KVCommand.DeleteRequest;
import com.varda.KVCommand.GetRequest;
import com.varda.KVCommand.PutRequest;
import com.varda.KVCommand.RegisterShardRequest;
import com.varda.KVCommand.RegisterShardResponse;
import java.util.*;

public class LoadbalancerActor extends AbstractBehavior<KVCommand.Command> {
    List<ActorRef<KVCommand.Command>> shards = new ArrayList();

    public static final ServiceKey<KVCommand.Command> KVShard_ACTOR_SERVICE_KEY =
      ServiceKey.create(KVCommand.Command.class, "pongActor");

    public LoadbalancerActor(ActorContext<KVCommand.Command> context) {
        super(context);
    }

    static public Behavior<KVCommand.Command> create() {
        return Behaviors.setup(context -> {
            context
                .getSystem()
                .receptionist()
                .tell(Receptionist.register(KVShard_ACTOR_SERVICE_KEY, context.getSelf())); 
            return new LoadbalancerActor(context);
        });
    }
    
    @Override
    public Receive<KVCommand.Command> createReceive() {
        return newReceiveBuilder()
            .onMessage(RegisterShardRequest.class, this::onRegisterShard)
            .onMessage(GetRequest.class, this::onGetRequest)
            .onMessage(DeleteRequest.class, this::onDeleteRequest)
            .onMessage(PutRequest.class, this::onPutRequest)
            .build();
    }

    private Behavior<KVCommand.Command> onRegisterShard(RegisterShardRequest msg){
        System.out.println("Registering shard");
        this.shards.add(msg.shard);
        msg.replyTo.tell(new RegisterShardResponse());
        return Behaviors.same();
    }
    
    private Behavior<KVCommand.Command> onGetRequest(GetRequest msg) {
        Random rand = new Random();
        ActorRef<KVCommand.Command> shard = this.shards.get(rand.nextInt(this.shards.size()));

        // Transmit request, direct response
        shard.tell(msg);
        return Behaviors.same();
    }

    private Behavior<KVCommand.Command> onDeleteRequest(DeleteRequest msg) {
        for(ActorRef<KVCommand.Command> shard : this.shards){
            shard.tell(msg);
        }
        return Behaviors.same();
    }

    private Behavior<KVCommand.Command> onPutRequest(PutRequest msg) {
        for(ActorRef<KVCommand.Command> shard : this.shards){
            shard.tell(msg);
        }
        return Behaviors.same();
    }
}
