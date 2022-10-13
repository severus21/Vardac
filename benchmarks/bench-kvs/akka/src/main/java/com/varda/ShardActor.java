package com.varda;

import akka.actor.typed.Behavior;
import akka.actor.typed.javadsl.*;
import akka.actor.typed.receptionist.Receptionist;
import akka.actor.typed.receptionist.ServiceKey;

import com.varda.KVCommand.*;
import com.varda.KVCommand.GetRequest;
import com.varda.KVCommand.GetResult;
import com.varda.KVCommand.PutResult;

import java.util.*;

public class ShardActor extends AbstractBehavior<KVCommand.Command> {
    Map<String, String> inner_state = new HashMap<String, String>();

    String inner_get(String key){
        String tmp = this.inner_state.get(key);
        if( tmp == null)
            return "Error";
        else
            return tmp;
    }

    Boolean inner_put(String key, String value){
        this.inner_state.put(key, value);
        return true;
    }

    public static final ServiceKey<KVCommand.Command> KVShard_ACTOR_SERVICE_KEY =
      ServiceKey.create(KVCommand.Command.class, "pongActor");

    public ShardActor(ActorContext<Command> context) {
        super(context);
    }

    static public Behavior<Command> create() {
        return Behaviors.setup(context -> {
            context
                .getSystem()
                .receptionist()
                .tell(Receptionist.register(KVShard_ACTOR_SERVICE_KEY, context.getSelf())); 
            return new ShardActor(context);
        });
    }
    
    @Override
    public Receive<Command> createReceive() {
        return newReceiveBuilder()
            .onMessage(GetRequest.class, this::onGetRequest)
            .onMessage(PutRequest.class, this::onPutRequest)
            .build();
    }
    
    private Behavior<Command> onGetRequest(GetRequest msg) {
        System.out.println("processing get at "+this.getContext().getSelf().path());
        String value = this.inner_get(msg.key);
        msg.replyTo.tell(new KVCommand.GetResult(msg.key, value, getContext().getSelf(), msg.initTimestamp));
        return Behaviors.same();
    }

    private Behavior<Command> onPutRequest(PutRequest msg) {
        Boolean flag = this.inner_put(msg.key, msg.value);
        msg.replyTo.tell(new PutResult(msg.key, flag, getContext().getSelf(), msg.initTimestamp));
        return Behaviors.same();
    }
}
