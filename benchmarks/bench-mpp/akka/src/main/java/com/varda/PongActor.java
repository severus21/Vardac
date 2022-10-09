package com.varda;

import akka.actor.typed.Behavior;
import akka.actor.typed.javadsl.*;
import akka.actor.typed.receptionist.Receptionist;
import akka.actor.typed.receptionist.ServiceKey;

public class PongActor extends AbstractBehavior<PongActor.Command> {
	public interface Command extends CborSerializable {}

    public static final ServiceKey<PongActor.Command> PONG_ACTOR_SERVICE_KEY =
      ServiceKey.create(PongActor.Command.class, "pongActor");

    public PongActor(ActorContext<Command> context) {
        super(context);
    }

    static public Behavior<Command> create() {
        return Behaviors.setup(context -> {
            context
                .getSystem()
                .receptionist()
                .tell(Receptionist.register(PONG_ACTOR_SERVICE_KEY, context.getSelf())); 
            return new PongActor(context);
        });
    }
    
    @Override
    public Receive<Command> createReceive() {
        return newReceiveBuilder()
            .onMessage(Ping.class, this::onPing)
            .build();
    }
    
    private Behavior<Command> onPing(Ping ping) {
        ping.replyTo.tell(new Pong(ping.i, ping.warmup, ping.payload, getContext().getSelf(), ping.initTimestamp));
        return Behaviors.same();
    }
}
