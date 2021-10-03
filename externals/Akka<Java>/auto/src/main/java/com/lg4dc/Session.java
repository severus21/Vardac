package com.lg4dc;

import java.util.UUID;
import akka.actor.typed.ActorRef;
import akka.actor.typed.javadsl.ActorContext;
import com.bmartin.*;

public class Session {
    UUID bridge_id;
    ActorRef<?> right;
    UUID session_id;
    ASTStype.Base st;

    public Session(UUID bridge_id, ActorRef<?> right, ASTStype.Base st) {
        this.bridge_id = bridge_id;
        this.session_id = UUID.randomUUID();
        this.right = right;

        this.st = st;
    }
    
    public <Msg extends CborSerializable> Session fire(Msg msg,  ActorContext context) {
        assert(this.st.continuations.size() == 1);
        
        Event<Msg> e = new Event(this.bridge_id,  this.session_id,  this.st,  msg);
        this.right.tell(e, context.getSelf());

        this.st = this.st.continuations.get(1)._2;
        return this;
    }
    
    /*public Tuple2<Object, Session> receive(ActorContext context) {
        assert(this.st.continuations.size() == 1);
    
        Duration timeout = Duration.ofSeconds(3);
        CompletableFuture<Event<Object>> future = 
        ask(this.right,  msg,  timeout).
        toCompletableFuture();
        Event<Object> e = future.join(3);

        this.st = this.st.continuations.get(1)._2;
        return new Tuple2(e.msg,  this);
    }*/
}