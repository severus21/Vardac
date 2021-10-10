package com.lg4dc;

import java.util.UUID;
import akka.actor.typed.ActorRef;
import akka.actor.typed.javadsl.ActorContext;
import com.bmartin.*;

public class Session {
    UUID bridge_id;
    ActorRef<CborSerializable> left;
    ActorRef<CborSerializable> right;
    UUID session_id;
    ASTStype.Base st;

    public Session(
        UUID bridge_id,
        ActorRef<CborSerializable> left,
        ActorRef<CborSerializable> right, 
        ASTStype.Base st) {
        this.bridge_id = bridge_id;
        this.session_id = UUID.randomUUID();
        this.left = left;
        this.right = right;

        this.st = st;
    }
    
    public void set_id(UUID id){
        this.session_id = id;
    }

    public UUID get_id(){
        return this.session_id;
    }
    
    public <E extends Event> Session fire(E e,  ActorContext context) {
        assert(this.st.continuations.size() == 1);

        e.hydrate(this.bridge_id,  this.session_id,  this.left, this.st, new NoMetadata());
        this.right.tell(e);

        this.st = this.st.continuations.get(0)._3;
        return this;
    }
}