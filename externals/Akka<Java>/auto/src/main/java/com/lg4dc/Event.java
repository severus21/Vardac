package com.lg4dc;

import akka.actor.typed.ActorRef;
import java.util.UUID;
import com.bmartin.*;

public class Event<T extends NoMetadata> implements CborSerializable {
    public UUID bridge_id;
    public UUID session_id;
    public ActorRef<CborSerializable> replyTo;
    public ASTStype.Base st;
    public NoMetadata metadata;
    
    public Event() {}

    public void hydrate(
        UUID bridge_id, 
        UUID session_id, 
        ActorRef<CborSerializable> replyTo, 
        ASTStype.Base st, 
        NoMetadata metadata
    ) {
        this.bridge_id = bridge_id;
        this.session_id = session_id;
        this.replyTo = replyTo;
        this.st = st;
        this.metadata = metadata;
    }

    public String toString(){
        return this.getClass().toString()+"<bridgeId="+this.bridge_id+"; session_id="+this.session_id+"; st="+this.st.getClass().toString()+"|"+this.st.toString()+">";
    }
}