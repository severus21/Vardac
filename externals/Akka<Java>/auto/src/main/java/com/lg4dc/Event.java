package com.lg4dc;

import akka.actor.typed.ActorRef;
import java.util.UUID;
import com.bmartin.*;

public class Event<T extends AbstractMetadata> implements CborSerializable {
    public UUID bridge_id;
    public UUID session_id;
    public ActorRef<CborSerializable> replyTo;
    public ASTStype.Base st;
    public AbstractMetadata metadata;
    
    public Event() {}

    public void hydrate(
        UUID bridge_id, 
        UUID session_id, 
        ActorRef<CborSerializable> replyTo, 
        ASTStype.Base st, 
        AbstractMetadata metadata
    ) {
        this.bridge_id = bridge_id;
        this.session_id = session_id;
        this.replyTo = replyTo;
        this.st = st;
        this.metadata = metadata;
    }
}