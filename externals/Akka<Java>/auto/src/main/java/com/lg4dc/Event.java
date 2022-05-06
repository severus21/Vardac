package com.lg4dc;

import akka.actor.typed.ActorRef;
import java.util.*;

import com.bmartin.*;
import com.lg4dc.ActivationRef;

public class Event<T extends NoMetadata> implements CborSerializable {
    public UUID bridge_id;
    public UUID session_id;
    public ActivationRef<CborSerializable> replyTo;

    // Mandatory metadata
    public ASTStype.Base st;
    public Boolean init_stage;
    public Optional<ActivationRef> hidden_right; //e.g. non anonymous redirection

    public NoMetadata metadata;
    
    public Event() {}

    public void hydrate(
        UUID bridge_id, 
        UUID session_id, 
        ActivationRef<CborSerializable> replyTo, 
        ASTStype.Base st, 
        Boolean init_stage,
        Optional<ActivationRef> hidden_right,
        NoMetadata metadata
    ) {
        assert(bridge_id != null);
        assert(session_id != null);
        assert(replyTo != null);
        assert(st != null);
        assert(init_stage != null);
        assert(hidden_right != null);
        assert(metadata != null);
        this.bridge_id = bridge_id;
        this.session_id = session_id;
        this.replyTo = replyTo;
        this.st = st;
        this.init_stage = init_stage;
        this.hidden_right = hidden_right;
        this.metadata = metadata;
    }

    public String toString(){
        return this.getClass().toString()+"<bridgeId="+this.bridge_id+"; session_id="+this.session_id+"; st="+this.st.getClass().toString()+"|"+this.st.toString()+", init-stage="+this.init_stage.toString()+"; hidden_right="+this.hidden_right.toString()+">";
    }
}