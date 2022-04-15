package com.lg4dc;

import java.util.*;
import akka.actor.typed.javadsl.ActorContext;
import com.bmartin.*;

// FIXME a port should be serializable
public final class OutPort<P extends Protocol> extends AbstractPort<P> {
    public OutPort (){
        super();
    }

    @Override
    public String toString(){
        return "OutPort on "+this.bridge.toString();
    }

    public Session initiate_session_with(
        ActivationRef from, 
        ActivationRef to,
        Optional<ActivationRef> hidden_to
    ){
        assert(null!=null);
        assert(from != null);
        assert(to != null);
        assert(this.bridge != null);
        assert(this.bridge.id != null);
        assert(this.bridge.protocol != null);
        assert(this.bridge.protocol.get_st() != null);
        assert(hidden_to != null);

        Session t = new Session(
            this.bridge.id, 
            from, 
            to, 
            this.bridge.protocol.get_st(), 
            true,
            hidden_to,
            this);
        return t;
    }    
}
