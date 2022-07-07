package com.lg4dc;

import java.util.*;
import akka.actor.typed.javadsl.ActorContext;
import com.bmartin.*;

// FIXME a port should be serializable
public final class OutPort extends AbstractPort {
    public OutPort (String name, List<AbstractPort> children){
        super(name);

        assert( children != null);
        this.children = children;
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
        assert(from != null);
        assert(to != null);
        assert(this.bridge != null);
        assert(this.bridge.id != null);
        assert(this.bridge.protocol != null);
        assert(this.bridge.protocol.get_st() != null);
        assert(hidden_to != null);

        if(hidden_to.isEmpty() && to.interceptedActivationRef_opt.isPresent())
            hidden_to = to.interceptedActivationRef_opt;

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
