package com.varda;

import java.util.*;

// FIXME a port should be serializable
public final class OutPort extends AbstractPort {
    public ASTStype.Base expecting_st;

    public OutPort (String name, List<AbstractPort> children, ASTStype.Base expecting_st){
        super(name);

        assert( children != null);
        assert( expecting_st != null);
        this.children = children;
        this.expecting_st = expecting_st;
    }

    @Override
    public String toString(){
        return "OutPort on "+this.bridge.toString();
    }

    public Session initiate_session_with(
        ActivationRef from, 
        ActivationRef to,
        SerializableOptional<ActivationRef> hidden_to
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
