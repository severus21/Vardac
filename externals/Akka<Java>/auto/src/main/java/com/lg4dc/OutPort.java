package com.lg4dc;

import java.util.*;
import akka.actor.typed.javadsl.ActorContext;
import com.bmartin.*;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonAutoDetect;
import com.fasterxml.jackson.annotation.JsonAutoDetect.Visibility;
import com.fasterxml.jackson.annotation.JsonCreator;



public final class OutPort<P extends Protocol> implements CborSerializable, JsonSerializable, java.io.Serializable {
    @JsonProperty("bridge")
    Bridge<P> bridge;
    
    @JsonCreator 
    public OutPort (Bridge<P> bridge){
        assert(bridge != null);

        this.bridge = bridge;
    }

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
            hidden_to);
        return t;
    }    

    public boolean equals(Object obj) {
        if (obj == this) {
            return true;
        }

        if (!(obj instanceof Bridge)) {
            return false;
        }   

        OutPort b = (OutPort) obj;
        return this.bridge.equals(b);
    }

}
