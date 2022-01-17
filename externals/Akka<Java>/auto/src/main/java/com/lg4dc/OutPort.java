package com.lg4dc;

import java.util.UUID;
import akka.actor.typed.ActorRef;
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
        this.bridge = bridge;
    }

    public String toString(){
        return "OutPort on "+this.bridge.toString();
    }

    public Session initiate_session_with(
        ActorRef from, 
        ActorRef to
    ){
        assert(from != null);
        assert(to != null);
        assert(this.bridge != null);

        return new Session(this.bridge.id, from, to, this.bridge.protocol.get_st());
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
