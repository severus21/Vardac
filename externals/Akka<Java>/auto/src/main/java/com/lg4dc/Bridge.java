package com.lg4dc;

import java.util.UUID;
import akka.actor.typed.javadsl.ActorContext;
import com.bmartin.*;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonAutoDetect;
import com.fasterxml.jackson.annotation.JsonAutoDetect.Visibility;
import com.fasterxml.jackson.annotation.JsonCreator;



public final class Bridge<P extends Protocol> implements CborSerializable, JsonSerializable, java.io.Serializable {
    @JsonProperty("id")
    UUID id;
    
    @JsonProperty("protocol")
    P protocol;

    @JsonCreator 
    public Bridge (P protocol, UUID id){
        this.id = id;
        this.protocol = protocol;
    }

    public Bridge (P protocol){
        this.id = UUID.randomUUID();

        this.protocol = protocol;
    }
    
    //Same key => Same id
    public Bridge (P protocol, String key){
        this.id = UUIDCreator.getSha1Uuid(key);
        this.protocol = protocol;
    }

    public UUID get_id(){
        return this.id;
    }
    public String toString(){
        return "Bridge<UUID="+this.id+">";
    }

    public boolean equals(Object obj) {
        if (obj == this) {
            return true;
        }

        if (!(obj instanceof Bridge)) {
            return false;
        }   

        Bridge b = (Bridge) obj;
        return this.id.equals(b.id) && this.protocol.equals(b.protocol);
    }

}