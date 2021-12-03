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



public final class Bridge<P extends Protocol> implements CborSerializable, JsonSerializable, java.io.Serializable {
    @JsonProperty("id")
    UUID id;
    
    //private Supplier<P> protocol_supplier;
    @JsonProperty("protocol")
    P protocol;

    @JsonCreator 
    public Bridge (P protocol, UUID id){
        this.id = id;
        this.protocol = protocol;
    }

    public Bridge (P protocol){ //Supplier<P> protocol_supplier) {
        this.id = UUID.randomUUID();

        //this.protocol_supplier = protocol_supplier;
        //this.protocol = protocol_supplier.get();
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

    public Session initiate_session_with(
        ActorRef<CborSerializable> from, 
        ActorRef<CborSerializable> to
    ){
        return new Session(this.id, from, to, this.protocol.get_st());
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