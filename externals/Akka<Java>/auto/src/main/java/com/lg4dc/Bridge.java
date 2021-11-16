package com.lg4dc;

import java.util.UUID;
import akka.actor.typed.ActorRef;
import akka.actor.typed.javadsl.ActorContext;
import com.bmartin.*;

public final class Bridge<P extends Protocol> implements CborSerializable {
    UUID id;
    
    //private Supplier<P> protocol_supplier;
    P protocol;
    
    public Bridge (P protocol){ //Supplier<P> protocol_supplier) {
        this.id = UUID.randomUUID();

        //this.protocol_supplier = protocol_supplier;
        //this.protocol = protocol_supplier.get();
        this.protocol = protocol;
    }

    public UUID get_id(){
        return this.id;
    }

    public Session initiate_session_with(
        ActorRef<CborSerializable> from, 
        ActorRef<CborSerializable> to
    ){
        return new Session(this.id, from, to, this.protocol.get_st());
    }    
}