package com.lg4dc;

import java.util.UUID;
import akka.actor.typed.ActorRef;
import akka.actor.typed.javadsl.ActorContext;
import com.bmartin.*;

public final class Bridge<P extends PProtocol> {
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

    public Session init_session_with(ActorRef<?> right){
        return new Session(this.id, right, this.protocol.st);
    }    
}