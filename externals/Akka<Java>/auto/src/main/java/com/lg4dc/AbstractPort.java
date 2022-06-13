package com.lg4dc;

import java.util.*;
import akka.actor.typed.javadsl.ActorContext;
import com.bmartin.*;

public abstract class AbstractPort {
    public UUID id;
    public String name;
    public Bridge bridge;

    // child is an other port such that 
    // its bindings follows those of its parents
    // this mechanism is mainly used for Varda generated ports (RecvElim for instance)
    // since program can dinamically change the bindings of a port
    List<AbstractPort> children = new LinkedList<>();
    
    public AbstractPort (String name){
        assert(name != null);
        this.name = name;
        this.id = UUID.randomUUID();
    }

    public AbstractPort (Bridge bridge){
        this.bind(bridge);
    }

    public UUID get_binded_bridge_id(){
        assert(this.bridge != null);

        return this.bridge.id;
    }

    public Void bind(Bridge bridge){
        assert(bridge != null);
        this.bridge = bridge;

        Iterator<AbstractPort> iterator = this.children.iterator();
        while(iterator.hasNext()) {
            iterator.next().bind(bridge);
        }

        return null;
    }

    public String toString(){
        return "AbstractPort on "+this.bridge.toString();
    }

    public boolean equals(Object obj) {
        if (obj == this) {
            return true;
        }

        if (!(obj instanceof AbstractPort)) {
            return false;
        }   

        AbstractPort b = (AbstractPort) obj;
        return this.bridge.equals(b);
    }
}
