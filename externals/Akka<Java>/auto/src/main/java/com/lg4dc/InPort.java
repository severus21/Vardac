package com.lg4dc;

import java.util.*;
import akka.actor.typed.javadsl.ActorContext;
import com.bmartin.*;


// FIXME a port should be serializable
public final class InPort<P extends Protocol> extends AbstractPort<P> {
    public ASTStype.Base expecting_st;
    boolean is_intermediate = false;

    public InPort (List<AbstractPort> children, boolean is_intermediate, ASTStype.Base expecting_st){
        super();

        assert( expecting_st != null);
        assert( children != null);
        this.expecting_st = expecting_st;
        this.children = children;
        this.is_intermediate = is_intermediate;
    }

    @Override
    public String toString(){
        return "InPort on "+this.bridge.toString();
    }
}