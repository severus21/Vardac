package com.lg4dc;

import java.util.*;
import akka.actor.typed.javadsl.ActorContext;
import com.bmartin.*;


// FIXME a port should be serializable
public final class InPort<P extends Protocol> extends AbstractPort<P> {
    public ASTStype.Base expecting_st;

    public InPort (ASTStype.Base expecting_st){
        super();

        assert( expecting_st != null);
        this.expecting_st = expecting_st;
    }

    @Override
    public String toString(){
        return "InPort on "+this.bridge.toString();
    }
}