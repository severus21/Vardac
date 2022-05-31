package com.lg4dc;

import java.util.*;
import akka.actor.typed.javadsl.ActorContext;
import com.bmartin.*;

public final class EPort<T> {
    public T empty;
    public Class<T> expecting_mt;

    public EPort (Class<T> expecting_mt){
        assert(expecting_mt != null);
        this.expecting_mt = expecting_mt;
    }

    public boolean match(Object obj){
        if (expecting_mt.isAssignableFrom(obj.getClass())) {
            return true;
        }   
        return false;
    }

    @Override
    public String toString(){
        return "InPort on "+this.expecting_mt.toString();
    }
}