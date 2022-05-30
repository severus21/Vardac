package com.lg4dc;

import java.util.*;
import akka.actor.typed.javadsl.ActorContext;
import com.bmartin.*;

public final class EPort<T> {
    public T expecting_mt;

    public EPort (T expecting_mt){
        assert(expecting_mt != null);
        this.expecting_mt = expecting_mt;
    }

    public match(Object obj){
        if (!(obj instanceof this.expecting_mt)) {
            return true;
        }   
        return false;
    }

    @Override
    public String toString(){
        return "InPort on "+this.expecting_mt.toString();
    }
}