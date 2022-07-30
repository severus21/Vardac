package com.lg4dc;

import java.util.*;
import java.util.function.BiFunction;
import akka.actor.typed.javadsl.ActorContext;
import com.bmartin.*;


// FIXME a port should be serializable
public final class InPort<A, B, C> extends AbstractPort {
    public ASTStype.Base expecting_st;
    boolean is_intermediate = false;
    BiFunction<A,B,C> callback;
    public Optional<String> receive_id;

    /*
      if(this.registered_session.containsKey(e.session_id)){
         InPort p = this.registered_session.get(e.session_id);
         getContext().getLog().info(">>>> find intermediate for "+e.session_id.toString());
      }
     */

    public InPort (String name, List<AbstractPort> children, boolean is_intermediate, ASTStype.Base expecting_st, Optional<String> receive_id){
        super(name);

        assert( expecting_st != null);
        assert( children != null);
        this.expecting_st = expecting_st;
        this.children = children;
        this.is_intermediate = is_intermediate;
        this.receive_id = receive_id;
    }

    @Override
    public String toString(){
        return "InPort {name="+this.id.toString()+"; id="+this.name+"} [expecting_st="+this.expecting_st.toString()+"] binded with ["+this.bridge.id+"]";
    }

    public void setCallback(BiFunction<A,B,C> callback){
        assert(callback != null);
        this.callback = callback;
    }

    public BiFunction<A,B,C> getCallback(){
        return this.callback;
    }

    public ASTStype.Base remaining_st() {
        if( this.expecting_st.continuations.size() == 0 ){
            return this.expecting_st;
        } else if ( this.expecting_st.continuations.size() == 1 ){
            return this.expecting_st.continuations.get(0)._3;
        } else {
            assert(false);
            return null;
        }
    }
}