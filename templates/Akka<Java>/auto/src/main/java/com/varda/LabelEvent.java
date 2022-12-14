package com.varda;

import akka.actor.typed.ActorRef;
import java.util.UUID;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;

import com.varda.metadata.*;

public class LabelEvent<T extends NoMetadata> extends Event<T> implements {% if components_command != [] %}{{join(", ", components_command)}}{%else%}CborSerializable{%endif%} {
    // protocol label - used for select and branch
    final public String value;

    @JsonCreator 
    public LabelEvent (@JsonProperty("value") String value){
        this.value = value;
    }

    public String _0_() {
        return this.value;
    }
    
    @Override
    public String toString(){
        return this.value+"{|"+super.toString()+"|}";
    }

    public boolean equals(Object obj){
        if (obj == null)
            return false;
        else if (obj == this)
            return true;
        else if( !(obj instanceof LabelEvent))
            return false;
        else {
            LabelEvent b = (LabelEvent) obj;
            return this.value.equals(b.value);
        }
    }
}
