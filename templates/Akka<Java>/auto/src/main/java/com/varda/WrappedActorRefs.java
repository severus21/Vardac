package com.varda;

import java.util.Set;

import akka.actor.typed.ActorRef;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.core.JsonGenerator;

public class WrappedActorRefs implements com.bmartin.SpawnProtocol.Command, {% if components_command != [] %}{{join(", ", components_command)}}{%else%}CborSerializable{%endif%}{
    public final Set<ActorRef> response;

    @JsonCreator
    public WrappedActorRefs(@JsonProperty("response") Set<ActorRef> response) {
        this.response = response;
    }
}