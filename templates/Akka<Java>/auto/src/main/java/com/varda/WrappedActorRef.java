package com.varda;

import akka.actor.typed.ActorRef;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.core.JsonGenerator;

public class WrappedActorRef<_T> implements com.bmartin.SpawnProtocol.Command, {% if components_command != [] %}{{join(", ", components_command)}}{%else%}CborSerializable{%endif%}{
    public final ActorRef<_T> response;

    @JsonCreator
    public WrappedActorRef(@JsonProperty("response") ActorRef<_T> response) {
      assert(response != null);
      this.response = response;
    }
}