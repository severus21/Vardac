package com.lg4dc;

import java.util.Set;

import akka.actor.typed.ActorRef;

public class WrappedActorRefs implements com.bmartin.SpawnProtocol.Command, {% if components_command != [] %}{{join(", ", components_command)}}{%else%}CborSerializable{%endif%}{
    public final Set<ActorRef> response;

    public WrappedActorRefs(Set<ActorRef> response) {
      this.response = response;
    }
}