package com.lg4dc;

import akka.actor.typed.ActorRef;

public class WrappedActorRef<_T> implements {% if components_command != [] %}{{join(", ", components_command)}}{%else%}CborSerializable{%endif%}{
    public final ActorRef<_T> response;

    public WrappedActorRef(ActorRef<_T> response) {
      this.response = response;
    }
}