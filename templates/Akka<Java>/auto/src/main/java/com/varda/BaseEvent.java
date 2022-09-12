package com.varda;

import akka.actor.typed.ActorRef;
import java.util.UUID;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;

import com.varda.metadata.*;

public class BaseEvent implements {% if components_command != [] %}{{join(", ", components_command)}}{%else%}CborSerializable{%endif%} {
    
}
