package com.lg4dc;

import java.util.Set;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.core.JsonGenerator;

public class WrappedActivationRefs implements com.bmartin.SpawnProtocol.Command, {% if components_command != [] %}{{join(", ", components_command)}}{%else%}CborSerializable{%endif%}{
    public final Set<ActivationRef> response;

    @JsonCreator
    public WrappedActivationRefs(@JsonProperty("response") Set<ActivationRef> response) {
        this.response = response;
    }
}