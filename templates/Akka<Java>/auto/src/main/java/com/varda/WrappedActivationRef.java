package com.varda;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.core.JsonGenerator;

public class WrappedActivationRef<_T> implements com.bmartin.SpawnProtocol.Command, {% if components_command != [] %}{{join(", ", components_command)}}{%else%}CborSerializable{%endif%}{
    public final ActivationRef<_T> response;

    @JsonCreator
    public WrappedActivationRef(@JsonProperty("response") ActivationRef<_T> response) {
      this.response = response;
    }
}