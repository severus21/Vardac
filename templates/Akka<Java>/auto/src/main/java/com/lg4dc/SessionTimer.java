package com.lg4dc.timers;

import java.time.Duration;
import java.util.UUID;

import com.bmartin.CborSerializable;
import com.lg4dc.ASTStype;
import com.lg4dc.ActivationRef;

public abstract class SessionTimer implements {% if components_command != [] %}{{join(", ", components_command)}}{%else%}CborSerializable{%endif%} {
    public UUID session_id;
    public ActivationRef<CborSerializable> replyTo;

    public SessionTimer (UUID session_id, ActivationRef<CborSerializable> replyTo){
        this.session_id = session_id;
        this.replyTo = replyTo;
    }
}