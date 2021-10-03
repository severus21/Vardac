package com.lg4dc;

import java.util.UUID;
import com.bmartin.*;

public class Event<Msg extends CborSerializable> implements CborSerializable {
    public UUID bridge_id;
    public UUID session_id;
    public ASTStype.Base st;
    public Msg msg;
    
    public Event(UUID bridge_id, UUID session_id, ASTStype.Base st, Msg msg) {
        this.bridge_id = bridge_id;
        this.session_id = session_id;
        this.st = st;
        this.msg = msg;
    }
}
