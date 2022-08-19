package com.varda;
import java.util.UUID;
import java.util.List;
import java.util.ArrayList;

import io.vavr.*;

import akka.actor.typed.javadsl.ActorContext;

import com.bmartin.*;
import com.fasterxml.jackson.annotation.JsonAutoDetect;
import com.fasterxml.jackson.annotation.JsonAutoDetect.Visibility;

@JsonAutoDetect(fieldVisibility = Visibility.ANY)
public class Protocol implements CborSerializable, java.io.Serializable {
    public ASTStype.Base get_st(){
        return new ASTStype.End();
    }
}