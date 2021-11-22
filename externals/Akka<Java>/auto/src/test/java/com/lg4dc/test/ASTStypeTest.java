package com.lg4dc.test;

import com.lg4dc.ASTStype;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.core.JsonProcessingException;
import java.io.IOException;

import akka.actor.*;
import akka.serialization.*;

import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;

import static org.junit.jupiter.api.Assertions.*;
import akka.testkit.javadsl.TestKit;

public class ASTStypeTest {
        ActorSystem system;

    @BeforeEach
    public void setup() {
        system = ActorSystem.create();
    }

    @AfterEach
    public void teardown() {
        TestKit.shutdownActorSystem(system);
        system = null;
    }

    @Test
    public void akka_serializes_End() {
        // Get the Serialization Extension
        Serialization serialization = SerializationExtension.get(system);

        // Have something to serialize
        ASTStype.End original = new ASTStype.End();

        // Turn it into bytes, and retrieve the serializerId and manifest, which are needed for
        // deserialization
        byte[] bytes = serialization.serialize(original).get();
        int serializerId = serialization.findSerializerFor(original).identifier();
        String manifest = Serializers.manifestFor(serialization.findSerializerFor(original), original);

        // Turn it back into an object
        ASTStype.End back = (ASTStype.End) serialization.deserialize(bytes, serializerId, manifest).get();
        System.out.println(original.toString()+ "\n\n"+ back.toString());
        assert(back.equals(original));
        //assertEquals(original, back);
    }
}
