package com.varda.objectsize;
import java.lang.instrument.Instrumentation;

import akka.serialization.*;
import akka.actor.typed.ActorSystem;

public class InstrumentationAgent {
    private static volatile Instrumentation globalInstrumentation;

    public static void premain(final String agentArgs, final Instrumentation inst) {
        globalInstrumentation = inst;
    }

    public static long getObjectSize(final Object object) {
        if (globalInstrumentation == null) {
            throw new IllegalStateException("Agent not initialized.");
        }
        return globalInstrumentation.getObjectSize(object);
    }

    public static long getSerializedSize(ActorSystem system, Object object){
        // Get the Serialization Extension
        Serialization serialization = SerializationExtension.get(system);

        // Turn it into bytes, and retrieve the serializerId and manifest, which are needed for
        // deserialization
        byte[] bytes = serialization.serialize(object).get();
        return globalInstrumentation.getObjectSize(bytes);
    }
}
