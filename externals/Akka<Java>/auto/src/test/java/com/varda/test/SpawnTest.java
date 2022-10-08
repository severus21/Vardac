package com.varda.test;

import akka.actor.testkit.typed.javadsl.ActorTestKit;
import akka.actor.typed.ActorRef;
import akka.actor.typed.Behavior;
import akka.actor.typed.javadsl.ActorContext;
import akka.actor.typed.javadsl.Behaviors;
import akka.actor.typed.javadsl.TimerScheduler;
import akka.serialization.*;
import com.bmartin.*;
import com.varda.*;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

public class SpawnTest {
    ActorTestKit testKit = null;

    static private class TestSystem extends AbstractSystem{
        public TestSystem( ActorContext<
            SpawnProtocol.Command> context, 
            TimerScheduler<SpawnProtocol.Command> timers, 
            String name) {
   
            super(context,  timers,  name,  null);
        }

        static public Behavior<SpawnProtocol.Command> create(String name) {
            return Behaviors.setup( (context) -> { 
                return Behaviors.withTimers(
                    (timers) -> new TestSystem( context, timers, name)
                );
            });
        }

    }



    @BeforeEach
    public void setup() {
        testKit = ActorTestKit.create("name");
    }

    @AfterEach
    public void teardown() {
        testKit.shutdownTestKit();
    }

    @Test
    public void akka_serializes_Protocol() {

        ActorRef kvs1 = testKit.spawn(TestSystem.create("TOTO"), "KVS1");

        // Get the Serialization Extension
        Serialization serialization = SerializationExtension.get(testKit.system());

        // Have something to serialize
        SpawnProtocol.SerializableRunnable<SpawnProtocol.Command> run = (guardian) -> {
                return (ActorContext<SpawnProtocol.Command> ctx) -> {
                    return Behaviors.withTimers(timers -> { return null; });
                };
        };

        SpawnProtocol.Command original = new SpawnProtocol.Spawn(
            (SpawnProtocol.SerializableRunnable) run,
            "name", 
            null, 
            kvs1);
        // Turn it into bytes, and retrieve the serializerId and manifest, which are needed for
        // deserialization
        byte[] bytes = serialization.serialize(original).get();
        int serializerId = serialization.findSerializerFor(original).identifier();
        String manifest = Serializers.manifestFor(serialization.findSerializerFor(original), original);

        // Turn it back into an object
        SpawnProtocol.Spawn back = (SpawnProtocol.Spawn) serialization.deserialize(bytes, serializerId, manifest).get();
        //System.out.println(original.toString()+ "\n\n"+ back.toString());
        //assert(back.equals(original));
        //assertEquals(original, back);
    }
}
