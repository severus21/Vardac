package com.bmartin;

import akka.actor.typed.ActorRef;
import akka.actor.typed.Behavior;
import akka.actor.typed.Props;
import akka.actor.typed.javadsl.ActorContext;
import akka.actor.typed.javadsl.Behaviors;
import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonDeserializer;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.SerializerProvider;
import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import scala.MatchError;

import java.io.*;
import java.util.Optional;
import java.util.function.Function;

public class SpawnProtocol {

    @JsonSerialize(using = LambdaJsonSerializer.class)
    @JsonDeserialize(using = LambdaJsonDeserializer.class)
    public
    interface SerializableRunnable<_T> extends Function<ActorContext<_T>, Behavior<_T>>, Serializable {
    }

    static class LambdaJsonSerializer<_T> extends JsonSerializer<SerializableRunnable<_T>> {
        @Override
        public void serialize(SerializableRunnable<_T> value, JsonGenerator gen, SerializerProvider serializers) throws IOException {
            try (ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
                 ObjectOutputStream outputStream = new ObjectOutputStream(byteArrayOutputStream)) {
                outputStream.writeObject(value);
                gen.writeBinary(byteArrayOutputStream.toByteArray());
            }
        }
    }

    static class LambdaJsonDeserializer<_T> extends JsonDeserializer<SerializableRunnable<_T>> {
        @Override
        public SerializableRunnable<_T> deserialize(JsonParser p, DeserializationContext ctx) throws IOException {
            byte[] value = p.getBinaryValue();
            try (ByteArrayInputStream byteArrayInputStream = new ByteArrayInputStream(value);
                 ObjectInputStream inputStream = new ObjectInputStream(byteArrayInputStream)) {
                return (SerializableRunnable<_T>) inputStream.readObject();
            } catch (ClassNotFoundException e) {
                throw new IOException(e);
            }
        }
    }


    public interface Command {
    }


    public static class Spawn<_T> implements Command, JsonSerializable {
        private final SerializableRunnable<_T> runnable;
        private final String name;
        private final Props props;
        private final ActorRef<ActorRef<_T>> replyTo;

        public Spawn(SerializableRunnable<_T> runnable, String name, Props props, ActorRef<ActorRef<_T>> replyTo) {
            assert (null != runnable);
            assert (null != name);
            assert (null != replyTo);

            this.runnable = runnable;
            this.name = name;
            this.props = props == Props.empty() ? null : props;
            this.replyTo = replyTo;
        }
    }

    public SpawnProtocol() {
    }

    public static Behavior<Command> apply() {
        return null;
    }

    public static <_T> Behavior<Command> create() {
        return Behaviors.receive((ctx, msg) -> {
            if (!(msg instanceof Spawn)) {
                System.out.println("throw new MatchError(msg): msg=" + msg);
//                throw new MatchError(msg);  // TODO test this
                return Behaviors.same();
            } else {
                Spawn<_T> spawn = (Spawn<_T>) msg;

                ActorRef<_T> actorRef;
                if (null == spawn.name || spawn.name.isEmpty()) {
                    // anonymous spawn
                    actorRef = ctx.spawnAnonymous(Behaviors.setup(spawn.runnable::apply),
                            null == spawn.props ? Props.empty() : spawn.props);
                } else {
                    actorRef = spawnWithUniqueName(0, spawn.name, ctx, Behaviors.setup(spawn.runnable::apply),
                            null == spawn.props ? Props.empty() : spawn.props);
                }

                spawn.replyTo.tell(actorRef);
                return Behaviors.same();
            }
        });
    }

    private static <_T> ActorRef<_T> spawnWithUniqueName(final int c, final String name, final ActorContext<?> ctx, final Behavior<_T> behavior, final Props props) {

        int i = c;
        while (true) {
            String nameSuggestion = c == 0 ? name : name + "-" + i;
            Optional<ActorRef<Void>> child = ctx.getChild(nameSuggestion);

            if (child.isEmpty()) {
                return ctx.spawn(behavior, nameSuggestion, props);
            }

//            if (!(var9 instanceof Some)) {
//                if (scala.None..MODULE$.equals(var9)) {
//                    return ctx.spawn(bhvr, nameSuggestion, props);
//                }
//
//                throw new MatchError(var9);
//            }

            ++i;
        }
    }

}
