package com.bmartin;

import akka.actor.Address;
import akka.actor.typed.ActorRef;
import akka.actor.typed.Behavior;
import akka.actor.typed.Props;
import akka.actor.typed.javadsl.ActorContext;
import akka.actor.typed.javadsl.Behaviors;
import akka.actor.typed.receptionist.Receptionist;

import com.bmartin.CborSerializable;
import com.bmartin.JsonSerializable;
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

import com.lg4dc.WrappedActorRef;

public class SpawnProtocol {
    @JsonSerialize(using = LambdaJsonSerializer.class)
    @JsonDeserialize(using = LambdaJsonDeserializer.class)
    public interface SerializableRunnable<_T> extends Function<ActorRef<SpawnProtocol.Command>, Function<ActorContext<_T>, Behavior<_T>>>, Serializable {
    }

    static class LambdaJsonSerializer<_T> extends JsonSerializer<SerializableRunnable<_T>> {
        @Override
        public void serialize(SerializableRunnable<_T> value, JsonGenerator gen, SerializerProvider serializers)
                throws IOException {
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
        public final SerializableRunnable<_T> runnable;
        public final String name;
        public final Props props;
        public final ActorRef<WrappedActorRef<_T>> replyTo;

        public Spawn(SerializableRunnable<_T> runnable, String name, Props props, ActorRef<WrappedActorRef<_T>> replyTo) {
            assert (null != runnable);
            assert (null != name);
            assert (null != replyTo);

            this.runnable = runnable;
            this.name = name;
            this.props = props == Props.empty() ? null : props;
            this.replyTo = replyTo;
        }
    }

    public static class SpawnAt<_T> implements Command, JsonSerializable {
        public final SerializableRunnable<_T> runnable;
        public final String name;
        public final Props props;
        public final Address at;
        public final ActorRef<WrappedActorRef<_T>> replyTo;


        public SpawnAt(SerializableRunnable<_T> runnable, String name, Props props, Address at,
                ActorRef<WrappedActorRef<_T>> replyTo) {
            assert (null != runnable);
            assert (null != name);
            assert (null != at);
        System.out.println(">> PlaceDiscovery::spawnAt");
            assert (null != replyTo);
        System.out.println(">>> PlaceDiscovery::spawnAt" + replyTo.toString());

            this.runnable = runnable;
            this.name = name;
            this.props = props == Props.empty() ? null : props;
            this.at = at;
            this.replyTo = replyTo;
        }
    }

    public static class ComponentsAt<_T> implements Command, JsonSerializable {
        public final Address at;
        public final ActorRef<WrappedActorRef<_T>> replyTo;


        public ComponentsAt(Address at, ActorRef<WrappedActorRef<_T>> replyTo) {
            assert (null != at);
            assert (null != replyTo);
        System.out.println(">>> PlaceDiscovery::ComponentsAt" + replyTo.toString());

            this.at = at;
            this.replyTo = replyTo;
        }
    }

    public static class WrappedListing implements JsonSerializable, Command {
        public final Receptionist.Listing response;

        public WrappedListing(Receptionist.Listing response) {
            this.response = response;
        }
    }

    public SpawnProtocol() {
    }

    public static Behavior<Command> apply() {
        return null;
    }

    public static <_T> ActorRef<_T> spawnWithUniqueName(final int c, final String name, final ActorContext<?> ctx,
            final Behavior<_T> behavior, final Props props) {

        int i = c;
        while (true) {
            String nameSuggestion = c == 0 ? name : name + "-" + i;
            Optional<ActorRef<Void>> child = ctx.getChild(nameSuggestion);

            if (child.isEmpty()) {
                return ctx.spawn(behavior, nameSuggestion, props);
            }

            // if (!(var9 instanceof Some)) {
            // if (scala.None..MODULE$.equals(var9)) {
            // return ctx.spawn(bhvr, nameSuggestion, props);
            // }
            //
            // throw new MatchError(var9);
            // }

            ++i;
        }
    }

}
