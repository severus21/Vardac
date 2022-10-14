package com.varda;

import akka.actor.typed.ActorRef;
import akka.actor.typed.ActorSystem;
import akka.actor.typed.Behavior;
import akka.actor.typed.javadsl.AbstractBehavior;
import akka.actor.typed.javadsl.ActorContext;
import akka.actor.typed.javadsl.Behaviors;
import akka.actor.typed.javadsl.Receive;
import akka.cluster.ddata.Key;
import akka.cluster.ddata.ORMultiMap;
import akka.cluster.ddata.ORMultiMapKey;
import akka.cluster.ddata.SelfUniqueAddress;
import akka.cluster.ddata.typed.javadsl.DistributedData;
import akka.cluster.ddata.typed.javadsl.Replicator;
import akka.cluster.ddata.typed.javadsl.ReplicatorMessageAdapter;
import akka.cluster.typed.ClusterSingleton;
import akka.cluster.typed.SingletonActor;
import java.util.Set;
import scala.collection.JavaConverters;

public class ReplacedReceptionist extends AbstractBehavior<ReplacedReceptionist.Command> {
    public static ActorRef<ReplacedReceptionist.Command> getReceptionist(ActorContext context){
        return getReceptionist(context.getSystem());
    }

    public static ActorRef<ReplacedReceptionist.Command> getReceptionist(ActorSystem sys){
        ClusterSingleton singleton = ClusterSingleton.get(sys);
        return singleton.init(SingletonActor.of(ReplacedReceptionist.create(new ORMultiMapKey("bridge")), "replacted_receptionist"));
    }

    interface Command extends CborSerializable {}

    public static class Register implements Command {
        public final ActorRef a;
        public final String key;

        public Register(String key, ActorRef a){
            this.key = key;
            this.a = a;
        }
    }

    public static class GetValue implements Command {
        public final String key;
        public final ActorRef replyTo;

        public GetValue(String key, ActorRef replyTo) {
            this.key = key;
            this.replyTo = replyTo;
        }
    }

    public static class GetCachedValue implements Command {
        public final String key;
        public final ActorRef replyTo;

        public GetCachedValue(String key, ActorRef replyTo) {
            this.key = key;
            this.replyTo = replyTo;
        }
    }

    enum Unsubscribe implements Command {
        INSTANCE
    }

    private interface InternalCommand extends Command {}

    private static class InternalUpdateResponse implements InternalCommand {
      final Replicator.UpdateResponse<ORMultiMap<String, ActorRef>> rsp;

      InternalUpdateResponse(Replicator.UpdateResponse<ORMultiMap<String, ActorRef>> rsp) {
        this.rsp = rsp;
      }
    }

    private static class InternalGetResponse implements InternalCommand {
      final Replicator.GetResponse<ORMultiMap<String, ActorRef>> rsp;
      final String key;
      final ActorRef<Set<ActorRef>> replyTo;

      InternalGetResponse(Replicator.GetResponse<ORMultiMap<String, ActorRef>> rsp, String key, ActorRef<Set<ActorRef>> replyTo) {
        this.rsp = rsp;
        this.key = key;
        this.replyTo = replyTo;
      }
    }

    private static final class InternalSubscribeResponse implements InternalCommand {
      final Replicator.SubscribeResponse<ORMultiMap<String, ActorRef>> rsp;

      InternalSubscribeResponse(Replicator.SubscribeResponse<ORMultiMap<String, ActorRef>> rsp) {
        this.rsp = rsp;
      }
    }

    public static Behavior<Command> create(Key<ORMultiMap<String, ActorRef>> key) {
        return Behaviors.setup(
            ctx ->
                DistributedData.withReplicatorMessageAdapter(
                    (ReplicatorMessageAdapter<Command, ORMultiMap<String, ActorRef>> replicatorAdapter) ->
                        new ReplacedReceptionist(ctx, replicatorAdapter, key)));
    }

    // adapter that turns the response messages from the replicator into our own protocol
    private final ReplicatorMessageAdapter<Command, ORMultiMap<String, ActorRef>> replicatorAdapter;
    private final SelfUniqueAddress node;
    private final Key<ORMultiMap<String, ActorRef>> key;

    private java.util.Map<String, java.util.Set<ActorRef>>	cachedValue;

    private ReplacedReceptionist(
        ActorContext<Command> context,
        ReplicatorMessageAdapter<Command, ORMultiMap<String, ActorRef>> replicatorAdapter,
        Key<ORMultiMap<String, ActorRef>> key) {
      super(context);

      this.replicatorAdapter = replicatorAdapter;
      this.key = key;

      final SelfUniqueAddress node = DistributedData.get(context.getSystem()).selfUniqueAddress();

      this.node = DistributedData.get(context.getSystem()).selfUniqueAddress();

      this.replicatorAdapter.subscribe(this.key, InternalSubscribeResponse::new);
    }

    @Override
    public Receive<Command> createReceive() {
      return newReceiveBuilder()
          .onMessage(Register.class, this::onRegister)
          .onMessage(InternalUpdateResponse.class, msg -> Behaviors.same())
          .onMessage(GetValue.class, this::onGetValue)
          .onMessage(GetCachedValue.class, this::onGetCachedValue)
          .onMessage(Unsubscribe.class, this::onUnsubscribe)
          .onMessage(InternalGetResponse.class, this::onInternalGetResponse)
          .onMessage(InternalSubscribeResponse.class, this::onInternalSubscribeResponse)
          .build();
    }

    private Behavior<Command> onRegister(Register register) {
        replicatorAdapter.askUpdate(
            askReplyTo ->
                new Replicator.Update<>(
                    key,
                    ORMultiMap.empty(),
                    Replicator.writeLocal(),
                    askReplyTo,
                    (ORMultiMap<String, ActorRef> curr) -> curr.addBinding(node, register.key, register.a)),
            InternalUpdateResponse::new);

        return this;
    }

    private Behavior<Command> onGetValue(GetValue cmd) {
        replicatorAdapter.askGet(
            askReplyTo -> new Replicator.Get<>(key, Replicator.readLocal(), askReplyTo),
            rsp -> new InternalGetResponse(rsp, cmd.key, cmd.replyTo));

        return this;
    }

    private Behavior<Command> onGetCachedValue(GetCachedValue cmd) {
        cmd.replyTo.tell(cachedValue.get(cmd.key));
        return this;
    }

    private Behavior<Command> onUnsubscribe(Unsubscribe cmd) {
        replicatorAdapter.unsubscribe(key);
        return this;
    }

    private Behavior<Command> onInternalGetResponse(InternalGetResponse msg) {
        if (msg.rsp instanceof Replicator.GetSuccess) {
            scala.Option<scala.collection.immutable.Set<ActorRef>> value = ((Replicator.GetSuccess<?>) msg.rsp).get(key).get(msg.key);

            if(value.isEmpty())
                msg.replyTo.tell(new java.util.HashSet());
            else
                msg.replyTo.tell(JavaConverters.asJava(value.get()));
            return this;
        } else {
            // not dealing with failures
            return Behaviors.unhandled();
        }
    }

    private Behavior<Command> onInternalSubscribeResponse(InternalSubscribeResponse msg) {
        if (msg.rsp instanceof Replicator.Changed) {
            ORMultiMap<String, ActorRef> map = ((Replicator.Changed<?>) msg.rsp).get(key);
            cachedValue = map.getEntries();
            return this;
        } else {
            return Behaviors.unhandled();
        }
    }
}