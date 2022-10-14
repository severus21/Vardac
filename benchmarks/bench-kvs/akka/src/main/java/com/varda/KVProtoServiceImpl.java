package com.varda;

import java.lang.reflect.Array;
import java.util.*;
import java.util.Iterator;
import java.time.Duration;
import java.util.concurrent.CompletableFuture;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.function.Function;
import java.util.function.BiFunction;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import akka.actor.typed.ActorRef;
import akka.actor.typed.Behavior;
import akka.actor.typed.javadsl.AbstractBehavior;
import akka.actor.typed.javadsl.ActorContext;
import akka.actor.typed.javadsl.Behaviors;
import akka.actor.typed.javadsl.Receive;
import akka.actor.typed.javadsl.TimerScheduler;
import akka.actor.typed.receptionist.Receptionist;
import akka.cluster.ClusterEvent;
import akka.cluster.typed.Cluster;
import akka.cluster.typed.Subscribe;
import akka.grpc.javadsl.ServiceHandler;
import akka.http.javadsl.model.HttpRequest;
import akka.http.javadsl.model.HttpResponse;
import java.util.concurrent.CompletionStage;
import akka.http.javadsl.Http;
import com.typesafe.config.Config;
import com.typesafe.config.ConfigFactory;
import akka.stream.SystemMaterializer;
import akka.stream.Materializer;
import akka.http.javadsl.ServerBinding;
import akka.actor.typed.ActorSystem;
import akka.actor.typed.javadsl.AskPattern;
import io.vavr.*;
import io.vavr.control.*;
import com.varda.*;

import akka.actor.typed.Props;
import akka.cluster.typed.Cluster;
import akka.actor.typed.receptionist.ServiceKey;
import com.varda.grpc.*;

class KVProtoServiceImpl implements KVProtoService {
   final public ActorSystem system;
   final public ActorRef actor;
   
    public ActorRef getActor(int retry) {
        assert (null != system);

        int DEFAULT_MAX_RETRY = 5;
        long DEFAULT_RETRY_TIMEOUT = 500;   // in ms
        Duration DEFAULT_TIMEOUT = Duration.ofSeconds(3);

        ActorRef actor = null;

        try {
            // blocking call
            Set<ActorRef> listing = PlaceDiscovery.activationsAt(this.system, "front");
            if (listing.isEmpty()) {
                if (++retry < DEFAULT_MAX_RETRY + 1) {
                    final long timeout = DEFAULT_RETRY_TIMEOUT * retry;
                    system.log().info("KVProtoServiceImpl::getActor() retry " + retry + "/" + DEFAULT_MAX_RETRY + ", timeout=" + timeout);
                    // sleep and retry
                    Thread.sleep(timeout);
                    return getActor(retry);
                } else {
                    throw new RuntimeException("Could not find systemProject_name after " + DEFAULT_MAX_RETRY + " retries.");
                }
            } else {
                actor = listing.iterator().next();    // TODO: if more than 1 result, use closest
                system.log().info("KVProtoServiceImpl::getActor(): found Client26 " + actor +
                        " out of " + listing.size());
            }
        } catch (java.lang.InterruptedException e) {
            assert(false);
            //TODO
        }

        return actor;
                                                    
   }
   
   public KVProtoServiceImpl(ActorSystem system) {
        this.system = system;
        this.actor = this.getActor(0);
                                                
   }

   @Override 
   public CompletionStage<ProtoGetResponse> get(
    ProtoGetRequest in) {
        assert(this.actor != null);
                                
        Function<KVCommand.GetResult,  ProtoGetResponse> unpack = 
        ( (KVCommand.GetResult msg) -> ProtoGetResponse.newBuilder().
        setKey(msg.key).setValue(msg.value).build() );
        return AskPattern.ask(
            this.actor, 
            ( ( replyToActor) -> { return new 
                KVCommand.GetRequest(
                    in.getKey(), 
                    replyToActor,
                    0); } ), 
                    Duration.ofSeconds(5), 
                    this.system.scheduler()
                ).thenApply(
                unpack);
    }

   @Override 
   public CompletionStage<ProtoDeleteResponse> delete(
    ProtoDeleteRequest in) {
        assert(this.actor != null);
                                
        Function<KVCommand.DeleteResult,  ProtoDeleteResponse> unpack = 
        ( (KVCommand.DeleteResult msg) -> ProtoDeleteResponse.newBuilder().
        setKey(msg.key).setFlag(msg.flag).build() );
        return AskPattern.ask(
            this.actor, 
            ( ( replyToActor) -> { return new 
                KVCommand.DeleteRequest(
                    in.getKey(), 
                    replyToActor,
                    0); } ), 
                    Duration.ofSeconds(5), 
                    this.system.scheduler()
                ).thenApply(
                unpack);
    }

   @Override 
   public CompletionStage<ProtoPutResponse> put(
    ProtoPutRequest in) {
        assert(this.actor != null);
                                
        Function<KVCommand.PutResult,  ProtoPutResponse> unpack = 
        ( (KVCommand.PutResult msg) -> ProtoPutResponse.newBuilder().
        setKey(msg.key).setFlag(msg.flag).build() );
        return AskPattern.ask(
            this.actor, 
            ( ( replyToActor) -> { return new 
                KVCommand.PutRequest(
                    in.getKey(), 
                    in.getValue(),
                    replyToActor,
                    0); } ), 
                    Duration.ofSeconds(5), 
                    this.system.scheduler()
                ).thenApply(
                unpack);
    }

}
