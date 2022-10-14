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

import akka.actor.typed.receptionist.Receptionist;
import akka.actor.typed.receptionist.ServiceKey;

class KVProtoServiceImpl implements KVProtoService {
   final public ActorSystem system;
   public ActorRef actor;
   
    public void getActor() {
        assert (null != system);

        while(actor == null){
            try {
                Thread.sleep(50);
                System.out.println("\nwaiting for detection\n");
                CompletionStage<Receptionist.Listing> result =
                AskPattern.ask(system.receptionist(),
                        (ActorRef<Receptionist.Listing> replyTo) -> Receptionist.find(KVCommand.SERVICE_KEY, replyTo),
                        Duration.ofSeconds(10),
                        system.scheduler());

                Set<ActorRef<KVCommand.Command>> listing = result.toCompletableFuture().get().getServiceInstances(KVCommand.SERVICE_KEY);

                for(ActorRef a: listing){
                    this.actor = a;
                }
            } catch (java.lang.Exception e) {
                System.out.println(e);
            }
        }
   }
   
   public KVProtoServiceImpl(ActorSystem system) {
        this.system = system;
        this.getActor();
                                                
   }

   @Override 
   public CompletionStage<ProtoGetResponse> get(
    ProtoGetRequest in) {
        if( this.actor == null)
            this.getActor();
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
        if( this.actor == null)
            this.getActor();
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
        if( this.actor == null)
            this.getActor();
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
