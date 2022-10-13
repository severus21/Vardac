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
import com.varda.timers.*;
import com.bmartin.*;

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
            Set<ActivationRef> listing = PlaceDiscovery.activationsAt(this.system, Client26.class);
            if (listing.isEmpty()) {
                if (++retry504 < DEFAULT_MAX_RETRY + 1) {
                    final long timeout = DEFAULT_RETRY_TIMEOUT * retry504;
                    system503.log().info("ClientServiceImpl453::getActor() retry " + retry504 + "/" + DEFAULT_MAX_RETRY + ", timeout=" + timeout);
                    // sleep and retry
                    Thread.sleep(timeout);
                    return getActor502(system503, retry504);
                } else {
                    throw new RuntimeException("Could not find systemProject_name after " + DEFAULT_MAX_RETRY + " retries.");
                }
            } else {
                actor = listing.iterator().next().actorRef;    // TODO: if more than 1 result, use closest
                system.log().info("ClientServiceImpl453::getActor(): found Client26 " + actor +
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
    ProtoGetRequest in457) {
        assert(this.actor455 != null);
                                
        Function<author.project_name.Stage2235.Actor2Service462,  ProtoMsgOut435> unpack463 = 
        ( (author.project_name.Stage2235.Actor2Service462 message458) -> { return ProtoMsgOut435.newBuilder().
        setRetValue434(message458._0_()).build(); } );
        return AskPattern.ask(
                                this.actor455, 
                                ( ( replyToActor464) -> { return new 
                                author.project_name.Stage2235.Service2Actor460(
                                
                                in457.getK430(), 
                                in457.getV431(), 
                                replyToActor464); } ), 
                                Duration.ofSeconds(5), 
                                this.system454.scheduler()).thenApply(
                unpack463);
    }

    @Override 
    public CompletionStage<ProtoPutResponse> put(ProtoGetResponse in473) {
        assert(this.actor != null);
                                    
        Function<author.project_name.Stage2235.Actor2Service478,  ProtoMsgOut447> unpack479 = 
        ( (author.project_name.Stage2235.Actor2Service478 message474) -> { return ProtoMsgOut447.newBuilder().
        setRetValue446(message474._0_()).build(); } );
        return AskPattern.ask(
                                this.actor455, 
                                ( ( replyToActor480) -> { return new 
                                author.project_name.Stage2235.Service2Actor476(
                                
                                in473.getK443(), 
                                replyToActor480); } ), 
                                Duration.ofSeconds(5), 
                                this.system454.scheduler()).thenApply(
                unpack479);
    }
}
