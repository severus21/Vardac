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


class MaingRPCServer {

   
    public static void main(String[] args) throws Exception {
        // important to enable HTTP/2 in ActorSystem's config
        Config conf = ConfigFactory.parseString("akka.http.server.preview.enable-http2 = on")
                .withFallback(AbstractMain.get_config(args));

        // Akka ActorSystem Boot
        ActorSystem sys = ActorSystem.create(
            author.project_name.MaingRPCServer.create(), 
            "systemProject_name",
            conf);

        run(sys).thenAccept(binding -> {
            System.out.println("gRPC server bound to: " + binding.localAddress());
        });

        // ActorSystem threads will keep the app alive until `system.terminate()` is called
    } 

    public static void main_run(ActorSystem sys){
        run(sys).thenAccept(binding -> {
            System.out.println("gRPC server bound to: " + binding.localAddress());
        });

        // ActorSystem threads will keep the app alive until `system.terminate()` is called
    }
   
    static public Behavior<KVCommand.Command> create() {
        return Behaviors.setup(context -> new MaingRPCServer(context));
    }

    public MaingRPCServer(ActorContext<KVCommand.Command> context) {
        super(context);
    }
                               
   
    static public CompletionStage<ServerBinding> run(
        ActorSystem sys) {
            akka.japi.function.Function<HttpRequest, CompletionStage<HttpResponse>> gRPCService = 
            KVProtoServiceHandlerFactory.create(
                new author.project_name.ClientServiceImpl453(sys), 
                sys);

        return Http.get(sys).newServerAt("0.0.0.0", 8090).bind(gRPCService);
    }

        @Override
        public Receive<KVCommand.Command> createReceive() {
            return newReceiveBuilder().build();
        }
}
