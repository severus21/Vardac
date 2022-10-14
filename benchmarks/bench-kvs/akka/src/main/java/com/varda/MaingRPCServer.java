package com.varda;

import akka.actor.typed.ActorSystem;
import akka.actor.typed.Behavior;
import akka.actor.typed.javadsl.AbstractBehavior;
import akka.actor.typed.javadsl.ActorContext;
import akka.actor.typed.javadsl.Behaviors;
import akka.actor.typed.javadsl.Receive;
import akka.http.javadsl.Http;
import akka.http.javadsl.ServerBinding;
import akka.http.javadsl.model.HttpRequest;
import akka.http.javadsl.model.HttpResponse;
import com.typesafe.config.Config;
import com.typesafe.config.ConfigFactory;
import com.varda.grpc.*;
import java.util.concurrent.CompletionStage;
import org.apache.commons.cli.*;

class MaingRPCServer  extends AbstractBehavior<KVCommand.Command> {

   
    public static void main(String[] args) throws Exception {
        // important to enable HTTP/2 in ActorSystem's config
        Config conf = ConfigFactory.parseString("akka.http.server.preview.enable-http2 = on")
                .withFallback(AbstractMain.get_config(AbstractMain.get_cmd(new Options(), args)._1));

        // Akka ActorSystem Boot
        ActorSystem sys = ActorSystem.create(
            MaingRPCServer.create(), 
            "systemAkkaBench",
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
                new KVProtoServiceImpl(sys), 
                sys);

        return Http.get(sys).newServerAt("0.0.0.0", 8090).bind(gRPCService);
    }

        @Override
        public Receive<KVCommand.Command> createReceive() {
            return newReceiveBuilder().build();
        }
}
