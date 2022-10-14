package com.varda;

import akka.actor.typed.ActorRef;
import akka.actor.typed.ActorSystem;
import akka.actor.typed.javadsl.ActorContext;
import akka.actor.typed.javadsl.AskPattern;
import io.vavr.control.*;
import java.time.Duration;
import java.util.*;
import java.util.concurrent.CompletionStage;



public class PlaceDiscovery {
    static int MAX_REGISTER_RETRY = 10;
    static int REGISTER_POLLING_SLEEP_DURATION = 100; // in ms

    // For activation registration
    public static String activationsServiceKeyOf(String key) {
        return "_activations_" + key;
    }

    public static Either<Error, Boolean> register(ActorContext context, String key, ActorRef a){
        ActorRef<ReplacedReceptionist.Command> receptionist = ReplacedReceptionist.getReceptionist(context); 

        context.getLog().debug("PlaceDiscovery.register");
        receptionist.tell(new ReplacedReceptionist.Register(activationsServiceKeyOf(key), a));

        //Wait that for visible registered activation
        for(int i = 0; i < MAX_REGISTER_RETRY; i++){ //10 retry max
            Set<ActorRef> activations = activationsAt(context, key);
            if(activations.contains(a)){
                context.getLog().debug("RegisterActivation::register "+a.toString());
                return Either.right(true);
            } else {
                try{
                    Thread.sleep(REGISTER_POLLING_SLEEP_DURATION);
                } catch( Exception e){
                    System.out.println(e);
                }
            }
        }

        context.getLog().error("RegisterActivation::not registered");
        return Either.left(new Error("timeout before seeing its own update"));
    }

    public static Set<ActorRef> activationsAt(ActorContext context, String key) {
       return activationsAt(context.getSystem(), key);
    }

    public static Set<ActorRef> activationsAt(ActorSystem sys, String key) {
        assert (sys != null);
        ActorRef<ReplacedReceptionist.Command> receptionist = ReplacedReceptionist.getReceptionist(sys);

        CompletionStage<Set<ActorRef>> ask = AskPattern.ask(
                receptionist,
                replyTo -> new ReplacedReceptionist.GetValue(activationsServiceKeyOf(key), replyTo),
                Duration.ofSeconds(10),
                sys.scheduler());

        try {
            // blocking call
            Set<ActorRef> tmp = ask.toCompletableFuture().get();
            return tmp;
        } catch (Exception e) {
            sys.log().error(e.toString());
            return new HashSet<>();
        }
    }
}
