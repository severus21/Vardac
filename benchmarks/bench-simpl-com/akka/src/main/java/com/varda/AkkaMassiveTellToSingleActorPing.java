package com.varda;

import akka.actor.typed.ActorRef;
import akka.actor.typed.javadsl.ActorContext;
import akka.actor.typed.ActorSystem;
import akka.actor.typed.javadsl.AskPattern;
import akka.actor.typed.receptionist.Receptionist;

import java.time.Duration;
import java.util.Set;
import java.util.concurrent.CompletionStage;
import java.util.concurrent.ExecutionException;

import io.vavr.Tuple2;

import com.typesafe.config.Config;

import org.apache.commons.cli.*;

public class AkkaMassiveTellToSingleActorPing {
    public static final int DEFAULT_MAX_RETRY = 5;
    public static final long DEFAULT_RETRY_TIMEOUT = 500;   // in ms    
    public static final Duration DEFAULT_TIMEOUT = Duration.ofSeconds(3);


	public static void main(String[] args) throws InterruptedException {

        Options options = new Options();
        Option nIterationsOpt = new Option("n", "niterations", true, "number of iterations");
        Option nWarmupIterationsOpt = new Option("warmup", "nwarmupiterations", true, "number of iterations for warmup");
		options.addOption(nIterationsOpt);
		options.addOption(nWarmupIterationsOpt);

        Tuple2<CommandLine, HelpFormatter> res = AbstractMain.get_cmd(options, args);
        CommandLine cmd = res._1;
        HelpFormatter formatter = res._2;

        if(!cmd.hasOption("n") || !cmd.hasOption("warmup") ){
            System.out.println("niterations/nwarmupiterations argument is mandatory!");
            formatter.printHelp("g", options);
            System.exit(1);
        }

		int nIterations = Integer.parseInt(cmd.getOptionValue("n"));
		int nWarmupIterations = Integer.parseInt(cmd.getOptionValue("warmup"));


		run(AbstractMain.get_config(cmd), nIterations, nWarmupIterations);
	}

    static public ActorRef<PongActor.Command> getPongActor(ActorContext context, int retry) {
        ActorSystem actorSystem = context.getSystem();

        CompletionStage<Receptionist.Listing> result = AskPattern.ask(
            actorSystem.receptionist(),
            (ActorRef<Receptionist.Listing> replyTo) -> Receptionist.find(PongActor.PONG_ACTOR_SERVICE_KEY, replyTo),
            DEFAULT_TIMEOUT,
            context.getSystem().scheduler());

        try {
            // blocking call
            Set<ActorRef<PongActor.Command>> listing = result.toCompletableFuture().get().getServiceInstances(PongActor.PONG_ACTOR_SERVICE_KEY);
            if (listing.isEmpty()) {
                if (++retry < DEFAULT_MAX_RETRY + 1) {
                    final long timeout = DEFAULT_RETRY_TIMEOUT * retry;
                    actorSystem.log().info("getPongActor() retry " + retry + "/" + DEFAULT_MAX_RETRY + ", timeout=" + timeout);
                    // sleep and retry
                    Thread.sleep(timeout);
                    return getPongActor(context, retry);
                } else {
                    throw new RuntimeException("Could not find TransactionManager after " + DEFAULT_MAX_RETRY + " retries.");
                }
            } else {
                return listing.iterator().next(); // return the first one
            }
        } catch (ExecutionException e) {
            throw new RuntimeException(e.getCause().toString());
        } catch (InterruptedException e) {
            throw new RuntimeException(e.toString());
        }
    }

	public static void run(Config config, int messagecount, int limitWarmup) throws InterruptedException {
        System.out.println("PING SERVICE STARTED");
		final ActorSystem system = ActorSystem.create(
			PingActor.create(context -> getPongActor(context, 0), messagecount, limitWarmup),
			AbstractMain.SYSTEM_NAME,
            config);

		system.getWhenTerminated().toCompletableFuture().join();
	}
	
	

	
}