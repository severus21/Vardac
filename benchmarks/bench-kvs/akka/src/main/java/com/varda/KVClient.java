package com.varda;

//#cloc-exclude

import akka.actor.typed.ActorRef;
import akka.actor.typed.ActorSystem;
import akka.actor.typed.javadsl.ActorContext;
import akka.actor.typed.javadsl.AskPattern;
import akka.actor.typed.receptionist.Receptionist;
import com.typesafe.config.Config;
import io.vavr.Tuple2;
import java.time.Duration;
import java.util.Set;
import java.util.concurrent.CompletionStage;
import java.util.concurrent.ExecutionException;
import org.apache.commons.cli.*;

public class KVClient {
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

    static public ActorRef<KVCommand.Command> getShardActor(ActorContext context, int retry) {
        ActorSystem actorSystem = context.getSystem();

        CompletionStage<Receptionist.Listing> result = AskPattern.ask(
            actorSystem.receptionist(),
            (ActorRef<Receptionist.Listing> replyTo) -> Receptionist.find(ShardActor.KVShard_ACTOR_SERVICE_KEY, replyTo),
            DEFAULT_TIMEOUT,
            context.getSystem().scheduler());

        try {
            // blocking call
            Set<ActorRef<KVCommand.Command>> listing = result.toCompletableFuture().get().getServiceInstances(ShardActor.KVShard_ACTOR_SERVICE_KEY);
            if (listing.isEmpty()) {
                if (++retry < DEFAULT_MAX_RETRY + 1) {
                    final long timeout = DEFAULT_RETRY_TIMEOUT * retry;
                    actorSystem.log().info("getShardActor() retry " + retry + "/" + DEFAULT_MAX_RETRY + ", timeout=" + timeout);
                    // sleep and retry
                    Thread.sleep(timeout);
                    return getShardActor(context, retry);
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

	public static void run(Config config, int limit, int limitWarmup) throws InterruptedException {
        System.out.println("KVClient SERVICE STARTED");
		final ActorSystem system = ActorSystem.create(
			ClientActor.create(context -> getShardActor(context, 0), limit, limitWarmup),
			AbstractMain.SYSTEM_NAME,
            config);

		system.getWhenTerminated().toCompletableFuture().join();
	}
	
	

	
}