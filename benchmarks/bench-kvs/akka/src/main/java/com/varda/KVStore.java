package com.varda;


import akka.actor.typed.ActorSystem;
import org.apache.commons.cli.*;

public class KVStore {

	public static void main(String[] args) throws InterruptedException {

        Options options = new Options();
        Option nIterationsOpt = new Option("n", "niterations", true, "number of iterations");
        Option nWarmupIterationsOpt = new Option("warmup", "nwarmupiterations", true, "number of iterations for warmup");
		options.addOption(nIterationsOpt);
		options.addOption(nWarmupIterationsOpt);

        CommandLineParser parser = new DefaultParser();
        HelpFormatter formatter = new HelpFormatter();
        CommandLine cmd = null;

        try {
            cmd = parser.parse(options, args);
        } catch (ParseException e) {
            System.out.println(e.getMessage());
            formatter.printHelp("g", options);
            System.exit(1);
        }

        assert (null != cmd);
        if(!cmd.hasOption("n") || !cmd.hasOption("warmup") ){
            System.out.println("niterations/nwarmupiterations argument is mandatory!");
            formatter.printHelp("g", options);
            System.exit(1);
        }

		int nIterations = Integer.parseInt(cmd.getOptionValue("n"));
		int nWarmupIterations = Integer.parseInt(cmd.getOptionValue("warmup"));


		run(nIterations, nWarmupIterations);
	}

	public static void run(int limit, int limitWarmup) throws InterruptedException {
        System.out.println("KVClientKVShard SERVICE STARTED");
		final ActorSystem system = ActorSystem.create(
			ClientActor.create(
				context -> context.spawn(ShardActor.create(), "runner"),
				limit, limitWarmup), 
			AbstractMain.SYSTEM_NAME);

		system.getWhenTerminated().toCompletableFuture().join();
	}
	
	

	
}