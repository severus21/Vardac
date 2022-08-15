package com.varda;

import akka.actor.typed.ActorSystem;

import java.util.Arrays;
import java.util.Random;
import java.util.stream.IntStream;

import org.apache.commons.cli.*;

public class AkkaMassiveTellToSingleActor {

    public static int[] generate_random_input(int size){
        //deterministic since the seed is controled, only depends on size value

        Random random = new Random(0L); 
		int[] input = IntStream.range(0, 1 << size).map(i -> random.nextInt()).toArray();
        return input;
    }

	public static void main(String[] args) throws InterruptedException {

        Options options = new Options();
        Option nIterationsOpt = new Option("n", "niterations", true, "number of iterations");
        Option nVSizeOpt = new Option("vs", "vsize", true, "vector size");
        Option nWarmupIterationsOpt = new Option("warmup", "nwarmupiterations", true, "number of iterations for warmup");
		options.addOption(nIterationsOpt);
		options.addOption(nWarmupIterationsOpt);
		options.addOption(nVSizeOpt);

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
        if(!cmd.hasOption("n")|| !cmd.hasOption("s")  || !cmd.hasOption("warmup") ){
            System.out.println("niterations/nwarmupiterations/vsize argument is mandatory!");
            formatter.printHelp("g", options);
            System.exit(1);
        }

		int nIterations = Integer.parseInt(cmd.getOptionValue("n"));
		int nWarmupIterations = Integer.parseInt(cmd.getOptionValue("warmup"));
		int nVSize = Integer.parseInt(cmd.getOptionValue("s"));


		run(nIterations, nWarmupIterations, generate_random_input(nVSize));
	}

	public static void run(int messagecount, int limitWarmup, int[] input) throws InterruptedException {
        System.out.println("PINGPONG SERVICE STARTED");
		final ActorSystem system = ActorSystem.create(
			MasterActor.create(messagecount, limitWarmup, input), 
			AbstractMain.SYSTEM_NAME);

		system.getWhenTerminated().toCompletableFuture().join();
	}
	
	

	
}