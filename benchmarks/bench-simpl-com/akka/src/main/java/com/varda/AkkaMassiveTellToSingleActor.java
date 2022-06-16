package com.varda;


import akka.actor.typed.ActorSystem;
import akka.actor.typed.ActorRef;
import akka.actor.typed.Behavior;
import akka.actor.typed.javadsl.*;

import java.util.BitSet;
import java.io.FileWriter;
import java.io.IOException;

import com.google.gson.Gson;

import org.apache.commons.cli.*;

public class AkkaMassiveTellToSingleActor {
	public interface Command {}

	static private class Ping implements Command {
		public int i;
		public boolean warmup;
		public ActorRef replyTo;
		public long initTimestamp;
		public Ping(int i, boolean warmup, ActorRef replyTo, long initTimestamp){
			this.i = i;
			this.warmup = warmup;
			this.replyTo = replyTo;
			this.initTimestamp = initTimestamp;
		}
	}

	static private class Pong implements Command {
		public int i;
		public boolean warmup;
		public ActorRef replyTo;
		public long initTimestamp;
		public Pong(int i, boolean warmup, ActorRef replyTo, long initTimestamp){
			this.i = i;
			this.warmup = warmup;
			this.replyTo = replyTo;
			this.initTimestamp = initTimestamp;
		}
	}

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

	public static void run(int messagecount, int limitWarmup) throws InterruptedException {
		final ActorSystem system = ActorSystem.create(Master.create(messagecount, limitWarmup), "systemAkkaBench");
		system.getWhenTerminated().toCompletableFuture().join();
	}
	
	

	private static class Master extends AbstractBehavior<Command> {
		long start;

		static public Behavior<Command> create(int n, int limitWarmup) {
			return Behaviors.setup(context -> new Master(context, n, limitWarmup));
		}

		private final int limit;
		private final int limitWarmup;
		private final BitSet bitset;
		private final BitSet bitsetWarmup;
		private final ActorRef<Command> pongAct;
		private final long[] rtts;

		public Master(ActorContext<Command> context, int limit, int limitWarmup) {
			super(context);
			this.limit = limit;
			this.bitset = new BitSet(limit);
			this.limitWarmup = limitWarmup;
			this.bitsetWarmup = new BitSet(limitWarmup);
			bitset.set(0, limit);
			bitsetWarmup.set(0, limitWarmup);
			this.pongAct = getContext().spawn(Runner.create(), "runner");
			this.rtts = new long[limit];

			this.start();
		}

		private void store_rtts(){
			Gson gson = new Gson();

			try {
				FileWriter fileWriter =new FileWriter("rtts.json"); 
				gson.toJson(this.rtts, fileWriter);
				fileWriter.close();
				System.err.println(gson.toJson(this.rtts));
			} catch (IOException e ){
				System.err.println(e.toString());
			}
		}

		private void start() {
			System.err.println("AKKA PingPong warmup");
			for (int i=0; i<limitWarmup; i++) {
				this.pongAct.tell(new Ping(i, true, getContext().getSelf(), System.currentTimeMillis()));	
			}

			if (limitWarmup == 0)
				this.run();
		}

		private void run(){
			System.err.println("VKKA Massive Tell started...");
			this.start = System.currentTimeMillis();
			for (int i=0; i<limit; i++) {
				this.pongAct.tell(new Ping(i, false, getContext().getSelf(), System.currentTimeMillis()));	
			}
		}

		@Override
		public Receive<Command> createReceive() {
			return newReceiveBuilder()
				.onMessage(Pong.class, this::onPong)
				.build();
		}
		
		private Behavior<Command> onPong(Pong pong) {
			if(pong.warmup) {
				bitsetWarmup.clear(pong.i);

				// Warmup is over, run the bench
				if (bitsetWarmup.isEmpty()) {
					this.run();
				}
			} else {
				bitset.clear(pong.i);

				// Calcull RTT
				long endTimestamp = System.currentTimeMillis();
				long rtt = endTimestamp - pong.initTimestamp;
				this.rtts[pong.i] = rtt;
				getContext().getLog().info("rtt of " + pong.i +": "+rtt);

				if (bitset.isEmpty()) {
					long end = System.currentTimeMillis();
					getContext().getLog().info("Time elapse " + (end - start) +" ms");
					this.store_rtts();

					getContext().getLog().info("Terminated ueyiqu8R");
					getContext().getLog().info("JamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglog");
					getContext().getSystem().terminate();	
				}
			}
			return Behaviors.same();
		}
	}
	
	private static class Runner extends AbstractBehavior<Command> {
		public Runner(ActorContext<Command> context) {
			super(context);
		}

		static public Behavior<Command> create() {
			return Behaviors.setup(context -> new Runner(context));
		}
		
		@Override
		public Receive<Command> createReceive() {
			return newReceiveBuilder()
				.onMessage(Ping.class, this::onPing)
				.build();
		}
		
		private Behavior<Command> onPing(Ping ping) {
			ping.replyTo.tell(new Pong(ping.i, ping.warmup, getContext().getSelf(), ping.initTimestamp));
			return Behaviors.same();
		}
	}
}