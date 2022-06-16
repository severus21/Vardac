package com.varda;


import akka.actor.typed.ActorSystem;
import akka.actor.typed.ActorRef;
import akka.actor.typed.Behavior;
import akka.actor.typed.javadsl.*;

import java.util.BitSet;

import org.apache.commons.cli.*;

public class AkkaMassiveTellToSingleActor {
	public interface Command {}

	static private class Ping implements Command {
		public int i;
		public ActorRef replyTo;
		public Ping(int i, ActorRef replyTo){
			this.i = i;
			this.replyTo = replyTo;
		}
	}

	static private class Pong implements Command {
		public int i;
		public ActorRef replyTo;
		public Pong(int i, ActorRef replyTo){
			this.i = i;
			this.replyTo = replyTo;
		}
	}

	public static void main(String[] args) throws InterruptedException {

        Options options = new Options();
        Option nIterationsOpt = new Option("n", "niterations", true, "number of iterations");
		options.addOption(nIterationsOpt);

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
        if(!cmd.hasOption("n")){
            System.out.println("niterations argument is mandatory!");
            formatter.printHelp("g", options);
            System.exit(1);
        }

		int nIterations = Integer.parseInt(cmd.getOptionValue("n"));


		run(nIterations);
	}

	public static void run(int messagecount) throws InterruptedException {
		final ActorSystem system = ActorSystem.create(Master.create(messagecount), "systemAkkaBench");
		system.getWhenTerminated().toCompletableFuture().join();
	}
	
	

	private static class Master extends AbstractBehavior<Command> {
		long start;

		static public Behavior<Command> create(int n) {
			return Behaviors.setup(context -> new Master(context, n));
		}

		private final int limit;
		private final BitSet bitset;
		private final ActorRef<Command> pongAct;

		public Master(ActorContext<Command> context, int limit) {
			super(context);
			this.limit = limit;
			this.bitset = new BitSet(limit);
			bitset.set(0, limit);
			this.pongAct = getContext().spawn(Runner.create(), "runner");

			this.start();
		}

		private void start() {
			System.err.println("AKKA Massive Tell started...");
			this.start = System.currentTimeMillis();
			for (int i=0; i<limit; i++) {
				this.pongAct.tell(new Ping(i, getContext().getSelf()));	
			}
		}

		@Override
		public Receive<Command> createReceive() {
			return newReceiveBuilder()
				.onMessage(Pong.class, this::onPong)
				.build();
		}
		
		private Behavior<Command> onPong(Pong pong) {
			bitset.clear(pong.i);
			if (bitset.isEmpty()) {
				long end = System.currentTimeMillis();
				getContext().getLog().info("Time elapse " + (end - start) +" ms");
				getContext().getLog().info("Terminated ueyiqu8R");
				getContext().getLog().info("JamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglog");
				getContext().getSystem().terminate();	
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
			ping.replyTo.tell(new Pong(ping.i, getContext().getSelf()));
			return Behaviors.same();
		}
	}
}