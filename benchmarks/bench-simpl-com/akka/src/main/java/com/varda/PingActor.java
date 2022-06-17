package com.varda;


import akka.actor.typed.ActorSystem;
import akka.actor.typed.ActorRef;
import akka.actor.typed.Behavior;
import akka.actor.typed.javadsl.*;

import java.util.BitSet;
import java.util.function.*;
import java.io.FileWriter;
import java.io.IOException;

import com.google.gson.Gson;
import org.apache.commons.cli.*;

public class PingActor extends AbstractBehavior<PingActor.Command> {

	public interface Command extends CborSerializable {}

	static public Behavior<Command> create(Function<ActorContext, ActorRef<PongActor.Command>> pongFactory, int n, int limitWarmup) {
		return Behaviors.setup(context -> new PingActor(context, pongFactory.apply(context), n, limitWarmup));
	}

	long start;
	private final int limit;
	private final int limitWarmup;
	private final BitSet bitset;
	private final BitSet bitsetWarmup;
	private final ActorRef<PongActor.Command> pongAct;
	private final long[] rtts;

	public PingActor(ActorContext<Command> context, ActorRef<PongActor.Command> pongAct, int limit, int limitWarmup) {
		super(context);
		assert(pongAct != null);
		this.limit = limit;
		this.bitset = new BitSet(limit);
		this.limitWarmup = limitWarmup;
		this.bitsetWarmup = new BitSet(limitWarmup);
		bitset.set(0, limit);
		bitsetWarmup.set(0, limitWarmup);
		this.pongAct = pongAct;
		this.rtts = new long[limit];

		this.start();
	}

	private void store_rtts(){
		Gson gson = new Gson();

		try {
			FileWriter fileWriter =new FileWriter("rtts.json"); 
			gson.toJson(this.rtts, fileWriter);
			fileWriter.close();
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