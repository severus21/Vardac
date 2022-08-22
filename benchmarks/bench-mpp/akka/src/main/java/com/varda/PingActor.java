package com.varda;


import akka.actor.typed.ActorSystem;
import akka.actor.typed.ActorRef;
import akka.actor.typed.Behavior;
import akka.actor.typed.javadsl.*;

import java.util.*;
import java.util.function.*;
import java.io.FileWriter;
import java.io.IOException;

import com.google.gson.Gson;
import org.apache.commons.cli.*;

public class PingActor extends AbstractBehavior<PingActor.Command> {

	public interface Command extends CborSerializable {}

	static public Behavior<Command> create(Function<ActorContext, ActorRef<PongActor.Command>> pongFactory, int n, int limitWarmup, int payload) {
		return Behaviors.setup(context -> new PingActor(context, pongFactory.apply(context), n, limitWarmup, payload));
	}

	long start;
	private final int limit;
	private final int limitWarmup;
	private final int payload;
	private final BitSet bitset;
	private final BitSet bitsetWarmup;
	private final ActorRef<PongActor.Command> pongAct;
	private final long[] rtts;
	private long ping_size = 0;
	private long pong_size = 0;

	public PingActor(ActorContext<Command> context, ActorRef<PongActor.Command> pongAct, int limit, int limitWarmup, int payload) {
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
		this.payload = payload;

		this.start();
	}

	private static int[] fresh_payload(int payload){
		int[] tmp = new int[payload];
		for(int i = 0; i<payload; i++)
			tmp[i] = i;
		return tmp;
	}

	private void store_data(){
		Gson gson = new Gson();


		HashMap<String,  Object> res = new HashMap();
		res.put("ping_size",  this.ping_size);
		res.put("pong_size",  this.pong_size);
		res.put("rtts",  this.rtts);


		try {
			FileWriter fileWriter =new FileWriter("results.json"); 
			gson.toJson(res, fileWriter);
			fileWriter.close();
		} catch (IOException e ){
			System.err.println(e.toString());
		}
	}

	private void start() {
		System.err.println("AKKA PingPong warmup");
		for (int i=0; i<limitWarmup; i++) {
			Ping msg = new Ping(i, true, fresh_payload(this.payload), getContext().getSelf(), System.currentTimeMillis());
			this.pongAct.tell(msg);	
		}

		if (limitWarmup == 0)
			this.run();
	}

	private void run(){
		System.err.println("VKKA Massive Tell started...");
		this.start = System.currentTimeMillis();
		for (int i=0; i<limit; i++) {
			Ping msg = new Ping(i, false, fresh_payload(this.payload), getContext().getSelf(), System.currentTimeMillis());
			if (this.ping_size==0)
				this.ping_size = com.varda.objectsize.InstrumentationAgent.getSerializedSize(getContext().getSystem(), msg);

			assert(msg.payload.length == this.payload);
			this.pongAct.tell(msg);	
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
			if (this.pong_size == 0)
				this.pong_size = com.varda.objectsize.InstrumentationAgent.getSerializedSize(getContext().getSystem(), pong);

			// Calcull RTT
			long endTimestamp = System.currentTimeMillis();
			long rtt = endTimestamp - pong.initTimestamp;
			this.rtts[pong.i] = rtt;

			if (bitset.isEmpty()) {
				long end = System.currentTimeMillis();
				getContext().getLog().info("Time elapse " + (end - start) +" ms");
				this.store_data();

				getContext().getLog().info("Terminated ueyiqu8R");
				getContext().getLog().info("JamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglog");
				getContext().getSystem().terminate();	
			}
		}
		return Behaviors.same();
	}
}