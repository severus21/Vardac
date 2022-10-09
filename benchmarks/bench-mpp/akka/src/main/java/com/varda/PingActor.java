package com.varda;


import akka.actor.typed.ActorRef;
import akka.actor.typed.Behavior;
import akka.actor.typed.javadsl.*;
import com.google.gson.Gson;
import java.io.FileWriter;
import java.io.IOException;
import java.util.*;
import java.util.function.*;

public class PingActor extends AbstractBehavior<PingActor.Command> {

	public interface Command extends CborSerializable {}

	static public Behavior<Command> create(Function<ActorContext, ActorRef<PongActor.Command>> pongFactory, int n, int limitWarmup, int payload) {
		return Behaviors.setup(context -> new PingActor(context, pongFactory.apply(context), n, limitWarmup, payload));
	}

	long start;
	private final int limit;
	private final int limitWarmup;/* #bench */
	private final int payload;
	private final BitSet bitset;
	private final BitSet bitsetWarmup;/* #bench */
	private final ActorRef<PongActor.Command> pongAct;
	private final long[] rtts;/* #bench */
	private long ping_size = 0;/* #bench */
	private long pong_size = 0;/* #bench */

	public PingActor(ActorContext<Command> context, ActorRef<PongActor.Command> pongAct, int limit, int limitWarmup, int payload) {
		super(context);
		assert(pongAct != null);
		this.limit = limit;
		this.bitset = new BitSet(limit);
		this.limitWarmup = limitWarmup;/* #bench */
		this.bitsetWarmup = new BitSet(limitWarmup);/* #bench */
		bitset.set(0, limit);
		bitsetWarmup.set(0, limitWarmup);/* #bench */
		this.pongAct = pongAct;
		this.rtts = new long[limit];/* #bench */
		this.payload = payload;

		this.start();
	}

	private static int[] fresh_payload(int payload){
		int[] tmp = new int[payload];
		for(int i = 0; i<payload; i++)
			tmp[i] = i;
		return tmp;
	}

	private void store_data(){/* #bench */
		Gson gson = new Gson();/* #bench */


		HashMap<String,  Object> res = new HashMap();/* #bench */
		res.put("ping_size",  this.ping_size);/* #bench */
		res.put("pong_size",  this.pong_size);/* #bench */
		res.put("rtts",  this.rtts);/* #bench */


		try {/* #bench */
			FileWriter fileWriter =new FileWriter("results.json"); /* #bench */
			gson.toJson(res, fileWriter);/* #bench */
			fileWriter.close();/* #bench */
		} catch (IOException e ){/* #bench */
			System.err.println(e.toString());/* #bench */
		}/* #bench */
	}/* #bench */

	private void start() {/* #bench */
		System.err.println("AKKA PingPong warmup");/* #bench */
		for (int i=0; i<limitWarmup; i++) {/* #bench */
			Ping msg = new Ping(i, true, fresh_payload(this.payload), getContext().getSelf(), System.currentTimeMillis());/* #bench */
			this.pongAct.tell(msg);	/* #bench */
		}/* #bench */

		if (limitWarmup == 0)/* #bench */
			this.run();/* #bench */
	}/* #bench */

	private void run(){
		System.err.println("VKKA Massive Tell started...");
		this.start = System.currentTimeMillis();/* #bench */
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
		if(pong.warmup) {/* #bench */
			bitsetWarmup.clear(pong.i);/* #bench */

			// Warmup is over, run the bench
			if (bitsetWarmup.isEmpty()) {/* #bench */
				this.run();/* #bench */
			}/* #bench */
		} else {/* #bench */
			bitset.clear(pong.i);
			if (this.pong_size == 0)/* #bench */
				this.pong_size = com.varda.objectsize.InstrumentationAgent.getSerializedSize(getContext().getSystem(), pong);/* #bench */

			// Calcull RTT
			long endTimestamp = System.currentTimeMillis();/* #bench */
			long rtt = endTimestamp - pong.initTimestamp;/* #bench */
			this.rtts[pong.i] = rtt;/* #bench */

			if (bitset.isEmpty()) {
				long end = System.currentTimeMillis();/* #bench */
				getContext().getLog().info("Time elapse " + (end - start) +" ms");/* #bench */
				this.store_data();/* #bench */

				getContext().getLog().info("Terminated ueyiqu8R");/* #bench */
				getContext().getLog().info("JamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglogJamminglog");/* #bench */
				getContext().getSystem().terminate();	
			}
		}/* #bench */
		return Behaviors.same();
	}
}