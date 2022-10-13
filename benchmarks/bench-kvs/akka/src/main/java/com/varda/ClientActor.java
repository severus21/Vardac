package com.varda;

import akka.actor.typed.ActorRef;
import akka.actor.typed.Behavior;
import akka.actor.typed.javadsl.*;
import com.google.gson.Gson;
import com.varda.KVCommand.GetRequest;
import com.varda.KVCommand.GetResult;
import com.varda.KVCommand.PutResult;
import java.io.FileWriter;
import java.io.IOException;
import java.util.*;
import java.util.function.*;

public class ClientActor extends AbstractBehavior<KVCommand.Command> {

	static public Behavior<KVCommand.Command> create(Function<ActorContext, ActorRef<KVCommand.Command>> pongFactory, int limit, int limitWarmup) {
		return Behaviors.setup(context -> new ClientActor(context, pongFactory.apply(context), limit, limitWarmup));
	}

	static public Behavior<KVCommand.Command> create(ActorRef<KVCommand.Command> shard, int limit, int limitWarmup) {
		return Behaviors.setup(context -> new ClientActor(context, shard, limit, limitWarmup));
	}

	long start;
	private final int limit;
	private int fill_n;/* #bench */
	private final int limitWarmup;/* #bench */
	private int fill_warmup_n;/* #bench */
	private final ActorRef<KVCommand.Command> pongAct;

	private long walltime;/* #bench */
	private final long[] rtts;/* #bench */

	public ClientActor(ActorContext<KVCommand.Command> context, ActorRef<KVCommand.Command> pongAct, int limit, int limitWarmup) {
		super(context);
		assert(pongAct != null);
		this.limit = limit;
		this.fill_n = 0; /* #bench  */
		this.limitWarmup = limitWarmup; /* #bench */
		this.fill_warmup_n = 0; /* #bench  */
		this.pongAct = pongAct;
		this.rtts = new long[limit];/* #bench */
		this.walltime = 0;

		this.start();
	}

	private void store_data(){/* #bench */
		Gson gson = new Gson();/* #bench */


		HashMap<String,  Object> res = new HashMap();/* #bench */
		res.put("walltime",  this.walltime);/* #bench */
		res.put("getrtts",  this.rtts);/* #bench */


		try {/* #bench */
			FileWriter fileWriter =new FileWriter("results.json"); /* #bench */
			gson.toJson(res, fileWriter);/* #bench */
			fileWriter.close();/* #bench */
		} catch (IOException e ){/* #bench */
			System.err.println(e.toString());/* #bench */
		}/* #bench */
	}/* #bench */

	private void start() {/* #bench */
		System.err.println("AKKA GetRequestGetResult warmup "+this.limitWarmup);/* #bench */
		for (int i=0; i<limitWarmup; i++) {/* #bench */
			GetRequest msg = new GetRequest(Integer.valueOf(i).toString(),  getContext().getSelf(), System.currentTimeMillis());/* #bench */
			this.pongAct.tell(msg);	/* #bench */
		}/* #bench */

		if (limitWarmup == 0)/* #bench */
			this.run();/* #bench */
	}/* #bench */

	private void run(){
		System.err.println("VKKA Massive Tell started...");
		this.start = System.currentTimeMillis();/* #bench */
		for (int i=0; i<limit; i++) {
			GetRequest msg = new GetRequest(Integer.valueOf(i).toString(),  getContext().getSelf(), System.currentTimeMillis());/* #bench */
			this.pongAct.tell(msg);	
		}
	}

	@Override
	public Receive<KVCommand.Command> createReceive() {
		return newReceiveBuilder()
			.onMessage(GetResult.class, this::onGetResult)
			.onMessage(PutResult.class, this::onPutResult)
			.build();
	}

	private Behavior<KVCommand.Command> onPutResult(PutResult msg){
		System.out.println("Receive put value");
		return Behaviors.same();
	}

	private Behavior<KVCommand.Command> onGetResult(GetResult pong) {
		if(this.fill_warmup_n < this.limitWarmup) {/* #bench */
			this.fill_warmup_n += 1;

			if(this.fill_warmup_n == this.limitWarmup)
				this.run();
		} else {/* #bench */
			// Calcull RTT
			long endTimestamp = System.currentTimeMillis();/* #bench */
			long rtt = endTimestamp - pong.initTimestamp;/* #bench */
			this.rtts[this.fill_n] = rtt;/* #bench */
			
			this.fill_n += 1;

			if (this.fill_n == this.limit) {
				long end = System.currentTimeMillis();/* #bench */
				this.walltime = end-start;
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