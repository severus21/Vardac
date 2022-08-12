package com.varda;


import akka.actor.typed.ActorSystem;
import akka.actor.typed.ActorRef;
import akka.actor.typed.Behavior;
import akka.actor.typed.javadsl.*;

import java.util.BitSet;
import java.util.Arrays;
import java.util.function.*;
import java.io.FileWriter;
import java.io.IOException;

import com.google.gson.Gson;
import org.apache.commons.cli.*;

public class MasterActor extends AbstractBehavior<MasterActor.Command> {

	public interface Command extends CborSerializable {}

	static public Behavior<Command> create(int n, int limitWarmup, int[] input) {
		return Behaviors.setup(context -> new MasterActor(context, n, limitWarmup, input));
	}

	long start;
	private final int limit;
	private final int limitWarmup;
	private final BitSet bitset;
	private final BitSet bitsetWarmup;
	private final long[] rtts;

	private final int[] input;

	public MasterActor(ActorContext<Command> context, int limit, int limitWarmup, int[] input) {
		super(context);
		this.limit = limit;
		this.bitset = new BitSet(limit);
		this.limitWarmup = limitWarmup;
		this.bitsetWarmup = new BitSet(limitWarmup);
		bitset.set(0, limit);
		bitsetWarmup.set(0, limitWarmup);
		this.rtts = new long[limit];

		this.input = input;

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
		ActorContext context = getContext();
		System.err.println("AKKA PingPong warmup");

		for (int i=0; i<limitWarmup; i++) {
			ActorRef<SorterActor.Command> sorter = context.spawn(SorterActor.create(-1), "runner");
			sorter.tell(new StartMessage(this.input, i, true, getContext().getSelf(), System.currentTimeMillis()));	
		}
		if (limitWarmup == 0)
			this.run();
	}

	private void run(){
		ActorContext context = getContext();
		System.err.println("VKKA Massive Tell started...");

		this.start = System.currentTimeMillis();
		for (int i=0; i<limit; i++) {
			ActorRef<SorterActor.Command> sorter = context.spawn(SorterActor.create(-1), "runner");
			sorter.tell(new StartMessage(this.input, i, false, context.getSelf(), System.currentTimeMillis()));	
		}
	}

	@Override
	public Receive<Command> createReceive() {
		return newReceiveBuilder()
			.onMessage(ResultMessage.class, this::onResult)
			.build();
	}
	
	public boolean is_sort(int[] array){
		if(array.length == 0)
			return true;

		int last = array[0];
		for(int i: array){
			if(last > i)
				return false;
			last = i;
		}

		return true;
	}

	public void print_array(int[] input_array, int[] result_array){
		String buffer = "\n\n\ninput_vector = [";
		for(int i: input_array){
			buffer += i+",";	
		}
		buffer += "]";
		buffer += "\nsorted_vector = [";
		for(int i: result_array){
			buffer += i+",";	
		}
		buffer += "]\n\n\n";
		getContext().getLog().debug(buffer);
	}

	private Behavior<Command> onResult(ResultMessage result) {
		assert(is_sort(result.array));
		print_array(this.input, result.array);

		if(result.warmup) {
			bitsetWarmup.clear(result.i);

			// Warmup is over, run the bench
			if (bitsetWarmup.isEmpty()) {
				this.run();
			}
		} else {
			bitset.clear(result.i);

			// Calcull RTT
			long endTimestamp = System.currentTimeMillis();
			long rtt = endTimestamp - result.initTimestamp;
			this.rtts[result.i] = rtt;

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