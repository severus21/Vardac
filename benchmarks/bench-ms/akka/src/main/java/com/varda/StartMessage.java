package com.varda;

import akka.actor.typed.ActorRef;

public class StartMessage implements SorterActor.Command {
	public int[] array;

	public int i;
	public boolean warmup;
	public ActorRef replyTo;
	public long initTimestamp;

	public StartMessage(int[] array, int i, boolean warmup, ActorRef replyTo, long initTimestamp){
		this.array = array;

		this.i = i;
		this.warmup = warmup;
		this.replyTo = replyTo;
		this.initTimestamp = initTimestamp;
	}
}