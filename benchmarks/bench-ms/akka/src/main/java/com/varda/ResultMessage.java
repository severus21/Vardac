package com.varda;

import akka.actor.typed.ActorRef;

public class ResultMessage implements MasterActor.Command, SorterActor.Command {
	public int[] array;
	public int side;

	public int i;
	public boolean warmup;
	public ActorRef replyTo;
	public long initTimestamp;

	public ResultMessage(int[] array, int side, int i, boolean warmup, ActorRef replyTo, long initTimestamp){
		this.array = array;
		this.side  = side;

		this.i = i;
		this.warmup = warmup;
		this.replyTo = replyTo;
		this.initTimestamp = initTimestamp;
	}
}