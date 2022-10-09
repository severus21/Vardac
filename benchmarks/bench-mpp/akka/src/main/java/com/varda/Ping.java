package com.varda;

import akka.actor.typed.ActorRef;

public class Ping implements PongActor.Command {
	public int i;
	public boolean warmup;
	public int[] payload;
	public ActorRef replyTo;// #bench
	public long initTimestamp;
	public Ping(int i, boolean warmup, int[] payload, ActorRef replyTo, long initTimestamp){
		this.i = i;
		this.warmup = warmup;
		this.payload = payload;
		this.replyTo = replyTo;
		this.initTimestamp = initTimestamp;
	}
}