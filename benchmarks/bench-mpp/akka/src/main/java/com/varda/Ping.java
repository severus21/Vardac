package com.varda;

import akka.actor.typed.ActorRef;

public class Ping implements PongActor.Command {
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