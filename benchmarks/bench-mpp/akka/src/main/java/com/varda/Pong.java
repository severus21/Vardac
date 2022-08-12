package com.varda;

import akka.actor.typed.ActorRef;

public class Pong implements PingActor.Command {
	public int i;
	public boolean warmup;
	public ActorRef replyTo;
	public long initTimestamp;
	public Pong(int i, boolean warmup, ActorRef replyTo, long initTimestamp){
		this.i = i;
		this.warmup = warmup;
		this.replyTo = replyTo;
		this.initTimestamp = initTimestamp;
	}
}