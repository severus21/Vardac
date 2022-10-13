package com.varda;

import akka.actor.typed.ActorRef;

public class KVCommand {

	public interface Command {}

    public static class RegisterShardRequest implements  Command {
        public ActorRef<Command> shard;
        public ActorRef replyTo;
        public RegisterShardRequest(ActorRef<Command> shard, ActorRef replyTo){
            this.shard = shard;
            this.replyTo = replyTo;
        }
    }

    public static class RegisterShardResponse implements  Command {
        public RegisterShardResponse(){ }
    }

	static public class GetRequest implements Command {
		public String key;
		public ActorRef replyTo;/* #bench */
		public long initTimestamp;
		public GetRequest(String key, ActorRef replyTo, long initTimestamp){
			this.key = key;
			this.replyTo = replyTo;
			this.initTimestamp = initTimestamp;
		}
	}

	static public class GetResult implements Command {
		public String key;
		public String value;
		public ActorRef replyTo;
		public long initTimestamp;
		public GetResult(String key, String value, ActorRef replyTo, long initTimestamp){
			this.key = key;
			this.value = value;
			this.replyTo = replyTo;
			this.initTimestamp = initTimestamp;
		}
	}

	static public class PutRequest implements Command {
		public String key;
		public String value;
		public ActorRef replyTo;/* #bench */
		public long initTimestamp;
		public PutRequest(String key, String value, ActorRef replyTo, long initTimestamp){
			this.key = key;
			this.value = value;
			this.replyTo = replyTo;
			this.initTimestamp = initTimestamp;
		}
	}

	static public class PutResult implements Command {
		public String key;
		public Boolean flag;
		public ActorRef replyTo;
		public long initTimestamp;
		public PutResult(String key, Boolean flag, ActorRef replyTo, long initTimestamp){
			this.key = key;
			this.flag = flag;
			this.replyTo = replyTo;
			this.initTimestamp = initTimestamp;
		}
	}
}