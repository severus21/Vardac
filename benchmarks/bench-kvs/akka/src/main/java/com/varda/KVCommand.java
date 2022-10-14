package com.varda;

import akka.actor.typed.ActorRef;
import akka.actor.typed.receptionist.Receptionist;
import akka.actor.typed.receptionist.ServiceKey;
public class KVCommand {
    public static final ServiceKey<Command> SERVICE_KEY
            = ServiceKey.create(Command.class, "key");


	public interface Command extends CborSerializable {}

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

	static public class DeleteRequest implements Command {
		public String key;
		public ActorRef replyTo;/* #bench */
		public long initTimestamp;
		public DeleteRequest(String key, ActorRef replyTo, long initTimestamp){
			this.key = key;
			this.replyTo = replyTo;
			this.initTimestamp = initTimestamp;
		}
	}

	static public class DeleteResult implements Command {
		public String key;
		public boolean flag;
		public ActorRef replyTo;
		public long initTimestamp;
		public DeleteResult(String key, boolean flag, ActorRef replyTo, long initTimestamp){
			this.key = key;
			this.flag = flag;
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