package com.varda;

import akka.actor.typed.ActorRef;
import akka.actor.typed.ActorSystem;
import akka.actor.typed.Behavior;
import akka.actor.typed.javadsl.*;
import org.apache.commons.cli.*;
import com.typesafe.config.Config;
import akka.actor.typed.receptionist.Receptionist;

public class KVLStore {

	public static void main(String[] args) throws InterruptedException {

        Options options = new Options();
        Option nIterationsOpt = new Option("n", "niterations", true, "number of iterations");
        Option nWarmupIterationsOpt = new Option("warmup", "nwarmupiterations", true, "number of iterations for warmup");
		options.addOption(nIterationsOpt);
		options.addOption(nWarmupIterationsOpt);

        CommandLineParser parser = new DefaultParser();
        HelpFormatter formatter = new HelpFormatter();
        CommandLine cmd = AbstractMain.get_cmd(options, args)._1;

        assert (null != cmd);
        if(!cmd.hasOption("n") || !cmd.hasOption("warmup") ){
            System.out.println("niterations/nwarmupiterations argument is mandatory!");
            formatter.printHelp("g", options);
            System.exit(1);
        }

		int limit = Integer.parseInt(cmd.getOptionValue("n"));
		int limitWarmup = Integer.parseInt(cmd.getOptionValue("warmup"));

        System.out.println("KVClientKVShard SERVICE STARTED");
		final ActorSystem system = ActorSystem.create(
			KVLSystem.create(limit, limitWarmup), 
			AbstractMain.SYSTEM_NAME, 
            AbstractMain.get_config(cmd));

        if( limit > 0)
            system.getWhenTerminated().toCompletableFuture().join();
	}

    static class KVLSystem extends AbstractBehavior<KVCommand.Command> {
        static public Behavior<KVCommand.Command> create(int limit, int limitWarmup) {
            System.out.println(("Creating KVLSystem"));
            return Behaviors.setup(context -> new KVLSystem(context, limit, limitWarmup));
        }

        int registered = 0;
        ActorRef<KVCommand.Command> lb;
        int limit;
        int limitWarmup;

        public KVLSystem(ActorContext context, int limit, int limitWarmup){
            super(context);
            this.limit = limit;
            this.limitWarmup = limitWarmup;

            System.out.println(("Starting KVLSystem"));
            ActorRef<KVCommand.Command> shard1 = context.spawn(ShardActor.create(), "shard-1");
            ActorRef<KVCommand.Command> shard2 = context.spawn(ShardActor.create(), "shard-2");

            this.lb     = context.spawn(LoadbalancerActor.create(), "lb");
            context.getSystem().receptionist().tell(Receptionist.register(KVCommand.SERVICE_KEY, this.lb));

            this.registered = 0;
            lb.tell(new KVCommand.RegisterShardRequest(shard1, this.getContext().getSelf()));
            lb.tell(new KVCommand.RegisterShardRequest(shard2, this.getContext().getSelf()));
        }

        @Override
        public Receive<KVCommand.Command> createReceive() {
            return newReceiveBuilder().onMessage(KVCommand.RegisterShardResponse.class, this::onRegisterShardResponse).build();
        }

        private Behavior<KVCommand.Command> onRegisterShardResponse(KVCommand.RegisterShardResponse msg) {
            System.out.println("onRegisterShardResponse");
            this.registered += 1;

            if(this.registered == 2 && this.limit > 0){ //limit ==0 => do nothing and wait for external client
                ActorRef<KVCommand.Command> client = this.getContext().spawn(ClientActor.create(this.lb, this.limit, this.limitWarmup), "client");
            }
            return Behaviors.same();
        }
    } 
}