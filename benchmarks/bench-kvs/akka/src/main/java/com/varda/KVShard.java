package com.varda;

import akka.actor.typed.ActorSystem;
import com.typesafe.config.Config;
import io.vavr.Tuple2;
import org.apache.commons.cli.*;

public class KVShard {

	public static void main(String[] args) throws InterruptedException {
        Options options = new Options();

        Tuple2<CommandLine, HelpFormatter> res = AbstractMain.get_cmd(options, args);
        CommandLine cmd = res._1;
        HelpFormatter formatter = res._2;

		run(AbstractMain.get_config(cmd));

	}

	public static void run(Config config) throws InterruptedException {
        System.out.println("KVShard SERVICE STARTED");
		final ActorSystem system = ActorSystem.create(
			ShardActor.create(), 
			AbstractMain.SYSTEM_NAME,
            config);

		system.getWhenTerminated().toCompletableFuture().join();
	}
	
	

	
}