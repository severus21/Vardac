package com.varda;

import com.typesafe.config.Config;
import com.typesafe.config.ConfigFactory;
import org.apache.commons.cli.*;

import java.io.IOException;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.CompletionStage;

import io.vavr.Tuple2;
 

public class AbstractMain {
    // Load config from env or cmd
    public static final String SYSTEM_NAME = "systemAkkaBench";
    private static final String DEFAULT_CLUSTER_IP = System.getenv("HOSTNAME") == null ? "127.0.0.1" : System.getenv("HOSTNAME");
    private static final String DEFAULT_CLUSTER_PORT = "25520";
    private static final String DEFAULT_SEED = "akka://"+SYSTEM_NAME+"@" + DEFAULT_CLUSTER_IP + ":25520";

    public static Tuple2<CommandLine, HelpFormatter> get_cmd(Options options, String[] args){
        Option clusterIp = new Option("ip", "clusterip", true, "cluster ip");
        Option clusterPort = new Option("p", "clusterport", true, "cluster port");
        Option seed = new Option("s", "seed-nodes", true, "seed nodes");
//        input.setRequired(true);
        options.addOption(clusterIp);
        options.addOption(clusterPort);
        options.addOption(seed);

        CommandLineParser parser = new DefaultParser();
        HelpFormatter formatter = new HelpFormatter();
        CommandLine cmd = null;

        try {
            cmd = parser.parse(options, args);
            assert (null != cmd);
        } catch (ParseException e) {
            System.out.println(e.getMessage());
            formatter.printHelp("akkaBench", options);
            System.exit(1);
        }

        return new Tuple2(cmd, formatter);
    }

    public static Config get_config(CommandLine cmd) {
        Map<String, Object> overrides = new HashMap<>();
        overrides.put("akka.remote.artery.canonical.hostname", cmd.getOptionValue("clusterip", DEFAULT_CLUSTER_IP));
        overrides.put("akka.remote.artery.canonical.port", cmd.getOptionValue("clusterport", DEFAULT_CLUSTER_PORT));
        overrides.put("akka.cluster.seed-nodes", Arrays.asList(cmd.getOptionValue("seed-nodes", DEFAULT_SEED).split(",")));
        Config config = ConfigFactory.parseMap(overrides).withFallback(ConfigFactory.load());

        return config;
    }
}
