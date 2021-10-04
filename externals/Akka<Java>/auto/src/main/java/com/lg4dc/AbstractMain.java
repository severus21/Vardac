package com.lg4dc;

import com.typesafe.config.Config;
import com.typesafe.config.ConfigFactory;
import org.apache.commons.cli.*;

import java.io.IOException;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.CompletionStage;
 

public class AbstractMain {
    // Load config from env or cmd
    private static final String DEFAULT_CLUSTER_IP = System.getenv("HOSTNAME") == null ? "127.0.0.1" : System.getenv("HOSTNAME");
    private static final String DEFAULT_CLUSTER_PORT = "25520";
    private static final String DEFAULT_SEED = "akka://KVSSystem@" + DEFAULT_CLUSTER_IP + ":25520";
    private static final String DEFAULT_HTTP_PORT = "8080";

    public static Config get_config(String[] args) {
        Options options = new Options();
        Option clusterIp = new Option("ip", "clusterip", true, "cluster ip");
        Option clusterPort = new Option("p", "clusterport", true, "cluster port");
        Option seed = new Option("s", "seed-nodes", true, "seed nodes");
        Option httpListen = new Option("l", "http-listen", true, "http listen port");
//        input.setRequired(true);
        options.addOption(clusterIp);
        options.addOption(clusterPort);
        options.addOption(seed);
        options.addOption(httpListen);

        CommandLineParser parser = new DefaultParser();
        HelpFormatter formatter = new HelpFormatter();
        CommandLine cmd = null;

        try {
            cmd = parser.parse(options, args);
        } catch (ParseException e) {
            System.out.println(e.getMessage());
            formatter.printHelp("KeyValueStore", options);
            System.exit(1);
        }

        assert (null != cmd);
        Map<String, Object> overrides = new HashMap<>();
        overrides.put("akka.remote.artery.canonical.hostname", cmd.getOptionValue("clusterip", DEFAULT_CLUSTER_IP));
        overrides.put("akka.remote.artery.canonical.port", cmd.getOptionValue("clusterport", DEFAULT_CLUSTER_PORT));
        overrides.put("akka.cluster.seed-nodes", Arrays.asList(cmd.getOptionValue("seed-nodes", DEFAULT_SEED).split(",")));
        overrides.put("akka.cluster.roles", Collections.singletonList("backend"));
        Config config = ConfigFactory.parseMap(overrides).withFallback(ConfigFactory.load());

        return config;
    }
}
