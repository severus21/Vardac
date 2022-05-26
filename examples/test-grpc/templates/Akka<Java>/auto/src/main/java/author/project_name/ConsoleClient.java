package {{author}}.{{project_name}};

import akka.actor.typed.ActorSystem;
import com.typesafe.config.Config;
import com.typesafe.config.ConfigFactory;
import org.jline.reader.*;
import org.jline.reader.impl.DefaultParser;
import org.jline.reader.impl.LineReaderImpl;
import org.jline.terminal.Terminal;
import org.jline.terminal.TerminalBuilder;
import org.jline.utils.InfoCmp;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

public class ConsoleClient {

    private static class State {
        {{grpc_client}} grpcClient = null;
    }

    private static class StateException extends RuntimeException {
        StateException(String s) {
            super(s);
        }
    }

    public static void main(String[] args) throws IOException {
        System.setProperty("org.jline.terminal.dumb", "true");

        State currentState = new State();

        // check if a connection can be established from passed arguments
        if(args.length == 3) {
            // eg: ./console hostname seedIp seedPort
            // eg: ./console 127.0.0.1 127.0.0.1 25520
            currentState.grpcClient = connect(args[0], args[1], args[2]);
        }

        TerminalBuilder builder = TerminalBuilder.builder();
        Completer completer = null;
        Parser parser = new DefaultParser();
        Terminal terminal = builder.build();

//        System.out.println(terminal.getName() + ": " + terminal.getType());
        System.out.println("\nhelp: list available commands");

        LineReader reader = LineReaderBuilder.builder()
                .terminal(terminal)
                .completer(completer)
                .parser(parser)
//                .variable(LineReader.SECONDARY_PROMPT_PATTERN, "%M%P > ")
                .variable(LineReader.INDENTATION, 2)
                .option(LineReader.Option.INSERT_BRACKET, true)
                .build();

        terminal.writer().println("-----------------------------");
        terminal.writer().println("\t {{project_name}} Console");
        terminal.writer().println("-----------------------------");
        terminal.flush();

        Executors.newScheduledThreadPool(1)
                .scheduleAtFixedRate(() -> {
                    try {
                        if (null == currentState.grpcClient) {
                            rightPrompt = "NOT connected";
                        } else {
                            rightPrompt = "connected";
                        }

                        // Note: this is a hack !
                        ((LineReaderImpl) reader).setRightPrompt(rightPrompt);

                        reader.callWidget(LineReader.CLEAR);
//                        reader.getTerminal().writer().println("Hello world!");
                        reader.callWidget(LineReader.REDRAW_LINE);
                        reader.callWidget(LineReader.REDISPLAY);
                        reader.getTerminal().writer().flush();
                    } catch (IllegalStateException e) {
                        // Widgets can only be called during a `readLine` call
                    }
                }, 1, 1, TimeUnit.SECONDS);

        while (true) {
            String line = null;
            try {
                line = reader.readLine(prompt, rightPrompt, (MaskingCallback) null, null);
                line = line.trim();

//                terminal.writer().println("======>\"" + line + "\"");
//                terminal.writer().println(
//                        AttributedString.fromAnsi("\u001B[33m======>\u001B[0m\"" + line + "\"")
//                                .toAnsi(terminal));
//                terminal.flush();

                if (line.equalsIgnoreCase("quit") || line.equalsIgnoreCase("exit")) {
                    exit(currentState, 0);
                    break;
                }
                ParsedLine pl = reader.getParser().parse(line, 0);

                if ("connect".equals(pl.word())) {
                    if (pl.words().size() == 4) {
                        currentState.grpcClient = connect(pl.words().get(1), pl.words().get(2), pl.words().get(3));
                    } else {
                        terminal.writer().println("Usage: connect <hostname> <ip> <port>");
                    }
                } else if ("disconnect".equals(pl.word())) {
                    if (null != currentState.grpcClient) {
                        // disconnect
                        currentState.grpcClient.disconnect();

                        // set state to null
                        currentState.grpcClient = null;
                    }
                } else if ("api_a".equals(pl.word())) {
                    checkState(currentState, true);
                    terminal.writer().println("TODO api_a");
                } else if ("api_b".equals(pl.word())) {
                    checkState(currentState, true);

                    if (pl.words().size() == 2) {
                        String key = pl.words().get(1);
                        terminal.writer().println("TODO api_b");
                    } else {
                        terminal.writer().println("Usage: api_b <string>");
                    }
                } else if ("api_c".equals(pl.word())) {
                    checkState(currentState, true);

                    if (pl.words().size() == 3) {
                        String key = pl.words().get(1);
                        Integer i = Integer.parseInt(pl.words().get(2));
                        terminal.writer().println("TODO api_c");
                    } else {
                        terminal.writer().println("Usage: put <key> <value>");
                    }
                } else if ("clear".equals(pl.word())) {
                    terminal.puts(InfoCmp.Capability.clear_screen);
                } else if ("status".equals(pl.word())) {
                    if (null == currentState.grpcClient) {
                        terminal.writer().println("Not connected to KVS");
                    } else {
                        terminal.writer().println("Connected");
                    }
                } else if ("help".equals(pl.word()) || "?".equals(pl.word())) {
                    help();
                }
            } catch (IllegalArgumentException e) {
                System.out.println(e.getMessage());
            } catch (UserInterruptException e) {
                // Ignore
            } catch (EndOfFileException e) {
                return;
            } catch (StateException e) {
                terminal.writer().println(e.getMessage());
                terminal.flush();
            }
        }
    }

    public static void help() {
        String[] help = {
                "List of available commands:"
                , "  Builtin:"
                , "    connect      connect to {{system_name}}"
                , "    disconnect   disconnect from {{system_name}}"
                , "    status       display status information"
                , "    exit|quit    exit"
                , "  Example:"
                , "    connect      connect 127.0.0.1 25520"
                , "  Additional help:"
                , "    <command> --help"};
        for (String u : help) {
            System.out.println(u);
        }
    }

    public static void exit(State state, int statusCode) {
        if (null != state.grpcClient) {
            state.grpcClient.disconnect();
        }
        System.exit(statusCode);
    }

    public static {{grpc_client}} connect(String hostname, String ip, String port) {
        assert (null != ip && !ip.isEmpty());
        assert (null != port && !port.isEmpty());

        //Config config = ConfigFactory.parseString(
        //        "akka.cluster.jmx.multi-mbeans-in-same-jvm=on \n" +
        //                "akka.remote.artery.canonical.hostname=" + hostname + "\n" +
        //                "akka.remote.artery.canonical.port=0 \n" +
        //                "akka.cluster.roles=[\"client\"] \n" +
        //                "akka.cluster.seed-nodes=[\"akka://KVSSystem@" + ip + ":" + port + "\"] \n");

        return new {{grpc_client}}(ip, Integer.parseInt(port));
    }

    private static void checkState(State state, boolean client) {
        if (client && null == state.grpcClient) {
            throw new StateException("Not connected to {{project_name}} !");
        }
    }

    private static final String prompt = "\n{{project_name}}> ";
    private static String rightPrompt = "not connected";
}