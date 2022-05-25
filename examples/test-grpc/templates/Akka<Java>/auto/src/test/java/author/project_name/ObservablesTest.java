package {{author}}.{{project_name}};

import akka.actor.testkit.typed.javadsl.LoggingTestKit;
import akka.actor.testkit.typed.javadsl.ActorTestKit;
import akka.actor.typed.ActorSystem;
import akka.actor.typed.Behavior;
import akka.actor.typed.javadsl.AbstractBehavior;
import akka.actor.typed.javadsl.ActorContext;
import akka.actor.typed.ActorRef;
import akka.actor.typed.javadsl.Behaviors;
import akka.actor.typed.javadsl.Receive;
import com.typesafe.config.ConfigFactory;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

import org.slf4j.event.Level;

import java.util.*;
import io.vavr.*;
import io.vavr.control.*;

import com.lg4dc.*;
import com.lg4dc.timers.*;
import com.bmartin.*;

{% for guardian_name in target_entrypoints %}
    {% if substring(0,8,guardian_name) == "TopLevel"%}
import static org.junit.jupiter.api.Assertions.*;
public class ObservablesTest {
    ActorTestKit testKit = null;

    @BeforeEach
    public void setUp() {
        String[] args = new String[2];
        args[0] = "-vp";
        args[1] = "placeA";
        testKit = ActorTestKit.create("{{system_name}}", AbstractMain.get_config(args));
    }

    @AfterEach
    public void tearDown() {
        testKit.shutdownTestKit();
    }

    @Test
    public void check_logs() {
        class Counter {
            public int seen = 0;
            public int unseen = 0;
        }
        Counter counter = new Counter ();

        LoggingTestKit.messageContains(">")
        .withCustom(
        event ->{
            if(event.level() == Level.DEBUG){
                System.out.println(event.message());
                if(event.message().contains("> Seen")){
                    counter.seen += 1;
                    return true;
                }
                if(event.message().contains("> Unseen")){
                    counter.unseen += 1;
                    return true;
                }
            }
            return false;
        }
        )
        .withOccurrences​(2)
        .expect(
            testKit.system(),
            () -> {
                ActorRef<SpawnProtocol.Command> kvs1 = testKit.spawn({{guardian_name}}.create(true), "KVS1");
                try{
                    java.lang.Thread.sleep(1);
                }catch(InterruptedException e) {}

                return null;
            });

        assertEquals(counter.seen, 2);
        assertEquals(counter.unseen, 0);
    }
}
    {% endif %}
{% endfor %}
