package {{author}}.{{project_name}};

import akka.actor.testkit.typed.javadsl.LoggingTestKit;
import akka.actor.typed.ActorSystem;
import akka.actor.typed.Behavior;
import akka.actor.typed.javadsl.AbstractBehavior;
import akka.actor.typed.javadsl.ActorContext;
import akka.actor.typed.javadsl.Behaviors;
import akka.actor.typed.javadsl.Receive;
import com.typesafe.config.ConfigFactory;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.concurrent.ExecutionException;

{% for guardian_name in target_entrypoints %}
    {% if substring(0,8,guardian_name) == "TopLevel"%}
import static org.junit.jupiter.api.Assertions.*;
public class LogTest {
    ActorSystem system = null;

    @BeforeEach
    public void setUp() {
        system = ActorSystem.create({{guardian_name}}.create(), {{guardian_name}}.NAME);
    }

    @AfterEach
    public void tearDown() {
        //try {
            system.terminate();
            //system.getWhenTerminated().toCompletableFuture().get();
        //} catch (InterruptedException | ExecutionException e) {
        //    fail(e);
        //} finally {
            system = null;
        //}
    }

    @Test
    public void transactionEquality() {
        LoggingTestKit.info("")
        .withMessageRegex​(".*")
        .withOccurrences(2)
        .expect(
            system,
            () -> {
              return null;
            });
    }
}
    {% endif %}
{% endfor %}
