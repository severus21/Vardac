## Mono JVM setup
```make```
```java -javaagent:build/libs/InstrumentationAgent.jar -enableassertions -jar build/libs/main.jar -n 100 -warmup 100 -payload 10```

## Two JVM setup

1. ```make```
2. ```1> java -javaagent:build/libs/InstrumentationAgent.jar -enableassertions -jar build/libs/pongService.jar -ip 127.0.0.1 -p 25520 -s akka://systemAkkaBench@127.0.0.1:25520```
3. ```2> java -javaagent:build/libs/InstrumentationAgent.jar -enableassertions -jar build/libs/pingService.jar -ip 127.0.0.1 -p 25521 -s akka://systemAkkaBench@127.0.0.1:25520 -n 100 -warmup 100 -payload 10```

This custom Akka parameters for atery
``outbound-message-queue-size = N`` where N should be > than the number of n parameters since this test does implement back-pressure
