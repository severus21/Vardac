## Mono JVM setup
```make```
```java -enableassertions -jar build/libs/main.jar -n 100 -warmup 100```

## Two JVM setup

1. ```make```
2. ```1> java -enableassertions -jar build/libs/pongService.jar -ip 127.0.0.1 -p 25520 -s akka://systemAkkaBench@127.0.0.1:25520```
3. ```2> java -enableassertions -jar build/libs/pingService.jar -ip 127.0.0.1 -p 25521 -s akka://systemAkkaBench@127.0.0.1:25520 -n 100 -warmup 100```