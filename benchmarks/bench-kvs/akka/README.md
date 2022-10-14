## Mono JVM setup
```make```
```java -enableassertions -jar build/libs/main.jar -n 100 -warmup 100```
```java -enableassertions -jar build/libs/lmain.jar -n 100 -warmup 100```


1. Run the Varda system (composed of one binary)
```bash
1> java -enableassertions -jar build/libs/main.jar -ip 127.0.0.1 -p 25520 -s akka://systemAkkaBench@127.0.0.1:25520 -n 0 -warmup 0 
```
1. Run the gRPC server
```bash
2> java -enableassertions -jar build/libs/gRPCServer.jar -ip 127.0.0.1 -p 25521 -s akka://systemAkkaBench@127.0.0.1:25520 
```
1. Run the console client (which is using the gRPC client under the hood)
```bash
3> java -enableassertions -jar build/libs/console.jar
```
1. Connect the console to the gRPC server
```bash
3> connect 127.0.0.1 8090
```

## Two JVM setup

1. ```make```
2. ```1> java -enableassertions -jar build/libs/shardService.jar -ip 127.0.0.1 -p 25520 -s akka://systemAkkaBench@127.0.0.1:25520```
3. ```2> java -enableassertions -jar build/libs/clientService.jar -ip 127.0.0.1 -p 25521 -s akka://systemAkkaBench@127.0.0.1:25520 -n 100 -warmup 100```

This custom Akka parameters for atery
``outbound-message-queue-size = N`` where N should be > than the number of n parameters since this test does implement back-pressure


### Docker

1> docker-compose up -d --build 

  ```bash
        export YCSB=$(pwd -P)/ycsb-0.17.0
        gradle -x test jarYCSBClient

        java  -classpath build/libs/YCSBClient.jar:$YCSB/conf:$YCSB/lib/HdrHistogram-2.1.4.jar:$YCSB/lib/core-0.17.0.jar:$YCSB/lib/htrace-core4-4.1.0-incubating.jar:$YCSB/lib/jackson-core-asl-1.9.4.jar:$YCSB/lib/jackson-mapper-asl-1.9.4.jar site.ycsb.Client -load -db com.varda.YCSBClient -P $YCSB/workloads/workloada > outputVardaLoad.txt
  ```