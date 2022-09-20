* Scenario 1 : without redis
    1. Generate code
        ```bash
        1> make run -- compile --places examples/3-minimal-simple-kv/places.yml --targets examples/3-minimal-simple-kv/targets.yml --filename examples/3-minimal-simple-kv/kv.varch --impl examples/3-minimal-simple-kv/kv-wo-redis.vimpl --provenance 0
        ```
    1. ```1> cd compiler-build/akka```
    1. Build artifacts
        1. Build Varda targets
            ```bash
            1> make
            ```
        1. Build Console binary 
            ```bash
            1> gradle build -x test jarConsole
            ```
    1. Run example
        1. Run the Varda system (composed of one binary)
        ```bash
        1> java -enableassertions -jar build/libs/local.jar -ip 127.0.0.1 -p 25520 -s akka://systemProject_name@127.0.0.1:25520 -l 8080 -vp placeA 
        ```
        1. Run the gRPC server
        ```bash
        2> java -enableassertions -jar build/libs/gRPCServer.jar -ip 127.0.0.1 -p 25521 -s akka://systemProject_name@127.0.0.1:25520 -l 8080 -vp placeA 
        ```
        1. Run the console client (which is using the gRPC client under the hood)
        ```bash
        3> java -enableassertions -jar build/libs/console.jar
        ```
        1. Connect the console to the gRPC server
        ```bash
        3> connect 127.0.0.1 8090
        3> get "key" #return -1 if the key is not in dom(KV) else return the value
        3> put "key" 5
        ```
    where 127.0.0.1 is the ip of the server and 8090 its port
2. Scenario 2 : with redis
    1. Generate code
        ```bash
        1> make run -- compile --places examples/3-minimal-simple-kv/places.yml --targets examples/3-minimal-simple-kv/targets.yml --filename examples/3-minimal-simple-kv/kv.varch --impl examples/3-minimal-simple-kv/kv-redis.vimpl --provenance 0
        ```
    1. ```1> cd compiler-build/akka```
    1. 
        ```bash 
        1> docker-compose up -d --build 
        1> list containers: docker ps -q --filter="ancestor=project_name-akka"
        1> docker container logs $(docker ps -q --filter="name=server-seed")
        1> docker run -it --network akka_default project_name-console "/usr/local/openjdk-11/bin/java -jar console.jar"
        1> docker-compose down 
        1> docker-compose rm 
        ```
    * To query the redis kv directly
        docker exec -it $(docker ps -q --filter name=redis) redis-cli


Debugbox:

* get ip : docker inspect $(docker ps -q --filter name="gateway") | grep IPAddress
* check grpc : docker run -it --network akka_default project_name-console "./grpc_health_probe -addr=gateway:8090"

3. Benchmark
    * Test YCSB client 
    ```bash
        java com.yahoo.ycsb.CommandLine -db author.project_name.YCSBClient -p kvs.url=localhost -p kvs.port=8090 
    ```

    ```bash
        curl -O --location https://github.com/brianfrankcooper/YCSB/releases/download/0.17.0/ycsb-0.17.0.tar.gz
        tar xfvz ycsb-0.17.0.tar.gz && rm ycsb-0.17.0.tar.gz
        export YCSB=$(pwd -P)/ycsb-0.17.0
    ```

    ```bash
        gradle -x test jarYCSBClient

        java  -classpath build/libs/YCSBClient.jar:$YCSB/conf:$YCSB/lib/HdrHistogram-2.1.4.jar:$YCSB/lib/core-0.17.0.jar:$YCSB/lib/htrace-core4-4.1.0-incubating.jar:$YCSB/lib/jackson-core-asl-1.9.4.jar:$YCSB/lib/jackson-mapper-asl-1.9.4.jar site.ycsb.Client -load -db author.project_name.YCSBClient -P $YCSB/workloads/workloada > outputVardaLoad.txt

        RESULT_DIR=xps-2019.workloadA.0/1
        mkdir -p $RESULT_DIR


        java  -classpath build/libs/YCSBClient.jar:$YCSB/conf:$YCSB/lib/HdrHistogram-2.1.4.jar:$YCSB/lib/core-0.17.0.jar:$YCSB/lib/htrace-core4-4.1.0-incubating.jar:$YCSB/lib/jackson-core-asl-1.9.4.jar:$YCSB/lib/jackson-mapper-asl-1.9.4.jar:$YCSB/redis-binding/lib/commons-pool2-2.4.2.jar:$YCSB/redis-binding/lib/jedis-2.9.0.jar:$YCSB/redis-binding/lib/redis-binding-0.17.0.jar site.ycsb.Client -t -db author.project_name.YCSBClient -s -P $YCSB/workloads/workloada > $RESULT_DIR/ycsb-results


        $YCSB/bin/ycsb.sh load redis -s -P $YCSB/workloads/workloada -p "redis.host=127.0.0.1" -p "redis.port=6379" > outputRedisLoad.txt
        $YCSB/bin/ycsb.sh run redis -s -P $YCSB/workloads/workloada -p "redis.host=127.0.0.1" -p "redis.port=6379" > outputRedisRun.txt
    ```