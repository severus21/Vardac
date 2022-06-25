
1. Generate code
    ```bash
    1> make run -- compile --places examples/test-grpc/places.yml --targets examples/test-grpc/targets.yml --filename examples/test-grpc/test.varch --impl examples/test-grpc/test.vimpl --provenance 0
    ```
1. ```1,2,3> cd compiler-build/akka```
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
    1> java -enableassertions -jar build/libs/main.jar -ip 127.0.0.1 -p 25520 -s akka://systemProject_name@127.0.0.1:25520 -l 8080 -vp placeB 
    ```
    1. Run the gRPC server
    ```bash
    2> java -enableassertions -jar build/libs/gRPCServer.jar -ip 127.0.0.1 -p 25521 -s akka://systemProject_name@127.0.0.1:25520 -l 8080 -vp placeB 
    ```
    1. Run the console client (which is using the gRPC client under the hood)
    ```bash
    3> java -enableassertions -jar build/libs/console.jar
    ```
    1. Connect the console to the gRPC server
    ```bash
    3>connect 127.0.0.1 8090
    ```
    where 127.0.0.1 is the ip of the server and 8090 its port

Note: ```i>``` denotes the shell identity
