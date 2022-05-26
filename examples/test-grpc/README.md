1. Generate code
```bash
make run -- compile --places examples/test-grpc/places.yml --targets examples/test-grpc/targets.yml --filename examples/test-grpc/test.spec --impl examples/test-grpc/test.impl --provenance 0
```
2. ```cd compiler-build/akka```
2. Build Varda targets
```bash
make
```
3. Build Console binary 
```bash
gradle build jarConsole
```
4. Run example
    1. Run the Varda system (composed of one binary)
    ```bash
    java -jar build/libs/MaingRPCServer144.jar -ip 127.0.0.1 -p 25520 -s akka://systemProject_name@127.0.0.1:25520 -l 8080 -vp placeB 
    ```
    2. Run the gRPC server
    ```bash
    java -jar build/libs/main.jar -ip 127.0.0.1 -p 25521 -s akka://systemProject_name@127.0.0.1:25520 -l 8080 -vp placeB 
    ```
    3. Run the console client (which is using the gRPC client under the hood)
    ```bash
    java -jar build/libs/console.jar
    ```
