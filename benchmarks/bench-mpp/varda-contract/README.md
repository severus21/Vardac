
1. Generate code
    ```bash
    1> make run -- compile --places benchmarks/bench-mpp/varda-contract/places.yml --targets benchmarks/bench-mpp/varda-contract/targets.yml --filename benchmarks/bench-mpp/varda-contract/bench.varch --impl benchmarks/libbench.vimpl --impl benchmarks/bench-mpp/varda-contract/bench.vimpl --provenance 0
    ```
1. ```1> cd compiler-build/akka```
1. Build Varda targets
    ```bash
    1> make
    ```
1. Run example
    1. Run the Varda system (composed of one binary)
    ```bash
    1> java -javaagent:build/libs/InstrumentationAgent.jar  -enableassertions -jar build/libs/main.jar -ip 127.0.0.1 -p 25520 -s akka://systemProject_name@127.0.0.1:25520 -l 8080 -vp placeB -n 100 -warmup 100 -payload 10
    ```
    where
    -n is the number of round of ping pong