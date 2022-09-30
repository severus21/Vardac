
1. Generate code
    ```bash
    1> make run -- compile --places benchmarks/bench-ms/varda/places.yml --targets benchmarks/bench-ms/varda/targets.yml --filename benchmarks/bench-ms/varda/bench.varch --impl benchmarks/libbench.vimpl --impl benchmarks/bench-ms/varda/bench.vimpl --provenance 0
    ```
    N.B. disable reflexivity to avoid waiting at activation creation "--disable-global-placement-reflexivity --disable-global-placement-reflexivity"
1. ```1> cd compiler-build/akka```
1. Build Varda targets
    ```bash
    1> make
    ```
1. Run example
    1. Run the Varda system (composed of one binary)
    ```bash
    1> java -enableassertions -jar build/libs/main.jar -ip 127.0.0.1 -p 25520 -s akka://systemProject_name@127.0.0.1:25520 -l 8080 -vp placeB -n 1 -warmup 0 -vs 3 
    ```
    where
    -n is the number of round of ping pong
    -vs size of the vector to sort