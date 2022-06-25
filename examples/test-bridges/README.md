1. Generate code
    ```bash
    make run -- compile --places examples/test-bridges/places.yml --targets examples/test-bridges/targets.yml --filename examples/test-bridges/test.varch --impl examples/test-bridges/test.vimpl --provenance 0
    ```
1. ```1> cd compiler-build/akka```
1. Build Varda targets
    ```bash
    1> make
    ```
1. Run example
    1. Run the Varda system (composed of one binary)
    ```bash
    1> java -enableassertions -jar build/libs/main.jar -ip 127.0.0.1 -p 25520 -s akka://systemProject_name@127.0.0.1:25520 -l 8080 -vp placeB 
    ```