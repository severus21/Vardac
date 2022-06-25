Scenario
    * 


1. Generate code
    ```bash
    1> make run -- compile --places examples/test-plgannot/places.yml --targets examples/test-plgannot/targets.yml --filename examples/test-plgannot/test.varch --impl examples/test-plgannot/test.vimpl --provenance 0
    ```
1. Check that generated code is well-formed
    ```1> ./examples/test-plgannot/is_well_formed.py ```
2. ```1> cd compiler-build/akka```
3. Build Varda targets
    ```bash
    1> make
    ```
4. Run example
    1. Run the Varda system (composed of one binary)
    ```bash
    1> java -enableassertions -jar build/libs/main.jar -ip 127.0.0.1 -p 25520 -s akka://systemProject_name@127.0.0.1:25520 -l 8080 -vp placeB 
    ```