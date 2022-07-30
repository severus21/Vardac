Scenario
    * 


1. Generate code
    ```bash
    1> make run -- compile --places examples/test-simpl-inline/places.yml --targets examples/test-simpl-inline/targets.yml --filename examples/test-simpl-inline/test.varch --impl examples/test-simpl-inline/test.vimpl --provenance 0
    ```
2. ```1> cd compiler-build/akka```
3. Build Varda targets
    ```bash
    1> make
    ```
4. Run example
    1. Run the Varda system (composed of one binary per case)
    * Case A: Inline in a dummy wrapper 
    ```bash
    1> java -enableassertions -jar build/libs/mainA.jar -ip 127.0.0.1 -p 25520 -s akka://systemProject_name@127.0.0.1:25520 -l 8080 -vp placeB 
    ```
    * Case B: Inline, then exchange message with host (host and inlined do not have the same type - discovery using bridges)
    ```bash
    1> java -enableassertions -jar build/libs/mainB.jar -ip 127.0.0.1 -p 25520 -s akka://systemProject_name@127.0.0.1:25520 -l 8080 -vp placeB 
    ```
    * Case C: Inline, then exchange message with host (host and inlined has the same type)
    ```bash
    1> java -enableassertions -jar build/libs/mainC.jar -ip 127.0.0.1 -p 25520 -s akka://systemProject_name@127.0.0.1:25520 -l 8080 -vp placeB 
    ```
    * Dase D: Inline, then exchange message with host (host and inlined has the same type)
    ```bash
    1> java -enableassertions -jar build/libs/mainD.jar -ip 127.0.0.1 -p 25520 -s akka://systemProject_name@127.0.0.1:25520 -l 8080 -vp placeB 
    ```