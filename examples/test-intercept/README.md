Scenario
    * Case A: Pingpong with an interceptor couting msg 

1. Generate code
    ```bash
    1> make run -- compile --places examples/test-intercept/places.yml --targets examples/test-intercept/targets.yml --filename examples/test-intercept/test.varch --impl examples/test-intercept/test.vimpl --provenance 0
    ```
2. ```1> cd compiler-build/akka```
3. Build Varda targets
    ```bash
    1> make
    ```
4. Run example
    1. Run the Varda system (composed of one binary per case)
    * Case A:
    ```bash
    1> java -enableassertions -jar build/libs/passivePlayer.jar -ip 127.0.0.1 -p 25520 -s akka://systemProject_name@127.0.0.1:25520 -l 8080 -vp placeA 
    2> java -enableassertions -jar build/libs/multiPlayer.jar -ip 127.0.0.1 -p 25521 -s akka://systemProject_name@127.0.0.1:25520 -l 8080 -vp Cloud 
    ```
    ```