Scenario
    * https://doc.akka.io/docs/akka/2.6.19/typed/fault-tolerance.html
    * expose PreRestart and PostStop events
        received from parent                
        Before a supervised actor is restarted it is sent (by its supervisor) the PreRestart signal giving it a chance to clean up resources it has created, much like the PostStop signal when the actor stops.
s

. ChildFailed
    * monitors
      * Terminated + ChildFailed + watch /unwatch


1. Generate code
    ```bash
    1> make run -- compile --places examples/test-eport/places.yml --targets examples/test-eport/targets.yml --filename examples/test-eport/test.varch --impl examples/test-eport/test.vimpl --provenance 0
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