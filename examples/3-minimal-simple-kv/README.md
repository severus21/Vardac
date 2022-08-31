* Scenario 1 : without redis
    1. Generate code
        ```bash
        1> make run -- compile --places examples/3-minimal-simple-kv/places.yml --targets examples/3-minimal-simple-kv/targets.yml --filename examples/3-minimal-simple-kv/kv.varch --impl examples/3-minimal-simple-kv/kv-wo-redis.vimpl --provenance 0
        ```
    1. ```1> cd compiler-build/akka```
    1. Build Varda targets
        ```bash
        1> make
        ```
    1. Run example
        1. Run the Varda system (composed of one binary)
        ```bash
        1> java -enableassertions -jar build/libs/local.jar -ip 127.0.0.1 -p 25520 -s akka://systemProject_name@127.0.0.1:25520 -l 8080 -vp placeA 
        ```
2. Scenario 2 : with redis
    1. Generate code
        ```bash
        1> make run -- compile --places examples/3-minimal-simple-kv/places.yml --targets examples/3-minimal-simple-kv/targets.yml --filename examples/3-minimal-simple-kv/kv.varch --impl examples/3-minimal-simple-kv/kv-redis.vimpl --provenance 0
        ```
    1. ```1> cd compiler-build/akka```
    1. 
        ```bash 
        1> docker-compose up -d --build 
        1> docker container logs $(docker ps -q --filter="ancestor=project_name-server")
        1> docker-compose down 
        1> docker-compose rm 
        ```
    * To query the redis kv directly
        docker exec -it container_id redis-cli
