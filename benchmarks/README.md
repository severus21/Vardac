# Benchmarking

## Run bench

1. write bench specification in config/benchmarks.py
2. run bench ```./benchmarks/main.py run [--bench-selector name_1:..;:name_n]```
3. data is stored in db.sqlite3

## Access DB

* First run:
    1. create the db ``python3 manage.py migrate``
    1. create an user ``python3 manage.py createsuperuser --username e``
* Web console
    ``python3 manage.py runserver``
* Shell console
    ``ipython3 -i shell.py``, N.B. models are already loaded by shell.py

## Reset DB

``python3 manage.py flush``

## Render

1. write bench specification in config/figures.py
2. run bench ```./benchmarks/main.py render [--fig-selector name_1:..;:name_n]```

## Refs

* https://shipilev.net/talks/j1-Oct2011-21682-benchmarking.pdf
* baeldung.com/java-microbenchmark-harness

## Writing bench in Akka

### Exporting results

```java
    import java.util.*;
    import java.io.FileWriter;
    import java.io.IOException;

    import com.google.gson.Gson;

    Gson gson = new Gson();


    HashMap<String,  Object> res = new HashMap();
    res.put("ping_size",  this.ping_size);
    res.put("pong_size",  this.pong_size);
    res.put("rtts",  this.rtts);


    try {
        FileWriter fileWriter =new FileWriter("results.json"); 
        gson.toJson(res, fileWriter);
        fileWriter.close();
    } catch (IOException e ){
        System.err.println(e.toString());
    }
```