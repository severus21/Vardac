## Known troubleshootings

## Setup/Working environment
* Setup CI
    * parallel:mateix variables are not injested -> therefore using COMPILER_VERSION hardcoded in variables
    * replace make bin by when warning/error done 
* Test coverage + fuzzing coverage + gadt

## Features

Semantics

* :question:          - does it make sens ?
* :x:                 - not yet addressed but we should do it
* :scroll:            - wiki page/ white report is written, implementation to be done
* :heavy_check_mark:  - implem done
* :anger:             - some code in compiler -> not finished          
* :collision:         - do not mix well with exsting PL or implementation


** Extract TODOC and TODO from sources**

| Name                      | Status    | Plan   |
| ---                       | ---       | ---           |
| [first-class time value](#time-value)                 | :question: |
| signature construct for component schema              | :x: |
| high order session                                    | :x: | discuss with Gio
| recursive session type                                | :scroll: |
| --- | --- | --- | 
| **programmin model**                                  |
| re-think the place of bridge                          | :x:      | broadcast bridge, tls bridge, ...
| introduce dynamic port                                | :x:
| monitoring activation like in Actor model             | :question: |
| communication are forbidden in contract/ghost         | :x: 
| define pattern matching: pattern + encoding in Java   | :x:| 
| **type system** |
| progress and preservation proof
| add subtyping for bridge
| rewrite type inference(reconstruction) with constraints (see 3.2.2)
| **compiler**
| rename *constraint* to *guard*                                    | :question:
| timer operation only support *<* *>* not *=* (no physical sens)   | :x:
| add parsing priority to support x<5 && x>1 <=> (x<5) && (x>1)     | :x:
| direct method call a::method() works only for RPC and inlined (is it working ????????????
| what if we have two kind of target for activation                 | :question:
| metadata for protocol guard                                       | :x:
| **garbage collection**        |
| intermediate_state (i.e.) recv        | :anger:
| --- | --- | --- | 
| **static analysis**                                               |
| detect un handle msg (i.e. missing port)                          | :x: | like in P
| **distributed data**                                              |
| expose (runtime) distributed data service                         | :x:  | Akka ShareData, what is done by Benoit, an external service
| adding consistency construct: consistency layer + replicas + object + view + version | :x:
| detecting inconsistency from mixing multiple consistency layer       | :x: | see. MixT and CISE
| --- | --- | --- | 
| **builtin**   |
| set/map implem should be defined in *.vimpl file not forced during encoding    | :question:
| _0_, ..., _n_ should be rewritten to smth else to access tuple elmts | :x: | last ``_`` because variables ending with a [0-9] can be confused
| --- | --- | --- | 
| **verif: detect unwanted (unsafe) composition** |
| CISE, TLA+ (see. DCal), Why3, SMT
| concolict testing 
| liquid type
| observables testing of Cezara PhD student
| **implicit elimination**
| implicit in type guard not supported yet (and not detetected as an error) | :x:
| **to doc/techreport**     |
| variable representation = each binder introduce an unique name |
| recv transformation for async (+garbage collection) | :x:
| place management and remote instanciation | :x:
| equality in *.varch is structural equality | :x:
| write the akka-encoding.md from sources   | :x:
| implicit elimination and why              | :x:  | 
| derivation definition/elimination     | :x: | diff with annotations + derivation are statically reduced + diff with metaprogrammation + deriviation are not location specific
| direct method call a::method() works only for RPC and inlined (is it working ????????????
| **Akka**  |
| use akka blocking primitive for sleep | :x: | to avoid blocking a thread of the pool
| is_instance only work for VarExpr arg | :x: | see Akka/misc.ml
| limitation: akka timer is not accurate |
| **defensive programming**
| write tests for each pass and for each features 
| write pre/post condition for each pass
| write functional tests
| write fuzzing tests       | :x:
| **tooling**


technical debts
* a lot of compilation pass are notrecursive (i.e. will not be applied on the generated component/... even if it should be reduced one more)

##### <a name="time-value"></a> first-class time value
```
    time t1 = time();
```

##### <a name="garbage-collection-intermediate-state"></a> 
Done
- ontimeout/frozen -> reply with AkkaDeadSession
- when it works

TODO
- on failure (never receive AkkaDeadSession)

##### <a name="interface-existing-code"></a> Integrating existing code

Trough: 
* glue in charge
    1. API call (REST, protobuf, ...) 
        * deploy the service is orthogonal to our work
    2. lib call (glue called external lib) 
        * needs to deploy the app with the glue
* external app call the glue (like ServiceWorker)
   * **TODO** what is the cost to it ??? what are the advantage of this ??
    1. API call 
    2. lib call




## Src 

### Akka plugin

#### Interfaces

##### gRPC

* custom port for server and client, currently fixed at 8090

### Java plugin
* Handle indentation when generating the source code

### GADT during parsing
https://arxiv.org/pdf/1807.06702.pdf

