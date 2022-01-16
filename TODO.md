## Known troubleshootings

## Setup/Working environment
* Setup CI
* Setup logging functionalities
* Unify error handling

## Code structure
* Load plugin from target files


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
| [first-class time value](#time-value)    | :question: |
| signature construct for component schema | :x: |
| [interface with existing code](#interface-existing-code) | :x: |
| high order session                        | :x: | discuss with Gio
| recursive session type                    | :scroll: |
| --- | --- | --- | 
| **programmin model**                       |
| re-think the place of bridge              | :x:      | broadcast bridge, tls bridge, ...
| bridge with multiple activation at the right/left | :x: |
| introduce dynamic port                    | :x:
| monitoring activation like in Actor model | :question: |
| communication are forbidden in contract/ghost/pattern of match | :x: 
| define pattern matching: pattern + encoding in Java | :x:| 
| separate port definition from port biding | :question: |
| **type system** |
| remove vplace info from place type or use subtyping based on type hierarchy ? | :question:
| progress and preservation proof
| add subtyping for bridge
| rewrite type inference(reconstruction) with constraints (see 3.2.2)
| **compiler**
| specify what pass to dump | :x:
| get ride of recv second arg since it is just used to infer ??? | :question: |
| rename *constraint* to *guard*    | :question:
| timer operation only support *<* *>* not *=* (no physical sens)
| add parsing priority to support x<5 && x>1 <=> (x<5) && (x>1) | :x:
| variable ``.*[0-9]$`` right trailing number are erased there fore ``a`` and ``a1`` became the same -> is it true ??? if so fix it | :question:
| rename shallow_scan in TypeInference.ml to collectsignature or smth meaning full | :x:
| rewrite 
| direct method call a::method() works only for RPC and inlined (is it working ????????????
| what if we have two kind of target for activation : process and thread / thread and actor / container and pods / thread + container + pod | :question:
| metadata for protocol guard | :x:
| **garbage collection**        |
| intermediate_state (i.e.) recv        | :anger:
| --- | --- | --- | 
| **static analysis**                       |
| detect un handle msg (i.e. missing port)  | :x: | like in P
| **distributed data** |
| expose (runtime) distributed data service   | :x:  | Akka ShareData, what is done by Benoit, an external service
| adding consistency construct: consistency layer + replicas + object + view + version | :x:
| detecting inconsistency from mixing multiple consistency layer       | :x: | see. MixT and CISE
| --- | --- | --- | 
| **builtin**   |
| set/map implem should be defined in *.impl file not forced during encoding    | :question:
| _0_, ..., _n_ should be rewritten to smth else to access tuple elmts | :x: | last ``_`` because variables ending with a [0-9] can be confused
| --- | --- | --- | 
| **verif: detect unwanted (unsafe) composition** |
| CISE, TLA+ (see. DCal), Why3, SMT
| concolict testing 
| liquid type
| **implicit elimination**
| implicit in type guard not supported yet (and not detetected as an error) | :x:
| **to doc/techreport**     |
| variable representation = each binder introduce an unique name |
| recv transformation for async (+garbage collection) | :x:
| place management and remote instanciation | :x:
| equality in *.spec is structural equality | :x:
| write the akka-encoding.md from sources   | :x:
| implicit elimination and why              | :x:  | N.B. create default onstartup if needed + guardian can not have implicit + explain scoping and implicit::
Warning: implicit::x -> type of x should be serializable not check by the compiler (tech debts)
| derivation definition/elimination     | :x: | diff with annotations + derivation are statically reduced + diff with metaprogrammation + deriviation are not location specific
| direct method call a::method() works only for RPC and inlined (is it working ????????????
| **Akka**  |
| use akka blocking primitive for sleep | :x: | to avoid blocking a thread of the pool
| is_instance only work for VarExpr arg | :x: | see Akka/misc.ml
| custom main: args and return are not yet use for guardian creation    | :x: |
| limitation: akka timer are not "précis" |
| **defensive programming**
| write tests for each pass and for each features 
| write pre/post condition for each pass
| write functional tests
| pass could be replaced by coq generated code | :x:
| **tooling**
| topology do not display subcomponent 


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

### Java plugin
* Handle indentation when generating the source code

### GADT during parsing
https://arxiv.org/pdf/1807.06702.pdf

