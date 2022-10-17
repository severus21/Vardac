from src.counters import *

VARDA_EXPORT = [("Varda", "varch"), ("Varda impl", "vimpl"), ("Java", "legacy"), ("YAML", "config")] 
AKKA_EXPORT = [("Java", "java"), ("Config","config"), ("Gradle", "build"), ("Makefile", "build")]

BENCHMARKS_DIR = Path(__file__).parent.parent
COUNTS = [
    # MPP
    LoCCounter("mpp-varda", BENCHMARKS_DIR/"bench-mpp/varda", VARDA_EXPORT),
    VardaCallbackCounter("mpp-varda", BENCHMARKS_DIR/"bench-mpp/varda"),
    LoCCounter("mpp-varda-inline", BENCHMARKS_DIR/"bench-mpp/varda-inline", VARDA_EXPORT),
    VardaCallbackCounter("mpp-varda-inline", BENCHMARKS_DIR/"bench-mpp/varda-inline"),
    LoCCounter("mpp-akka", BENCHMARKS_DIR/"bench-mpp/akka", AKKA_EXPORT),
    AkkaCallbackCounter("mpp-akka", BENCHMARKS_DIR/"bench-mpp/akka" ),

    # MS 
    LoCCounter("ms-varda", BENCHMARKS_DIR/"bench-ms/varda", VARDA_EXPORT),
    VardaCallbackCounter("ms-varda", BENCHMARKS_DIR/"bench-ms/varda"),
    LoCCounter("ms-akka", BENCHMARKS_DIR/"bench-ms/akka", AKKA_EXPORT),
    AkkaCallbackCounter("ms-akka", BENCHMARKS_DIR/"bench-ms/akka"),

    # KVS
    LoCCounter("kvs-varda", BENCHMARKS_DIR/"bench-kvs/varda", VARDA_EXPORT, excludes=["kv-redis.vimpl"]),
    VardaCallbackCounter("kvs-varda", BENCHMARKS_DIR/"bench-kvs/varda"),
    LoCCounter("kvs-akka", BENCHMARKS_DIR/"bench-kvs/akka", AKKA_EXPORT, excludes=[
        "src/main/java/com/varda/ConsoleClient.java",
        "src/main/java/com/varda/KVLStore.java",
        "src/main/java/com/varda/LoadbalancerActor.java",
        "src/main/java/com/varda/YCSB.java"
        ]),
    AkkaCallbackCounter("kvs-akka", BENCHMARKS_DIR/"bench-kvs/akka"),

    # KVS with loadbalancer
    LoCCounter("lkvs-varda", BENCHMARKS_DIR/"bench-kvs/varda-full", VARDA_EXPORT, excludes=["kv-redis.vimpl"]),
    VardaCallbackCounter("lkvs-varda", BENCHMARKS_DIR/"bench-kvs/varda-full"),
    LoCCounter("lkvs-akka", BENCHMARKS_DIR/"bench-kvs/akka", AKKA_EXPORT,excludes=[
        "src/main/java/com/varda/ConsoleClient.java",
        "src/main/java/com/varda/KVStore.java",
        "src/main/java/com/varda/YCSB.java"
        ]),
    AkkaCallbackCounter("lkvs-akka", BENCHMARKS_DIR/"bench-kvs/akka"),
]
