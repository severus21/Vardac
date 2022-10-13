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
    LoCCounter("kvs-varda", BENCHMARKS_DIR/"bench-kvs/varda", VARDA_EXPORT),
    VardaCallbackCounter("kvs-varda", BENCHMARKS_DIR/"bench-kvs/varda"),
]
