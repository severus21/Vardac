from src.counters import *

VARDA_EXPORT = [("Varda", "varch"), ("Varda impl", "vimpl"), ("Java", "legacy")] 
AKKA_EXPORT = [("Java", "java")]

BENCHMARKS_DIR = Path(__file__).parent.parent
COUNTS = [
    # MPP
    LoCCounter("mpp-varda", BENCHMARKS_DIR/"bench-mpp/varda", VARDA_EXPORT),
    LoCCounter("mpp-varda-inline", BENCHMARKS_DIR/"bench-mpp/varda-inline", VARDA_EXPORT),
    LoCCounter("mpp-akka", BENCHMARKS_DIR/"bench-mpp/akka", AKKA_EXPORT),
    # MS 
    LoCCounter("ms-varda", BENCHMARKS_DIR/"bench-ms/varda", VARDA_EXPORT),
    LoCCounter("ms-akka", BENCHMARKS_DIR/"bench-ms/akka", AKKA_EXPORT),
    # KVS
    LoCCounter("kvs-varda", BENCHMARKS_DIR/"bench-kvs/varda", VARDA_EXPORT),
]
