from src.counters import *

VARDA_EXPORT = [("Varda", "varch"), ("Varda impl", "vimpl"), ("Java", "legacy")] 
AKKA_EXPORT = [("Java", "java")]

COUNTS = [
    # MPP
    LoCCounter("mpp-varda", "bench-mpp/varda", VARDA_EXPORT),
    LoCCounter("mpp-varda-inline", "bench-mpp/varda-inline", VARDA_EXPORT),
    LoCCounter("mpp-akka", "bench-mpp/akka", AKKA_EXPORT),
    # MS 
    LoCCounter("ms-varda", "bench-ms/varda", VARDA_EXPORT),
    LoCCounter("ms-akka", "bench-ms/akka", AKKA_EXPORT),
]