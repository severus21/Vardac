from zlib import DEFLATED
from .utils import *

from src.bench import *
from src.collectors import *
from src.builders import *
from src.runners import *
from src.factories import *
from src.citerators import *
from src.cgenerators import *

DEFAULT_N_MIN = 1 #log
DEFAULT_N_MAX = 2 #log
DEFAULT_WARMUP_MIN = 10
DEFAULT_WARMUP_MAX = DEFAULT_WARMUP_MIN + 1
DEFAULT_PAYLOAD_MIN = 0
DEFAULT_PAYLOAD_MAX = 1000 
DEFAULT_PAYLOAD_STEP = 100
DEFAULT_RUNS = 3

DEFAULT_JVM_OPTIONS = ' '.join([
    '-javaagent:build/libs/InstrumentationAgent.jar',
    '-enableassertions'
])

BENCHMARKS_DIR = Path(__file__).parent.parent

BENCHMARKS = [
    # Mono jvm
    Benchmark(
        "mpp-varda-one-jvm",
        VardaBuilder(
            BENCHMARKS_DIR/"bench-mpp"/"varda", 
            "dune exec --profile release -- vardac compile --places {{build_dir}}/places.yml --targets {{build_dir}}/targets.yml --impl benchmarks/libbench.vimpl  --filename {{build_dir}}/bench.varch --impl {{build_dir}}/bench.vimpl --provenance 0 && cd compiler-build/akka && make", 
            Path(os.getcwd()).absolute()),
        ShellRunnerFactory(
            f"java {DEFAULT_JVM_OPTIONS} -jar build/libs/main.jar -ip 127.0.0.1 -p 25520 -s akka://systemProject_name@127.0.0.1:25520 -l 8080 -vp placeB", 
            Path(os.getcwd())/"compiler-build"/"akka", 
            "Terminated ueyiqu8R"
        ),
        [ 
            StdoutCollector(get_elapse_time),
            FileCollector(Path(os.getcwd())/"compiler-build"/"akka"/"results.json", get_rtts),
            FileCollector(Path(os.getcwd())/"compiler-build"/"akka"/"results.json", get_pp_size),
        ],
        Generator(RangeIterator({
            "n": logrange(DEFAULT_N_MIN, DEFAULT_N_MAX, base=10),
            "warmup": range(DEFAULT_WARMUP_MIN, DEFAULT_WARMUP_MAX).__iter__()
        }), DEFAULT_RUNS)
    ),
    # inlined Pong in Wrapper
    Benchmark(
        "mpp-varda-inline-one-jvm",
        VardaBuilder(
            BENCHMARKS_DIR/"bench-mpp"/"varda", 
            "dune exec --profile release -- vardac compile --places {{build_dir}}/places.yml --targets {{build_dir}}/targets.yml --filename {{build_dir}}/bench.varch --impl benchmarks/libbench.vimpl --impl {{build_dir}}/bench.vimpl --provenance 0 && cd compiler-build/akka && make", 
            Path(os.getcwd()).absolute()
        ),
        ShellRunnerFactory(
            f"java {DEFAULT_JVM_OPTIONS} -jar build/libs/main.jar -ip 127.0.0.1 -p 25520 -s akka://systemProject_name@127.0.0.1:25520 -l 8080 -vp placeB", 
            Path(os.getcwd())/"compiler-build"/"akka", 
            "Terminated ueyiqu8R"
        ),
        [ 
            StdoutCollector(get_elapse_time),
            FileCollector(Path(os.getcwd())/"compiler-build"/"akka"/"results.json", get_rtts),
            FileCollector(Path(os.getcwd())/"compiler-build"/"akka"/"results.json", get_pp_size),
        ],
        Generator(RangeIterator({
            "n": logrange(DEFAULT_N_MIN, DEFAULT_N_MAX, base=10),
            "warmup": range(DEFAULT_WARMUP_MIN, DEFAULT_WARMUP_MAX).__iter__()
        }), DEFAULT_RUNS)
    ),
    Benchmark(
        "mpp-akka-one-jvm",
        AkkaBuilder(
            BENCHMARKS_DIR/"bench-mpp"/"akka", 
            "cd {{build_dir}}/ && make", 
            Path(os.getcwd()).absolute()
        ),
        ShellRunnerFactory(
            f"java {DEFAULT_JVM_OPTIONS} -jar build/libs/main.jar", 
            BENCHMARKS_DIR/"bench-mpp"/"akka", 
            "Terminated ueyiqu8R" 
        ),
        [ 
            StdoutCollector(get_elapse_time),
            FileCollector(BENCHMARKS_DIR/"bench-mpp"/"akka"/"results.json", get_rtts),
            FileCollector(BENCHMARKS_DIR/"bench-mpp"/"akka"/"results.json", get_pp_size),
        ],
        Generator(RangeIterator({
            "n": logrange(DEFAULT_N_MIN, DEFAULT_N_MAX, base=10),
            "warmup": range(DEFAULT_WARMUP_MIN, DEFAULT_WARMUP_MAX).__iter__(),
            "payload": range(DEFAULT_PAYLOAD_MIN, DEFAULT_PAYLOAD_MAX, DEFAULT_PAYLOAD_STEP).__iter__()
        }), DEFAULT_RUNS)
    ),
    # Multi jvm
    Benchmark(
        "mpp-akka-multi-jvms",
        AkkaBuilder(
            BENCHMARKS_DIR/"bench-mpp"/"akka", 
            "cd {{project_dir}} && make", 
            Path(os.getcwd()).absolute()
        ),
        MultiShellRunnerFactory(
            [
                ShellRunnerFactory(
                    f"java {DEFAULT_JVM_OPTIONS} -jar build/libs/pongService.jar -ip 127.0.0.1 -p 25520 -s akka://systemAkkaBench@127.0.0.1:25520", 
                    BENCHMARKS_DIR/"bench-mpp"/"akka", 
                    None,
                    config_adaptor=lambda config: remove_dict(config, ["n", "warmup"]) 
                ),
                ShellRunnerFactory(
                    f"java {DEFAULT_JVM_OPTIONS} -jar build/libs/pingService.jar -ip 127.0.0.1 -p 25521 -s akka://systemAkkaBench@127.0.0.1:25520", 
                    BENCHMARKS_DIR/"bench-mpp"/"akka", 
                    "Terminated ueyiqu8R",
                    set_stop_event=True
                )]),
        [ 
            StdoutCollector(get_elapse_time),
            FileCollector(BENCHMARKS_DIR/"bench-mpp"/"akka"/"results.json", get_rtts),
            FileCollector(BENCHMARKS_DIR/"bench-mpp"/"akka"/"results.json", get_pp_size),
        ],
        Generator(RangeIterator({
            "n": logrange(DEFAULT_N_MIN, DEFAULT_N_MAX, base=10),
            "warmup": range(DEFAULT_WARMUP_MIN, DEFAULT_WARMUP_MAX).__iter__()
            }), DEFAULT_RUNS)
    ),
    ### MS Bench
    Benchmark(
        "ms-varda-one-jvm",
        VardaBuilder(
            BENCHMARKS_DIR/"bench-mpp"/"varda", 
            "dune exec --profile release -- vardac compile --places {{project_dir}}/places.yml --targets {{project_dir}}/targets.yml --filename {{project_dir}}/bench.varch --impl benchmarks/libbench.vimpl --impl {{project_dir}}/bench.vimpl --provenance 0 && cd compiler-build/akka && make", 
            Path(os.getcwd()).absolute()),
        ShellRunnerFactory(
            f"java {DEFAULT_JVM_OPTIONS} -jar build/libs/main.jar -ip 127.0.0.1 -p 25520 -s akka://systemProject_name@127.0.0.1:25520 -l 8080 -vp placeB", 
            Path(os.getcwd())/"compiler-build"/"akka", 
            "Terminated ueyiqu8R"
        ),
        [ 
            StdoutCollector(get_elapse_time),
            FileCollector(Path(os.getcwd())/"compiler-build"/"akka"/"results.json", get_rtts),
        ],
        Generator(RangeIterator({
            "n": 1,
            "warmup": 0,
            "vs": range(3,6).__iter__(),
        }), DEFAULT_RUNS)
    ),
    Benchmark(
        "ms-akka-one-jvm",
        AkkaBuilder(
            BENCHMARKS_DIR/"bench-ms"/"akka", 
            "cd {{project_dir}} && make", 
            Path(os.getcwd()).absolute()),
        ShellRunnerFactory(
            f"java {DEFAULT_JVM_OPTIONS} -jar build/libs/main.jar", 
            BENCHMARKS_DIR/"bench-ms"/"akka", 
            "Terminated ueyiqu8R" 
        ),
        [ 
            StdoutCollector(get_elapse_time),
            FileCollector(BENCHMARKS_DIR/"bench-ms"/"akka"/"results.json", get_rtts),
        ],
        Generator(RangeIterator({
            "n": 1,
            "warmup": 0,
            "vs": range(3,6).__iter__(),
            }), DEFAULT_RUNS)
    ),
    Benchmark(
        "ms-akka-one-jvm-docker",
        DockerBuilder(
            BENCHMARKS_DIR/'bench-ms'/'akka'
        ),
        DockerRunnerFactory(
            f"/usr/local/openjdk-11/bin/java {DEFAULT_JVM_OPTIONS} -jar main.jar", 
            "Terminated ueyiqu8R" 
        ),
        [ 
            StdoutCollector(get_elapse_time),
            VolumeCollector([FileCollector("results.json", get_rtts)]),
        ],
        Generator(RangeIterator({
            "n": 1,
            "warmup": 0,
            "vs": range(3,4).__iter__(),
            }), DEFAULT_RUNS)
    ),
    Benchmark(
        "ms-varda-one-jvm-docker",
        ChainBuilder(
            [
                VardaBuilder(
                    BENCHMARKS_DIR/"bench-mpp"/"varda", 
                    "dune exec --profile release -- vardac compile --places {{project_dir}}/places.yml --targets {{project_dir}}/targets.yml --filename {{project_dir}}/bench.varch --impl benchmarks/libbench.vimpl --impl {{project_dir}}/bench.vimpl --provenance 0 && cd compiler-build/akka && make", 
                    Path(os.getcwd()).absolute()),
                DockerBuilder(
                    Path(os.getcwd())/"compiler-build"/"akka"),
            ],
            stamp_strategy = lambda builders: builders[0]._get_bench_model(), #since Varda striclty stronger than docker
            exposed_builder = lambda builders: builders[-1]# the one pass to runner
        ),
        DockerRunnerFactory(
            f"/usr/local/openjdk-11/bin/java {DEFAULT_JVM_OPTIONS} -jar main.jar -ip 127.0.0.1 -p 25520 -s akka://systemProject_name@127.0.0.1:25520 -l 8080 -vp placeB", 
            "Terminated ueyiqu8R"
        ),
        [ 
            StdoutCollector(get_elapse_time),
            VolumeCollector([FileCollector("results.json", get_rtts)]),
        ],
        Generator(RangeIterator({
            "n": 1,
            "warmup": 0,
            "vs": range(3,6).__iter__(),
        }), DEFAULT_RUNS)
    ),
]