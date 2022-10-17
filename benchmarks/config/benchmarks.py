from zlib import DEFLATED
from .utils import *

from src.bench import *
from src.collectors import *
from src.builders import *
from src.runners import *
from src.factories import *
from src.citerators import *
from src.cgenerators import *

DEFAULT_LOG_BASE = 2
DEFAULT_N_MIN = 10  # log
DEFAULT_N_MAX = 20  # log

DEFAULT_WARMUP_MIN = 10
DEFAULT_WARMUP_MAX = DEFAULT_WARMUP_MIN + 1

DEFAULT_PAYLOAD_MIN = 0
DEFAULT_PAYLOAD_MAX = 1000
DEFAULT_PAYLOAD_STEP = 300
DEFAULT_RUNS = 3


# |array to sort| = 2^VS
DEFAULT_VS_MIN = 3
DEFAULT_VS_MAX = 12

DEFAULT_JVM_OPTIONS = ' '.join([
    '-javaagent:build/libs/InstrumentationAgent.jar',
    '-enableassertions'
])

DEFAULT_DOCKER_JVM_OPTIONS = ' '.join([
    '-javaagent:InstrumentationAgent.jar',
    '-enableassertions'
])

BENCHMARKS_DIR = Path(__file__).parent.parent

BENCHMARKS = [
    # Mono jvm
    Benchmark(
        "mpp-varda-one-jvm",
        VardaBuilder(
            BENCHMARKS_DIR/"bench-mpp"/"varda",
            "dune exec --profile release -- vardac compile --places {{project_dir}}/places.yml --targets {{project_dir}}/targets.yml --impl benchmarks/libbench.vimpl  --filename {{project_dir}}/bench.varch --impl {{project_dir}}/bench.vimpl --provenance 0 && cd compiler-build/akka && make",
            Path(os.getcwd()).absolute()),
        ShellRunnerFactory(
            f"java {DEFAULT_JVM_OPTIONS} -jar build/libs/main.jar -ip 127.0.0.1 -p 25520 -s akka://systemProject_name@127.0.0.1:25520 -l 8080 -vp placeB {{{{rconfig}}}}",
            Path(os.getcwd())/"compiler-build"/"akka",
            "Terminated ueyiqu8R"
        ),
        [
            StdoutCollector(get_elapse_time),
            FileCollector(Path(os.getcwd())/"compiler-build" / \
                          "akka"/"results.json", get_rtts),
            FileCollector(Path(os.getcwd())/"compiler-build" / \
                          "akka"/"results.json", get_pp_size),
        ],
        Generator(RangeIterator({
            "n": logrange(DEFAULT_N_MIN, DEFAULT_N_MAX, base=DEFAULT_LOG_BASE),
            "warmup": range(DEFAULT_WARMUP_MIN, DEFAULT_WARMUP_MAX).__iter__(),
            "payload": 0,#range(DEFAULT_PAYLOAD_MIN, DEFAULT_PAYLOAD_MAX, DEFAULT_PAYLOAD_STEP).__iter__()
        }), DEFAULT_RUNS)
    ),
    #Benchmark(
    #    "mpp-varda-one-jvm-tracing",
    #    VardaBuilder(
    #        BENCHMARKS_DIR/"bench-mpp"/"varda",
    #        "dune exec --profile release -- vardac compile --places {{project_dir}}/places.yml --targets {{project_dir}}/targets.yml --impl benchmarks/libbench.vimpl  --filename {{project_dir}}/bench.varch --impl {{project_dir}}/bench.vimpl --provenance 0 --enable-tracing && cd compiler-build/akka && make",
    #        Path(os.getcwd()).absolute()),
    #    ShellRunnerFactory(
    #        f"java {DEFAULT_JVM_OPTIONS} -jar build/libs/main.jar -ip 127.0.0.1 -p 25520 -s akka://systemProject_name@127.0.0.1:25520 -l 8080 -vp placeB {{{{rconfig}}}}",
    #        Path(os.getcwd())/"compiler-build"/"akka",
    #        "Terminated ueyiqu8R"
    #    ),
    #    [
    #        StdoutCollector(get_elapse_time),
    #        FileCollector(Path(os.getcwd())/"compiler-build" / \
    #                      "akka"/"results.json", get_rtts),
    #        FileCollector(Path(os.getcwd())/"compiler-build" / \
    #                      "akka"/"results.json", get_traces("ping")),
    #        FileCollector(Path(os.getcwd())/"compiler-build" / \
    #                      "akka"/"results-pong.json", get_traces("pong")),
    #        FileCollector(Path(os.getcwd())/"compiler-build" / \
    #                      "akka"/"results.json", get_pp_size),
    #    ],
    #    Generator(RangeIterator({
    #        "n": logrange(2, 3, base=DEFAULT_LOG_BASE),
    #        "warmup": range(0, 1).__iter__()
    #    }), 1)
    #),
    Benchmark(
        "mpp-varda-contract-one-jvm",
        VardaBuilder(
            BENCHMARKS_DIR/"bench-mpp"/"varda-contract",
            "dune exec --profile release -- vardac compile --places {{project_dir}}/places.yml --targets {{project_dir}}/targets.yml --impl benchmarks/libbench.vimpl  --filename {{project_dir}}/bench.varch --impl {{project_dir}}/bench.vimpl --provenance 0 && cd compiler-build/akka && make",
            Path(os.getcwd()).absolute()),
        ShellRunnerFactory(
            f"java {DEFAULT_JVM_OPTIONS} -jar build/libs/main.jar -ip 127.0.0.1 -p 25520 -s akka://systemProject_name@127.0.0.1:25520 -l 8080 -vp placeB {{{{rconfig}}}}",
            Path(os.getcwd())/"compiler-build"/"akka",
            "Terminated ueyiqu8R"
        ),
        [
            StdoutCollector(get_elapse_time),
            FileCollector(Path(os.getcwd())/"compiler-build" / \
                          "akka"/"results.json", get_rtts),
            FileCollector(Path(os.getcwd())/"compiler-build" / \
                          "akka"/"results.json", get_pp_size),
        ],
        Generator(RangeIterator({
            "n": logrange(DEFAULT_N_MIN, DEFAULT_N_MAX, base=DEFAULT_LOG_BASE),
            "warmup": range(DEFAULT_WARMUP_MIN, DEFAULT_WARMUP_MAX).__iter__()
        }), DEFAULT_RUNS)
    ),
    # inlined Pong in Wrapper
    Benchmark(
        "mpp-varda-inline-one-jvm",
        VardaBuilder(
            BENCHMARKS_DIR/"bench-mpp"/"varda",
            "dune exec --profile release -- vardac compile --places {{project_dir}}/places.yml --targets {{project_dir}}/targets.yml --filename {{project_dir}}/bench.varch --impl benchmarks/libbench.vimpl --impl {{project_dir}}/bench.vimpl --provenance 0 && cd compiler-build/akka && make",
            Path(os.getcwd()).absolute()
        ),
        ShellRunnerFactory(
            f"java {DEFAULT_JVM_OPTIONS} -jar build/libs/main.jar -ip 127.0.0.1 -p 25520 -s akka://systemProject_name@127.0.0.1:25520 -l 8080 -vp placeB {{{{rconfig}}}}",
            Path(os.getcwd())/"compiler-build"/"akka",
            "Terminated ueyiqu8R"
        ),
        [
            StdoutCollector(get_elapse_time),
            FileCollector(Path(os.getcwd())/"compiler-build" / \
                          "akka"/"results.json", get_rtts),
            FileCollector(Path(os.getcwd())/"compiler-build" / \
                          "akka"/"results.json", get_pp_size),
        ],
        Generator(RangeIterator({
            "n": logrange(DEFAULT_N_MIN, DEFAULT_N_MAX, base=DEFAULT_LOG_BASE),
            "warmup": range(DEFAULT_WARMUP_MIN, DEFAULT_WARMUP_MAX).__iter__()
        }), DEFAULT_RUNS)
    ),
    Benchmark(
        "mpp-akka-one-jvm",
        AkkaBuilder(
            BENCHMARKS_DIR/"bench-mpp"/"akka",
            "cd {{project_dir}}/ && make",
            Path(os.getcwd()).absolute()
        ),
        ShellRunnerFactory(
            f"java {DEFAULT_JVM_OPTIONS} -jar build/libs/main.jar {{{{rconfig}}}}",
            BENCHMARKS_DIR/"bench-mpp"/"akka",
            "Terminated ueyiqu8R"
        ),
        [
            StdoutCollector(get_elapse_time),
            FileCollector(BENCHMARKS_DIR/"bench-mpp" / \
                          "akka"/"results.json", get_rtts),
            FileCollector(BENCHMARKS_DIR/"bench-mpp" / \
                          "akka"/"results.json", get_pp_size),
        ],
        Generator(RangeIterator({
            "n": logrange(DEFAULT_N_MIN, DEFAULT_N_MAX, base=DEFAULT_LOG_BASE),
            "warmup": range(DEFAULT_WARMUP_MIN, DEFAULT_WARMUP_MAX).__iter__(),
            "payload": 0,#range(DEFAULT_PAYLOAD_MIN, DEFAULT_PAYLOAD_MAX, DEFAULT_PAYLOAD_STEP).__iter__()
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
                    f"java {DEFAULT_JVM_OPTIONS} -jar build/libs/pongService.jar -ip 127.0.0.1 -p 25520 -s akka://systemAkkaBench@127.0.0.1:25520 {{{{rconfig}}}}",
                    BENCHMARKS_DIR/"bench-mpp"/"akka",
                    None,
                    config_adaptor=lambda config: remove_dict(
                        config, ["n", "warmup", "payload"]),
                    name = "pong-service"
                ),
                ShellRunnerFactory(
                    f"java {DEFAULT_JVM_OPTIONS} -jar build/libs/pingService.jar -ip 127.0.0.1 -p 25521 -s akka://systemAkkaBench@127.0.0.1:25520 {{{{rconfig}}}}",
                    BENCHMARKS_DIR/"bench-mpp"/"akka",
                    "Terminated ueyiqu8R",
                    set_stop_event=True,
                    name = "ping-service"
                )]),
        [
            StdoutCollector(get_elapse_time),
            FileCollector(BENCHMARKS_DIR/"bench-mpp" / \
                          "akka"/"results.json", get_rtts),
            FileCollector(BENCHMARKS_DIR/"bench-mpp" / \
                          "akka"/"results.json", get_pp_size),
        ],
        Generator(RangeIterator({
            "n": logrange(DEFAULT_N_MIN, DEFAULT_N_MAX, base=DEFAULT_LOG_BASE),
            "warmup": range(DEFAULT_WARMUP_MIN, DEFAULT_WARMUP_MAX).__iter__(),
            "payload": 0,
            #"payload": range(DEFAULT_PAYLOAD_MIN, DEFAULT_PAYLOAD_MAX, DEFAULT_PAYLOAD_STEP).__iter__()
        }), DEFAULT_RUNS)
    ),
    Benchmark(
        "mpp-varda-multi-jvms",
        VardaBuilder(
            BENCHMARKS_DIR/"bench-mpp"/"varda",
            "dune exec --profile release -- vardac compile --places {{project_dir}}/places.yml --targets {{project_dir}}/targets.yml --filename {{project_dir}}/bench.varch --impl benchmarks/libbench.vimpl --impl {{project_dir}}/bench.vimpl --provenance 0 && cd compiler-build/akka && make",
            Path(os.getcwd()).absolute()
        ),
        MultiShellRunnerFactory(
            [
                ShellRunnerFactory(
                    f"java {DEFAULT_JVM_OPTIONS} -jar build/libs/passivePlayer.jar -ip 127.0.0.1 -p 25520 -s akka://systemProject_name@127.0.0.1:25520 -l 8080 -vp placeA {{{{rconfig}}}}",
                    Path(os.getcwd())/"compiler-build"/"akka",
                    None,
                    config_adaptor=lambda config: remove_dict(
                        config, ["n", "warmup", "payload"]),
                    name = "pong-service"
                ),
                ShellRunnerFactory(
                    f"sleep 5 && java {DEFAULT_JVM_OPTIONS} -jar build/libs/multiPlayer.jar -ip 127.0.0.1 -p 25521 -s akka://systemProject_name@127.0.0.1:25520 -l 8080 -vp placeB {{{{rconfig}}}}",
                    Path(os.getcwd())/"compiler-build"/"akka",
                    "Terminated ueyiqu8R",
                    set_stop_event=True,
                    name = "ping-service"
                ),
            ],
            name = "orchestrator"
            ),
        [
            StdoutCollector(get_elapse_time),
            FileCollector(Path(os.getcwd())/"compiler-build" / \
                          "akka"/"results.json", get_rtts),
            FileCollector(Path(os.getcwd())/"compiler-build" / \
                          "akka"/"results.json", get_pp_size),
        ],
        Generator(RangeIterator({
            "n": logrange(DEFAULT_N_MIN, DEFAULT_N_MAX, base=DEFAULT_LOG_BASE),
            "warmup": range(DEFAULT_WARMUP_MIN, DEFAULT_WARMUP_MAX).__iter__(),
            "payload": 0,
            #"payload": range(DEFAULT_PAYLOAD_MIN, DEFAULT_PAYLOAD_MAX, DEFAULT_PAYLOAD_STEP).__iter__()
        }), DEFAULT_RUNS)
    ),
    # MS Bench
    Benchmark(
        "ms-varda-one-jvm",
        VardaBuilder(
            BENCHMARKS_DIR/"bench-ms"/"varda",
            "dune exec --profile release -- vardac compile --places {{project_dir}}/places.yml --targets {{project_dir}}/targets.yml --filename {{project_dir}}/bench.varch --impl benchmarks/libbench.vimpl --impl {{project_dir}}/bench.vimpl --provenance 0 && cd compiler-build/akka && make",
            Path(os.getcwd()).absolute()),
        ShellRunnerFactory(
            f"java {DEFAULT_JVM_OPTIONS} -jar build/libs/main.jar -ip 127.0.0.1 -p 25520 -s akka://systemProject_name@127.0.0.1:25520 -l 8080 -vp placeB {{{{rconfig}}}}",
            Path(os.getcwd())/"compiler-build"/"akka",
            "Terminated ueyiqu8R"
        ),
        [
            StdoutCollector(get_elapse_time),
            FileCollector(Path(os.getcwd())/"compiler-build" / \
                          "akka"/"results.json", get_rtts),
        ],
        Generator(RangeIterator({
            "n": 1,
            "warmup": 0,
            "vs": range(DEFAULT_VS_MIN, DEFAULT_VS_MAX).__iter__(),
        }), DEFAULT_RUNS)
    ),
    Benchmark(
        "ms-varda-one-jvm-wo-refl",
        VardaBuilder(
            BENCHMARKS_DIR/"bench-ms"/"varda",
            "dune exec --profile release -- vardac compile --places {{project_dir}}/places.yml --targets {{project_dir}}/targets.yml --filename {{project_dir}}/bench.varch --impl benchmarks/libbench.vimpl --impl {{project_dir}}/bench.vimpl --provenance 0 --disable-global-placement-reflexivity --disable-global-bridge-reflexivity && cd compiler-build/akka && make",
            Path(os.getcwd()).absolute()),
        ShellRunnerFactory(
            f"java {DEFAULT_JVM_OPTIONS} -jar build/libs/main.jar -ip 127.0.0.1 -p 25520 -s akka://systemProject_name@127.0.0.1:25520 -l 8080 -vp placeB {{{{rconfig}}}}",
            Path(os.getcwd())/"compiler-build"/"akka",
            "Terminated ueyiqu8R"
        ),
        [
            StdoutCollector(get_elapse_time),
            FileCollector(Path(os.getcwd())/"compiler-build" / \
                          "akka"/"results.json", get_rtts),
        ],
        Generator(RangeIterator({
            "n": 1,
            "warmup": 0,
            "vs": range(DEFAULT_VS_MIN, DEFAULT_VS_MAX).__iter__(),
        }), DEFAULT_RUNS)
    ),
    Benchmark(
        "ms-akka-one-jvm",
        AkkaBuilder(
            BENCHMARKS_DIR/"bench-ms"/"akka",
            "cd {{project_dir}} && make",
            Path(os.getcwd()).absolute()),
        ShellRunnerFactory(
            f"java {DEFAULT_JVM_OPTIONS} -jar build/libs/main.jar {{{{rconfig}}}}",
            BENCHMARKS_DIR/"bench-ms"/"akka",
            "Terminated ueyiqu8R"
        ),
        [
            StdoutCollector(get_elapse_time),
            FileCollector(BENCHMARKS_DIR/"bench-ms" / \
                          "akka"/"results.json", get_rtts),
        ],
        Generator(RangeIterator({
            "n": 1,
            "warmup": 0,
            "vs": range(DEFAULT_VS_MIN, DEFAULT_VS_MAX).__iter__(),
        }), DEFAULT_RUNS)
    ),
    #Benchmark(
    #    "ms-akka-one-jvm-docker",
    #    DockerBuilder(
    #        BENCHMARKS_DIR/'bench-ms'/'akka'
    #    ),
    #    DockerRunnerFactory(
    #        f"/usr/local/openjdk-11/bin/java {DEFAULT_DOCKER_JVM_OPTIONS} -jar main.jar",
    #        "Terminated ueyiqu8R"
    #    ),
    #    [
    #        StdoutCollector(get_elapse_time),
    #        VolumeCollector([FileCollector("results.json", get_rtts)]),
    #    ],
    #    Generator(RangeIterator({
    #        "n": 1,
    #        "warmup": 0,
    #        "vs": range(DEFAULT_VS_MIN, DEFAULT_VS_MAX).__iter__(),
    #    }), DEFAULT_RUNS)
    #),
    #Benchmark(
    #    "ms-varda-one-jvm-docker",
    #    ChainBuilder(
    #        [
    #            VardaBuilder(
    #                BENCHMARKS_DIR/"bench-ms"/"varda",
    #                "dune exec --profile release -- vardac compile --places {{project_dir}}/places.yml --targets {{project_dir}}/targets.yml --filename {{project_dir}}/bench.varch --impl benchmarks/libbench.vimpl --impl {{project_dir}}/bench.vimpl --provenance 0 && cd compiler-build/akka && make",
    #                Path(os.getcwd()).absolute()),
    #            DockerBuilder(
    #                Path(os.getcwd())/"compiler-build"/"akka"),
    #        ],
    #        # since Varda striclty stronger than docker
    #        stamp_strategy=lambda builders: builders[0]._get_bench_model(),
    #        # the one pass to runner
    #        exposed_builder=lambda builders: builders[-1]
    #    ),
    #    DockerRunnerFactory(
    #        f"/usr/local/openjdk-11/bin/java {DEFAULT_DOCKER_JVM_OPTIONS} -jar main.jar -ip 127.0.0.1 -p 25520 -s akka://systemProject_name@127.0.0.1:25520 -l 8080 -vp placeB",
    #        "Terminated ueyiqu8R"
    #    ),
    #    [
    #        StdoutCollector(get_elapse_time),
    #        VolumeCollector([FileCollector("results.json", get_rtts)]),
    #    ],
    #    Generator(RangeIterator({
    #        "n": 1,
    #        "warmup": 0,
    #        "vs": range(DEFAULT_VS_MIN, DEFAULT_VS_MAX).__iter__(),
    #    }), DEFAULT_RUNS)
    #),
    # YCSB
    Benchmark(
        "ycsb-kvs-varda-redis",
        ChainBuilder(
            [
                VardaBuilder(
                    BENCHMARKS_DIR/"bench-kvs"/"varda",
                    "dune exec --profile release -- vardac compile --places {{project_dir}}/places.yml --targets {{project_dir}}/targets.yml --filename {{project_dir}}/kv.varch --impl benchmarks/libbench.vimpl --impl {{project_dir}}/kv-redis.vimpl --provenance 0 && cd compiler-build/akka && gradle -x test jarYCSBClient",
                    Path(os.getcwd()).absolute()),
                DockerComposeBuilder(
                    BENCHMARKS_DIR/"bench-kvs"/"varda",
                    Path(os.getcwd())/"compiler-build"/"akka"),
            ],
            # since Varda one is strictly stronger than docker
            stamp_strategy=lambda builders: builders[0]._get_bench_model(),
            # the one pass to runner
            exposed_builder=lambda builders: builders[-1]
        ),
        ShellRunnerFactory(
            f"java -classpath build/libs/YCSBClient.jar:YCSB/conf:{{{{env.YCSB}}}}/lib/HdrHistogram-2.1.4.jar:{{{{env.YCSB}}}}/lib/core-0.17.0.jar:{{{{env.YCSB}}}}/lib/htrace-core4-4.1.0-incubating.jar:{{{{env.YCSB}}}}/lib/jackson-core-asl-1.9.4.jar:{{{{env.YCSB}}}}/lib/jackson-mapper-asl-1.9.4.jar:{{{{env.YCSB}}}}/redis-binding/lib/commons-pool2-2.4.2.jar:{{{{env.YCSB}}}}/redis-binding/lib/jedis-2.9.0.jar:{{{{env.YCSB}}}}/redis-binding/lib/redis-binding-0.17.0.jar site.ycsb.Client -t -db author.project_name.YCSBClient -s -P {{{{env.YCSB}}}}/workloads/workloada > /tmp/ycsb-results",
            Path(os.getcwd())/"compiler-build"/"akka",
            "Terminated ueyiqu8R",
            environment = {
                'YCSB': BENCHMARKS_DIR/"ycsb-0.17.0"
            }
        ),
        [
            FileCollector("/tmp/ycsb-results", get_ycsb_result),
        ],
        Generator(RangeIterator({
            "threads": range(1, 2).__iter__(),
            "workload": ["workloada"].__iter__(),
        }), 3)
    ),
    Benchmark(
        "ycsb-kvs-varda-inmemory",
        ChainBuilder(
            [
                VardaBuilder(
                    BENCHMARKS_DIR/"bench-kvs"/"varda",
                    "dune exec --profile release -- vardac compile --places {{project_dir}}/places.yml --targets {{project_dir}}/targets.yml --filename {{project_dir}}/kv.varch --impl benchmarks/libbench.vimpl --impl {{project_dir}}/kv-wo-redis.vimpl --provenance 0 && cd compiler-build/akka && gradle -x test jarYCSBClient",
                    Path(os.getcwd()).absolute()),
                DockerComposeBuilder(
                    BENCHMARKS_DIR/"bench-kvs"/"varda",
                    Path(os.getcwd())/"compiler-build"/"akka"),
            ],
            # since Varda striclty stronger than docker
            stamp_strategy=lambda builders: builders[0]._get_bench_model(),
            # the one pass to runner
            exposed_builder=lambda builders: builders[-1]
        ),
        ShellRunnerFactory(
            f"java -classpath build/libs/YCSBClient.jar:{{{{env.YCSB}}}}/conf:{{{{env.YCSB}}}}/lib/HdrHistogram-2.1.4.jar:{{{{env.YCSB}}}}/lib/core-0.17.0.jar:{{{{env.YCSB}}}}/lib/htrace-core4-4.1.0-incubating.jar:{{{{env.YCSB}}}}/lib/jackson-core-asl-1.9.4.jar:{{{{env.YCSB}}}}/lib/jackson-mapper-asl-1.9.4.jar:{{{{env.YCSB}}}}/redis-binding/lib/commons-pool2-2.4.2.jar:{{{{env.YCSB}}}}/redis-binding/lib/jedis-2.9.0.jar:{{{{env.YCSB}}}}/redis-binding/lib/redis-binding-0.17.0.jar site.ycsb.Client -t -db author.project_name.YCSBClient -s -P {{{{env.YCSB}}}}/workloads/workloada > /tmp/ycsb-results",
            Path(os.getcwd())/"compiler-build"/"akka",
            "Terminated ueyiqu8R",
            environment = {
                'YCSB': BENCHMARKS_DIR/"ycsb-0.17.0"
            }
        ),
        [
            FileCollector("/tmp/ycsb-results", get_ycsb_result),
        ],
        Generator(RangeIterator({
            "threads": range(1, 2).__iter__(),
            "workload": ["workloada"].__iter__(),
        }), 3)
    ),
    Benchmark(
        "ycsb-kvs-full-varda-inmemory",
        ChainBuilder(
            [
                VardaBuilder(
                    BENCHMARKS_DIR/"bench-kvs"/"varda-full",
                    "dune exec --profile release -- vardac compile --places {{project_dir}}/places.yml --targets {{project_dir}}/targets.yml --filename {{project_dir}}/kv.varch --impl benchmarks/libbench.vimpl --impl {{project_dir}}/kv-wo-redis.vimpl --provenance 0 && cd compiler-build/akka && gradle -x test jarYCSBClient",
                    Path(os.getcwd()).absolute()),
                DockerComposeBuilder(
                    BENCHMARKS_DIR/"bench-kvs"/"varda-full",
                    Path(os.getcwd())/"compiler-build"/"akka"),
            ],
            # since Varda striclty stronger than docker
            stamp_strategy=lambda builders: builders[0]._get_bench_model(),
            # the one pass to runner
            exposed_builder=lambda builders: builders[-1]
        ),
        ShellRunnerFactory(
            f"java -classpath build/libs/YCSBClient.jar:{{{{env.YCSB}}}}/conf:{{{{env.YCSB}}}}/lib/HdrHistogram-2.1.4.jar:{{{{env.YCSB}}}}/lib/core-0.17.0.jar:{{{{env.YCSB}}}}/lib/htrace-core4-4.1.0-incubating.jar:{{{{env.YCSB}}}}/lib/jackson-core-asl-1.9.4.jar:{{{{env.YCSB}}}}/lib/jackson-mapper-asl-1.9.4.jar:{{{{env.YCSB}}}}/redis-binding/lib/commons-pool2-2.4.2.jar:{{{{env.YCSB}}}}/redis-binding/lib/jedis-2.9.0.jar:{{{{env.YCSB}}}}/redis-binding/lib/redis-binding-0.17.0.jar site.ycsb.Client -t -db author.project_name.YCSBClient -s -P {{{{env.YCSB}}}}/workloads/workloada > /tmp/ycsb-results",
            Path(os.getcwd())/"compiler-build"/"akka",
            "Terminated ueyiqu8R",
            environment = {
                'YCSB': BENCHMARKS_DIR/"ycsb-0.17.0"
            }
        ),
        [
            FileCollector("/tmp/ycsb-results", get_ycsb_result),
        ],
        Generator(RangeIterator({
            "threads": range(1, 2).__iter__(),
            "workload": ["workloada"].__iter__(),
        }), 3)
    ),
    Benchmark(
        "ycsb-redis",
        ShellBuilder(
            BENCHMARKS_DIR/"bench-kvs"/"varda",
            None,
            "docker run -d -e ALLOW_EMPTY_PASSWORD=yes -p 6379:6379 bitnami/redis:latest",
            Path(os.getcwd()).absolute(),
            clean_cmd='docker rm -f $(docker ps --filter="ancestor=bitnami/redis:latest")',
            build_each_time=True
        ),
        ShellRunnerFactory(
            f'java -classpath {{{{env.YCSB}}}}/conf:{{{{env.YCSB}}}}/lib/HdrHistogram-2.1.4.jar:{{{{env.YCSB}}}}/lib/core-0.17.0.jar:{{{{env.YCSB}}}}/lib/htrace-core4-4.1.0-incubating.jar:{{{{env.YCSB}}}}/lib/jackson-core-asl-1.9.4.jar:{{{{env.YCSB}}}}/lib/jackson-mapper-asl-1.9.4.jar:{{{{env.YCSB}}}}/redis-binding/lib/commons-pool2-2.4.2.jar:{{{{env.YCSB}}}}/redis-binding/lib/jedis-2.9.0.jar:{{{{env.YCSB}}}}/redis-binding/lib/redis-binding-0.17.0.jar site.ycsb.Client -t -db site.ycsb.db.RedisClient -s -P {{{{env.YCSB}}}}/workloads/workloada -p redis.host=127.0.0.1 -p redis.port=6379 > /tmp/ycsb-results',
            "/tmp",
            "Terminated ueyiqu8R",
            environment = {
                'YCSB': BENCHMARKS_DIR/"ycsb-0.17.0"
            }
        ),
        [
            FileCollector("/tmp/ycsb-results", get_ycsb_result),
        ],
        Generator(RangeIterator({
            "threads": range(1, 2).__iter__(),
            "workload": ["workloada"].__iter__(),
        }), 3)
    ),
    Benchmark(
        "ycsb-b-kvs-varda-redis",
        ChainBuilder(
            [
                VardaBuilder(
                    BENCHMARKS_DIR/"bench-kvs"/"varda",
                    "dune exec --profile release -- vardac compile --places {{project_dir}}/places.yml --targets {{project_dir}}/targets.yml --filename {{project_dir}}/kv.varch --impl benchmarks/libbench.vimpl --impl {{project_dir}}/kv-redis.vimpl --provenance 0 && cd compiler-build/akka && gradle -x test jarYCSBClient",
                    Path(os.getcwd()).absolute()),
                DockerComposeBuilder(
                    BENCHMARKS_DIR/"bench-kvs"/"varda",
                    Path(os.getcwd())/"compiler-build"/"akka"),
            ],
            # since Varda one is strictly stronger than docker
            stamp_strategy=lambda builders: builders[0]._get_bench_model(),
            # the one pass to runner
            exposed_builder=lambda builders: builders[-1]
        ),
        ShellRunnerFactory(
            f"java -classpath build/libs/YCSBClient.jar:{{{{env.YCSB}}}}/conf:{{{{env.YCSB}}}}/lib/HdrHistogram-2.1.4.jar:{{{{env.YCSB}}}}/lib/core-0.17.0.jar:{{{{env.YCSB}}}}/lib/htrace-core4-4.1.0-incubating.jar:{{{{env.YCSB}}}}/lib/jackson-core-asl-1.9.4.jar:{{{{env.YCSB}}}}/lib/jackson-mapper-asl-1.9.4.jar:{{{{env.YCSB}}}}/redis-binding/lib/commons-pool2-2.4.2.jar:{{{{env.YCSB}}}}/redis-binding/lib/jedis-2.9.0.jar:{{{{env.YCSB}}}}/redis-binding/lib/redis-binding-0.17.0.jar site.ycsb.Client -t -db author.project_name.YCSBClient -s -P {{{{env.YCSB}}}}/workloads/workloadb > /tmp/ycsb-results",
            Path(os.getcwd())/"compiler-build"/"akka",
            "Terminated ueyiqu8R",
            environment = {
                'YCSB': BENCHMARKS_DIR/"ycsb-0.17.0"
            }
        ),
        [
            FileCollector("/tmp/ycsb-results", get_ycsb_result),
        ],
        Generator(RangeIterator({
            "threads": range(1, 2).__iter__(),
            "workload": ["workloadb"].__iter__(),
        }), 3)
    ),
    Benchmark(
        "ycsb-b-kvs-varda-inmemory",
        ChainBuilder(
            [
                VardaBuilder(
                    BENCHMARKS_DIR/"bench-kvs"/"varda",
                    "dune exec --profile release -- vardac compile --places {{project_dir}}/places.yml --targets {{project_dir}}/targets.yml --filename {{project_dir}}/kv.varch --impl benchmarks/libbench.vimpl --impl {{project_dir}}/kv-wo-redis.vimpl --provenance 0 && cd compiler-build/akka && gradle -x test jarYCSBClient",
                    Path(os.getcwd()).absolute()),
                DockerComposeBuilder(
                    BENCHMARKS_DIR/"bench-kvs"/"varda",
                    Path(os.getcwd())/"compiler-build"/"akka"),
            ],
            # since Varda striclty stronger than docker
            stamp_strategy=lambda builders: builders[0]._get_bench_model(),
            # the one pass to runner
            exposed_builder=lambda builders: builders[-1]
        ),
        ShellRunnerFactory(
            f"java -classpath build/libs/YCSBClient.jar:{{{{env.YCSB}}}}/conf:{{{{env.YCSB}}}}/lib/HdrHistogram-2.1.4.jar:{{{{env.YCSB}}}}/lib/core-0.17.0.jar:{{{{env.YCSB}}}}/lib/htrace-core4-4.1.0-incubating.jar:{{{{env.YCSB}}}}/lib/jackson-core-asl-1.9.4.jar:{{{{env.YCSB}}}}/lib/jackson-mapper-asl-1.9.4.jar:{{{{env.YCSB}}}}/redis-binding/lib/commons-pool2-2.4.2.jar:{{{{env.YCSB}}}}/redis-binding/lib/jedis-2.9.0.jar:{{{{env.YCSB}}}}/redis-binding/lib/redis-binding-0.17.0.jar site.ycsb.Client -t -db author.project_name.YCSBClient -s -P {{{{env.YCSB}}}}/workloads/workloadb > /tmp/ycsb-results",
            Path(os.getcwd())/"compiler-build"/"akka",
            "Terminated ueyiqu8R",
            environment = {
                'YCSB': BENCHMARKS_DIR/"ycsb-0.17.0"
            }
        ),
        [
            FileCollector("/tmp/ycsb-results", get_ycsb_result),
        ],
        Generator(RangeIterator({
            "threads": range(1, 2).__iter__(),
            "workload": ["workloadb"].__iter__(),
        }), 3)
    ),
    Benchmark(
        "ycsb-b-redis",
        ShellBuilder(
            BENCHMARKS_DIR/"bench-kvs"/"varda",
            None,
            "docker run -d -e ALLOW_EMPTY_PASSWORD=yes -p 6379:6379 bitnami/redis:latest",
            Path(os.getcwd()).absolute(),
            clean_cmd='docker rm -f $(docker ps --filter="ancestor=bitnami/redis:latest")',
            build_each_time=True
        ),
        ShellRunnerFactory(
            f'{{{{env.YCSB}}}}/bin/ycsb.sh run redis -s -P {{{{env.YCSB}}}}/workloads/workloadb -p "redis.host=127.0.0.1" -p "redis.port=6379" > /tmp/ycsb-results',
            "/tmp",
            "Terminated ueyiqu8R",
            environment = {
                'YCSB': BENCHMARKS_DIR/"ycsb-0.17.0"
            }
        ),
        [
            FileCollector("/tmp/ycsb-results", get_ycsb_result),
        ],
        Generator(RangeIterator({
            "threads": range(1, 2).__iter__(),
            "workload": ["workloadb"].__iter__(),
        }), 3)
    ),
    Benchmark(
        "ycsb-b-kvs-full-akka-inmemory",
        DockerComposeBuilder(
            BENCHMARKS_DIR/"bench-kvs"/"akka",
            BENCHMARKS_DIR/"bench-kvs"/"akka"),
        ShellRunnerFactory(
            f"java -classpath build/libs/YCSBClient.jar:{{{{env.YCSB}}}}/conf:{{{{env.YCSB}}}}/lib/HdrHistogram-2.1.4.jar:{{{{env.YCSB}}}}/lib/core-0.17.0.jar:{{{{env.YCSB}}}}/lib/htrace-core4-4.1.0-incubating.jar:{{{{env.YCSB}}}}/lib/jackson-core-asl-1.9.4.jar:{{{{env.YCSB}}}}/lib/jackson-mapper-asl-1.9.4.jar:{{{{env.YCSB}}}}/redis-binding/lib/commons-pool2-2.4.2.jar:{{{{env.YCSB}}}}/redis-binding/lib/jedis-2.9.0.jar:{{{{env.YCSB}}}}/redis-binding/lib/redis-binding-0.17.0.jar site.ycsb.Client -t -db com.varda.YCSBClient -s -P {{{{env.YCSB}}}}/workloads/workloadb > /tmp/ycsb-results",
            BENCHMARKS_DIR/"bench-kvs"/"akka",
            "Terminated ueyiqu8R",
            environment = {
                'YCSB': BENCHMARKS_DIR/"ycsb-0.17.0"
            }
        ),
        [
            FileCollector("/tmp/ycsb-results", get_ycsb_result),
        ],
        Generator(RangeIterator({
            "threads": range(1, 2).__iter__(),
            "workload": ["workloadb"].__iter__(),
        }), 3)
    ),
    Benchmark(
        "ycsb-b-kvs-full-varda-inmemory",
        ChainBuilder(
            [
                VardaBuilder(
                    BENCHMARKS_DIR/"bench-kvs"/"varda-full",
                    "dune exec --profile release -- vardac compile --places {{project_dir}}/places.yml --targets {{project_dir}}/targets.yml --filename {{project_dir}}/kv.varch --impl benchmarks/libbench.vimpl --impl {{project_dir}}/kv-wo-redis.vimpl --provenance 0 && cd compiler-build/akka && gradle -x test jarYCSBClient",
                    Path(os.getcwd()).absolute()),
                DockerComposeBuilder(
                    BENCHMARKS_DIR/"bench-kvs"/"varda-full",
                    Path(os.getcwd())/"compiler-build"/"akka"),
            ],
            # since Varda striclty stronger than docker
            stamp_strategy=lambda builders: builders[0]._get_bench_model(),
            # the one pass to runner
            exposed_builder=lambda builders: builders[-1]
        ),
        ShellRunnerFactory(
            f"java -classpath build/libs/YCSBClient.jar:{{{{env.YCSB}}}}/conf:{{{{env.YCSB}}}}/lib/HdrHistogram-2.1.4.jar:{{{{env.YCSB}}}}/lib/core-0.17.0.jar:{{{{env.YCSB}}}}/lib/htrace-core4-4.1.0-incubating.jar:{{{{env.YCSB}}}}/lib/jackson-core-asl-1.9.4.jar:{{{{env.YCSB}}}}/lib/jackson-mapper-asl-1.9.4.jar:{{{{env.YCSB}}}}/redis-binding/lib/commons-pool2-2.4.2.jar:{{{{env.YCSB}}}}/redis-binding/lib/jedis-2.9.0.jar:{{{{env.YCSB}}}}/redis-binding/lib/redis-binding-0.17.0.jar site.ycsb.Client -t -db author.project_name.YCSBClient -s -P {{{{env.YCSB}}}}/workloads/workloadb > /tmp/ycsb-results",
            Path(os.getcwd())/"compiler-build"/"akka",
            "Terminated ueyiqu8R",
            environment = {
                'YCSB': BENCHMARKS_DIR/"ycsb-0.17.0"
            }
        ),
        [
            FileCollector("/tmp/ycsb-results", get_ycsb_result),
        ],
        Generator(RangeIterator({
            "threads": range(1, 2).__iter__(),
            "workload": ["workloadb"].__iter__(),
        }), 3)
    ),
]