from .utils import *

from src.bench import *
from src.collectors import *
from src.builders import *
from src.runners import *
from src.citerators import *
from src.cgenerators import *
 
BENCHMARKS = [
    # Mono jvm
    Benchmark(
        "simpl-com-varda-one-jvm",
        VardaBuilder("simpl-com-varda-one-jvm", "benchmarks/bench-simpl-com/varda", "dune exec --profile release -- compspec compile --places benchmarks/bench-simpl-com/varda/places.yml --targets benchmarks/bench-simpl-com/varda/targets.yml --filename benchmarks/bench-simpl-com/varda/bench.spec --impl benchmarks/bench-simpl-com/varda/bench.impl --provenance 0 && cd compiler-build/akka && sed -i 's/DEBUG/INFO/g' src/main/resources/logback.xml && make", Path(os.getcwd()).absolute()),
        ShellRunnerFactory(
            "simpl-com-varda-one-jvm",
            "java -enableassertions -jar build/libs/main.jar -ip 127.0.0.1 -p 25520 -s akka://systemProject_name@127.0.0.1:25520 -l 8080 -vp placeB", 
            Path(os.getcwd())/"compiler-build"/"akka", 
            "Terminated ueyiqu8R"
        ),
        [ 
            StdoutCollector(get_elapse_time),
            FileCollector(Path(os.getcwd())/"compiler-build"/"akka"/"rtts.json", get_rtts),
        ],
        Generator(RangeIterator({
            "n": logrange(1, 4, base=10),
            "warmup": range(1000, 1000+1).__iter__()
        }), 3)
    ),
    Benchmark(
        "simpl-com-akka-one-jvm",
        VardaBuilder("simpl-com-akka-one-jvm", "benchmarks/bench-simpl-com/akka", "cd benchmarks/bench-simpl-com/akka && make", Path(os.getcwd()).absolute()),
        ShellRunnerFactory(
            "simpl-com-akka-one-jvm",
            "java -enableassertions -jar build/libs/main.jar", 
            Path(os.getcwd())/"benchmarks"/"bench-simpl-com"/"akka", 
            "Terminated ueyiqu8R" 
        ),
        [ 
            StdoutCollector(get_elapse_time),
            FileCollector(Path(os.getcwd())/"benchmarks"/"bench-simpl-com"/"akka"/"rtts.json", get_rtts),
        ],
        Generator(RangeIterator({
            "n": logrange(1, 4, base=10),
            "warmup": range(1000, 1000+1).__iter__()
            }), 3)
    ),
    # Multi jvm
    Benchmark(
        "simpl-com-akka-multi-jvms",
        VardaBuilder("simpl-com-akka-multi-jvm", "benchmarks/bench-simpl-com/akka", "cd benchmarks/bench-simpl-com/akka && make", Path(os.getcwd()).absolute()),
        MultiShellRunnerFactory(
            "simpl-com-akka-multi-jvms",
            [
                ShellRunnerFactory(
                    "runner-pong",
                    "java -enableassertions -jar build/libs/pongService.jar -ip 127.0.0.1 -p 25520 -s akka://systemAkkaBench@127.0.0.1:25520", 
                    Path(os.getcwd())/"benchmarks"/"bench-simpl-com"/"akka", 
                    None,
                    config_adaptor=lambda config: remove_dict(config, ["n", "warmup"]) 
                ),
                ShellRunnerFactory(
                    "runner-ping",
                    "java -enableassertions -jar build/libs/pingService.jar -ip 127.0.0.1 -p 25521 -s akka://systemAkkaBench@127.0.0.1:25520", 
                    Path(os.getcwd())/"benchmarks"/"bench-simpl-com"/"akka", 
                    "Terminated ueyiqu8R",
                    set_stop_event=True
                )]),
        [ 
            StdoutCollector(get_elapse_time),
            FileCollector(Path(os.getcwd())/"benchmarks"/"bench-simpl-com"/"akka"/"rtts.json", get_rtts),
        ],
        Generator(RangeIterator({
            "n": logrange(1, 4, base=10),
            "warmup": range(1000, 1000+1).__iter__()
            }), 3)
    ),
]