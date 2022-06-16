#!/usr/bin/python3
# coding: utf-8

import argparse
import logging
import coloredlogs
import os
import re
import shlex
import subprocess
import time
import select
import numpy
import json
import os
import re
import signal

os.environ.setdefault("DJANGO_SETTINGS_MODULE", "orm.settings")
import django
django.setup()

from pathlib import Path

from checksumdir import dirhash
from src.models import *


from collections import defaultdict
from jinja2 import Template
from sympy import Abs

coloredlogs.install(level='DEBUG')
logging.basicConfig(format='%(levelname)s:%(message)s', encoding='utf-8', level=logging.DEBUG)


parser   = argparse.ArgumentParser()

subparsers = parser.add_subparsers(help='sub-command help', dest="cmdaction")
parser_run = subparsers.add_parser('run', help='TODO')
parser_run.add_argument("--bench-selector", default="all",
                    help=f"all or name1:name2:")

args = parser.parse_args()

cmdactions = defaultdict(lambda *args: parser.print_help())
cmdactions['run'] = lambda kwargs: do_run(**kwargs)

class AbstractBuilder:
    def __init__(self, name, project_dir=None, build_dir=None, build_cmd=None, build_cwd=None) -> None:
        self.name= name
        self.project_dir = project_dir
        self.build_dir = build_dir

        self.build_cmd = build_cmd
        self.build_cwd = build_cwd

        self.build_stdout           = ""
        self.build_stderr           = ""

        self._is_build = False


    def get_bench_model(self):
        bench, flag = Bench.objects.get_or_create(
            name = self.name,
            host_spec = HostSpec.objects.get_or_create(name = HostSpec.get_hostname())[0],    
            soft_spec = SoftSpec.objects.get_or_create(
                os_version = SoftSpec.get_os_version(),
                docker_version = SoftSpec.get_docker_version(),
                varda_version = SoftSpec.get_varda_version(),
                )[0],    
            project_hash = dirhash(self.project_dir, 'md5'),
            build_hash = dirhash(self.build_dir, 'md5'),
            build_cmd = self.build_cmd 
        )

        if flag:
            logging.debug(f"Bench {self.name}> Create bench model {bench.id} in DB !")
        else:
            logging.debug(f"Bench {self.name}> Load bench model {bench.id} from DB !\n\t(new results will be append to it)")

        return bench, flag

    @property
    def is_build(self):
        if not self._is_build and self.build_dir and self.project_dir:
            _, is_created = self.get_bench_model()
            if not is_created:
                self._is_build = True
        return self._is_build

    def _build(self) -> bool:
        res = subprocess.run(
            self.build_cmd, 
            capture_output=True, 
            encoding='utf-8',
            cwd = self.build_cwd,
            shell=True)

        self.build_stdout = res.stdout
        self.build_stderr = res.stderr
        return res.returncode == 0


    def build(self, *args, **kwargs):
        if not self.is_build:
            logging.info(f"Bench {self.name}> Building !")
            tmp = self._build(*args, **kwargs)
            self._is_build = True
            b, _ = self.get_bench_model()
            if b:
                logging.info(f"Bench {self.name}> Built !")
            return tmp, b
        else:
            logging.info(f"Bench {self.name}> Already built !")
            return True, self.get_bench_model()[0]



class VardaBuilder(AbstractBuilder):
    def __init__(self, name, project_dir, build_cmd, build_cwd, build_dir=Path(os.getcwd())/"compiler-build") -> None:
        super().__init__(name, project_dir, build_dir, build_cmd, build_cwd)


RUN_TIMEOUT = 30 # s
class ShellRunnerFactory:
    def __init__(self, run_cmd, run_cwd=None, stdout_termination_token=None, error_token="[ERROR]") -> None:
        self.run_cmd                    = run_cmd
        self.run_cwd                    = run_cwd
        self.stdout_termination_token   = stdout_termination_token
        self.error_token                = error_token

    def make(self, config):
        return ShellRunner(self.run_cmd, self.run_cwd, self.stdout_termination_token, self.error_token, config)

class ShellRunner:
    def __init__(self, run_cmd, run_cwd, stdout_termination_token, error_token, config) -> None:
        self.run_cmd    = run_cmd
        self.run_cwd    = run_cwd
        self.stdout_termination_token = stdout_termination_token
        self.error_token    = error_token

        self.config =   config
        
        self.run_stdout           = ""
        self.run_stderr           = ""

    def render(self, snapshot):
        return " ".join([f"-{k} {shlex.quote(str(v))}" for k,v in snapshot.items()])
    
    def run(self) -> bool:
        start_time = time.time()
        res = subprocess.Popen(
            self.run_cmd+" "+self.render(self.config), 
            stdout= subprocess.PIPE,
            stderr= subprocess.PIPE,
            encoding='utf-8', 
            cwd=self.run_cwd,
            shell=True,
            preexec_fn=os.setpgrp,
            )

        poll_obj = select.poll()
        poll_obj.register(res.stdout, select.POLLIN)

        stdout_buffer = ""
        last_elapse = 0
        while res.returncode == None and not time.time() - start_time > RUN_TIMEOUT :
            poll_result = poll_obj.poll(0)
            if int(time.time() - start_time) > last_elapse:
                last_elapse = int(time.time() - start_time)
                print(f"Ran for {last_elapse} s [Timeout: {RUN_TIMEOUT}s]")
            if poll_result:
                stdout_buffer += res.stdout.readline()
                if self.stdout_termination_token and self.stdout_termination_token in stdout_buffer:
                    os.killpg(os.getpgid(res.pid),signal.SIGTERM)
                    res.wait()
                    self.run_stdout = stdout_buffer
                    return True
                if self.error_token and self.error_token in stdout_buffer:
                    os.killpg(os.getpgid(res.pid),signal.SIGTERM)
                    res.wait()
                    self.run_stdout = stdout_buffer
                    self.run_stderr = res.stderr.read()
                    print(stdout_buffer)
                    print(self.run_stderr)
                    return False

        if res.returncode:
            self.run_stdout = stdout_buffer 
            self.run_stderr = res.stderr.read()
            return res.returncode == 0
        else:
            os.killpg(os.getpgid(res.pid),signal.SIGTERM)
            res.wait()
            print(stdout_buffer)
            print("Timeout !")
            return False
        



class AbstractCollector:
    def __init__(self) -> None:
        pass

    def collect(self, runner):
        pass

class ShellCollector(AbstractCollector):
    def __init__(self, collect_cmd, collect_cwd=None) -> None:
        super().__init__()
        self.collect_cmd    = collect_cmd
        self.collect_cwd    = collect_cwd
        
        self.collect_stdout           = ""
        self.collect_stderr           = ""
    
    def collect(self, runner):
        res = subprocess.collect(
            self.collect_cmd, 
            capture_output=True, 
            encoding='utf-8', 
            cwd=self.collect_cwd, 
            shell=True)

        self.collect_stdout = res.stdout
        self.collect_stderr = res.stderr
        assert(res.returncode == 0) 
        return True

class StdoutCollector(AbstractCollector):
    def __init__(self, stdlambda) -> None:
        super().__init__()
        self.stdcollect = stdlambda

    def collect(self, runner):
       return self.stdcollect(runner.run_stdout) 

class FileCollector(AbstractCollector):
    def __init__(self, filename, flambda) -> None:
        super().__init__()
        self.filename = filename
        self.flambda = flambda

    def collect(self, runner):
        if os.path.exists(self.filename):
            return self.flambda(self.filename)
        else:
            logging.warning(f"filename {self.filename} does not exists !!")
            return []

class RangeIterator:
    # def = {
    #   pname_1: range
    # }
    #
    def __init__(self, defs):
        self.defs = defs

        self.keys = self.defs.keys()
        self.last = 0
        self.init = True
        self.closed = set()

        self.snapshot   = [ r.__next__() for k, r in self.defs.items()]
        self.pos2key    = { i: k for i, k in enumerate(self.defs.keys())}

    def prepare(self, snapshot):
        return { list(self.defs.keys())[i]: v for i, v in enumerate(snapshot)}

    def __iter__(self):
        return self

    def __next__(self):
        if len(self.closed) == len(self.defs):
            raise StopIteration
        if self.last in self.closed:
            return self.__next__()
        if self.init:
            self.init = False
            return self.prepare(self.snapshot)

        try:
            self.snapshot[self.last] = self.defs[self.pos2key[self.last]].__next__()
            self.last = (self.last + 1) % len(self.keys)
            return self.prepare(self.snapshot)
        except StopIteration:
            self.closed.add(self.last)
            print(self.closed, self.last)
            return self.__next__()


class Benchmark:
    def __init__(self, name, builder, runner_factory, collectors, generator) -> None:
        self.name                   = name
        self.builder                = builder
        self.runner_factory         = runner_factory
        self.collectors             = collectors
        self.generator              = generator

        self.bench                  = None

    def build(self):
        flag, bench = self.builder.build()
        self.bench  = bench
        return flag

    def run(self) -> bool:
        flag = True
        for config in self.generator.config_range:
            for i, _ in enumerate(range(self.generator.n_run)):
                logging.info(f"Bench {self.name}> Start run {i+1}/{self.generator.n_run} for config {config}")

                runner = self.runner_factory.make(config)

                flag = flag and runner.run()
                res = self.collect_results(runner)
                print(res)
                tmp = BenchResult.objects.create(run_config=res["config"], results=res['results'])
                self.bench.results.add(tmp)
                self.bench.save()
                print(tmp)
                logging.info(f"Bench {self.name}> Collected results !")
                if not flag:
                    logging.error(f"Bench {self.name}> Run failure !\n"+runner.build_stderr)
        return flag 

    def collect_results(self, runner):
        res = []
        for collector in self.collectors:
            tmp = collector.collect(runner)
            if tmp:
                res.append(tmp)
        return {'config': runner.config, 'results': res}

    def start(self):
        logging.info(f"Bench {self.name}> Started !")

        if not self.build():
            logging.error(f"Bench {self.name}> Built failure !")
            print(self.builder.build_stderr)
            return False

        if not self.run():
            return False

        logging.info(f"Bench {self.name}> End !")


def logrange(start, end, base):
    for x in numpy.logspace(start,end,base=10, num = end-start+1, dtype='int'):
        yield x

def get_elapse_time(stdout):
    res = re.search('Time elapse (\d+) ms', stdout)
    return {"duration": {"unit":"ms", "value":
            res.group(1) if res else "N/A"},
    }

def get_rtts(filename):
    with open(filename) as fp:
        return {"rtt": {"unit": "ms", "value":json.load(fp)}}

class Generator:
    def __init__(self, config_range, n_run=1):
        self.config_range   = config_range
        self.n_run          = n_run

BENCHMARKS = [
    Benchmark(
        "simpl-com-jvm-varda",
        VardaBuilder("simpl-com-jvm-varda", "benchmarks/bench-simpl-com/varda", "dune exec --profile release -- compspec compile --places benchmarks/bench-simpl-com/varda/places.yml --targets benchmarks/bench-simpl-com/varda/targets.yml --filename benchmarks/bench-simpl-com/varda/bench.spec --impl benchmarks/bench-simpl-com/varda/bench.impl --provenance 0 && cd compiler-build/akka && sed -i 's/DEBUG/INFO/g' src/main/resources/logback.xml && make", Path(os.getcwd()).absolute()),
        ShellRunnerFactory(
            "java -enableassertions -jar build/libs/main.jar -ip 127.0.0.1 -p 25520 -s akka://systemProject_name@127.0.0.1:25520 -l 8080 -vp placeB", 
            Path(os.getcwd())/"compiler-build"/"akka", 
            "Terminated ueyiqu8R"
        ),
        [ 
            StdoutCollector(get_elapse_time),
            FileCollector(Path(os.getcwd())/"compiler-build"/"akka"/"rtts.json", get_rtts),
        ],
        Generator(RangeIterator({
            "n": logrange(1, 5, base=10),
            "warmup": range(1000, 1000+1).__iter__()
        }), 3)
    ),
    Benchmark(
        "simpl-com-jvm-akka",
        VardaBuilder("simpl-com-jvm-akka", "benchmarks/bench-simpl-com/akka", "cd benchmarks/bench-simpl-com/akka && make", Path(os.getcwd()).absolute()),
        ShellRunnerFactory(
            "java -enableassertions -jar build/libs/main.jar", 
            Path(os.getcwd())/"benchmarks"/"bench-simpl-com"/"akka", 
            "Terminated ueyiqu8R" 
        ),
        [ 
            StdoutCollector(get_elapse_time),
            FileCollector(Path(os.getcwd())/"benchmarks"/"bench-simpl-com"/"akka"/"rtts.json", get_rtts),
        ],
        Generator(RangeIterator({
            "n": logrange(1, 2, base=10),
            "warmup": range(1000, 1000+1).__iter__()
            }), 3)
    ),

]

def do_run(bench_selector):
    if bench_selector == "all":
        benchmarks = BENCHMARKS
    else:
        selected_bench_names = set(bench_selector.split(":"))
        benchmarks = [ b for b in BENCHMARKS if b.name in selected_bench_names]
    for bench in benchmarks:
        res = bench.start()
        print(res)
    

cmdaction = args.cmdaction
kwargs = vars(args)
del kwargs['cmdaction']
cmdactions[cmdaction](kwargs)