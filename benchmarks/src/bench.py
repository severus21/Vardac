import logging

from .models import *

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
        for collector in self.collectors:
            collector.clean()

        flag = True
        for config in self.generator.config_range:
            for i, _ in enumerate(range(self.generator.n_run)):
                logging.info(f"Bench {self.name}> Start run {i+1}/{self.generator.n_run} for config {config}")

                runner = self.runner_factory.make(config)

                flag = flag and runner.run_async()
                #flag = flag and runner.run()
                res = self.collect_results(runner)
                tmp = BenchResult.objects.create(run_config=res["config"], results=res['results'])
                self.bench.results.add(tmp)
                self.bench.save()
                logging.info(f"Bench {self.name}> Collected results !")
                if not flag:
                    logging.error(f"Bench {self.name}> Run failure !\n"+runner.run_stderr+"\n"+runner.run_stdout)
        return flag 

    def collect_results(self, runner):
        res = {} 
        for collector in self.collectors:
            tmp = collector.collect(runner)
            if tmp:
                res = res | tmp
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