#!/usr/bin/python3
# coding: utf-8

import argparse
import logging
import os
import re
import shlex
import shutil
import subprocess
import sys
import tempfile

from collections import defaultdict
from jinja2 import Template

parser   = argparse.ArgumentParser()

subparsers = parser.add_subparsers(help='sub-command help', dest="cmdaction")
parser_run = subparsers.add_parser('run', help='TODO')

args = parser.parse_args()

cmdactions = defaultdict(lambda *args: parser.print_help())
cmdactions['run'] = lambda kwargs: do_run()

class Benchmark:
    def __init__(self, name, build_cmd, run_cmd, extract_results_cmd) -> None:
        self.name                   = name
        self.build_cmd              = build_cmd
        self.run_cmd                = run_cmd
        self.extract_results_cmd    = extract_results_cmd

        self.build_stdout           = ""
        self.build_stderr           = ""

    def build(self):
        res = subprocess.run(
            self.build_cmd, 
            capture_output=True, 
            encoding='utf-8', 
            cwd=self.article_directory())

        self.build_stdout = res.stdout
        self.build_stderr = res.stderr
        assert(res.returncode == 0) 

    def run(self):
        res = subprocess.run(
            self.run_cmd, 
            capture_output=True, 
            encoding='utf-8', 
            cwd=self.article_directory())

        self.run_stdout = res.stdout
        self.run_stderr = res.stderr
        assert(res.returncode == 0) 

    def extract_results(self):
        res = subprocess.run(
            self.extract_results_cmd, 
            capture_output=True, 
            encoding='utf-8', 
            cwd=self.article_directory())

        self.extract_results_stdout = res.stdout
        self.extract_results_stderr = res.stderr
        assert(res.returncode == 0) 

    def start(self):
        self.build()
        self.run()
        return self.extract_results

benchmarks = [
    Benchmark(
        "simpl-com-jvm",
        "make run_cmd -- compile --places benchmarks/bench-simpl-com/places.yml --targets benchmarks/bench-simpl-com/targets.yml --filename benchmarks/bench-simpl-com/bench.spec --impl benchmarks/bench-simpl-com/bench.impl --provenance 0 && cd compiler-build/akka && make",
        "java -enableassertions -jar build/libs/main.jar -ip 127.0.0.1 -p 25520 -s akka://systemProject_name@127.0.0.1:25520 -l 8080 -vp placeB",
        lambda self : self.extract_results_stdout
    )
]

def do_run():
    for bench in benchmarks:
        res = bench.start()
        print(res)
    

cmdaction = args.cmdaction
kwargs = vars(args)
del kwargs['cmdaction']
cmdactions[cmdaction](kwargs)