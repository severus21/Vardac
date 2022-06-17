#!/usr/bin/python3
# coding: utf-8

import argparse
import logging
import coloredlogs
import os

os.environ.setdefault("DJANGO_SETTINGS_MODULE", "orm.settings")
import django
django.setup()

from collections import defaultdict

from config.benchmarks import BENCHMARKS 

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

def do_run(bench_selector):
    if bench_selector == "all":
        benchmarks = BENCHMARKS
    else:
        selected_bench_names = set(bench_selector.split(":"))
        benchmarks = [ b for b in BENCHMARKS if b.name in selected_bench_names]

    n_error = 0
    for bench in benchmarks:
        tmp_flag = bench.start()

        n_error += int(not tmp_flag)

    if n_error > 0:
        logging.error(f"{n_error} benchmarks ha{'ve' if n_error > 1 else 's'} failed !")

cmdaction = args.cmdaction
kwargs = vars(args)
del kwargs['cmdaction']
cmdactions[cmdaction](kwargs)