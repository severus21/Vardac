#!/usr/bin/python3
# coding: utf-8

import asyncio
from asyncio import subprocess
from collections import defaultdict
import argparse
import logging
import coloredlogs
import os
import re
import itertools
import time

os.environ.setdefault("DJANGO_SETTINGS_MODULE", "orm.settings")
import django
django.setup()


from src.utils import *
from src.settings import *


coloredlogs.install(level='DEBUG')
logging.basicConfig(format='%(levelname)s:%(message)s',
                    encoding='utf-8', level=logging.DEBUG)
logging.getLogger('matplotlib').setLevel(logging.INFO)


parser = argparse.ArgumentParser()

subparsers = parser.add_subparsers(help='sub-command help', dest="cmdaction")
parser_count = subparsers.add_parser('count', help='TODO')
parser_count.add_argument("--count-selector", default="all",
                        help=f"all or name1:name2:")
parser_count.add_argument("--use_re", default=False, help=f"TODO", action='store_true')
parser_run = subparsers.add_parser('run', help='TODO')
parser_run.add_argument("--bench-selector", default="all",
                        help=f"all or name1:name2:")
parser_run.add_argument("--use_re", default=False, help=f"TODO", action='store_true')
parser_render = subparsers.add_parser('render', help='TODO')
parser_render.add_argument(
    "--save", action='store_true', default=False, help='save figure to disk')
parser_render.add_argument("--fig-selector", default="all",
                           help=f"all or name1:name2:")
parser_render.add_argument("--use_re", default=False, help=f"TODO", action='store_true')
parser_test = subparsers.add_parser('test', help='TODO')

args = parser.parse_args()

cmdactions = defaultdict(lambda *args: parser.print_help())
cmdactions['count'] = lambda kwargs: do_count(**kwargs)
cmdactions['run'] = lambda kwargs: do_run(**kwargs)
cmdactions['render'] = lambda kwargs: do_render(**kwargs)
cmdactions['test'] = lambda kwargs: do_test(**kwargs)


def apply_selector(selector, ITEMS, use_re=False):
    if selector == "all":
        items = ITEMS
    else:
        if not use_re:
            selected_item_names = set(selector.split(":"))
            items = [f for f in ITEMS if f.name in selected_item_names]
        else:
            p = re.compile(selector)
            items = [ f for f in ITEMS if p.match(f.name)]
    return items

def do_count(count_selector, use_re=False):
    from config.counts import COUNTS
    from src.settings import COUNTSDIR
    import subprocess

    counts = apply_selector(count_selector, COUNTS)
    for count in counts:
        count.run()

    path = os.getcwd()
    subprocess.run(f'cd {COUNTSDIR}/ && make && cd {path}', shell=True)


def do_render(save, fig_selector, use_re=False):
    from config.figures import FIGURES

    if fig_selector == "all":
        figures = FIGURES
    else:
        if not use_re:
            selected_fig_names = set(fig_selector.split(":"))
            figures = [f for f in FIGURES if f.title in selected_fig_names]
        else:
            p = re.compile(fig_selector)
            figures = [ f for f in FIGURES if p.match(f.title)]

    figures = list(itertools.chain.from_iterable([ f.compare() for f in figures]))

    if not figures:
        print("No selected figures!")
        print("\t- "+"\n\t- ".join([b.title for b in FIGURES]))

    for fig in figures:
        if save:
            from src.provenance import get_current_stamp
            import json
            fig.filename = os.path.join(FIGURESDIR, normalize_path(fig.title+'.png'))
            with open(os.path.join(FIGURESDIR, normalize_path(f'{fig.title}.json')), 'w') as f:
                # TODO write more provenance data
                json.dump({'stamp': get_current_stamp()}, f)
        fig.render()


def do_run(bench_selector, use_re=False):
    from config.benchmarks import BENCHMARKS

    if bench_selector == "all":
        benchmarks = BENCHMARKS
    else:

        if not use_re:
            selected_bench_names = set(bench_selector.split(":"))
            benchmarks = [b for b in BENCHMARKS if b.name in selected_bench_names]
        else:
            p = re.compile(bench_selector)
            benchmarks = [ b for b in BENCHMARKS if p.match(b.name)]

    if not benchmarks:
        print("No selected benchmarks!")
        print("\t- "+"\n\t- ".join([b.name for b in BENCHMARKS]))

    n_error = 0

    loop = asyncio.get_event_loop()
    start_time = time.time_ns()
    try:
        for bench in benchmarks:
            with bench: 
                tmp_flag = bench.start()

                n_error += int(not tmp_flag)

        if n_error > 0:
            logging.error(
                f"{n_error} benchmark{'s' if n_error > 1 else ''} ha{'ve' if n_error > 1 else 's'} failed !")

        logging.info(f"Benchmarks are done! in {int((time.time_ns()-start_time)/1000/1000/1000)} s")
    finally:
        logging.info("Closing loop in main")
        loop.close()

def do_test():
    from src.runners import DockerRunner, ShellRunner
    from src.builders import DockerBuilder

    pass
    #b = DockerBuilder("testdockerbuilder", os.path.join(
    #    os.path.dirname(os.path.realpath(__file__)), 'bench-ms/akka'))
    #b._build()

    #d = DockerRunner("testdockerbuilder", b.image_name(),
    #    "/usr/local/openjdk-11/bin/java -enableassertions -jar main.jar -n 1 -warmup 0 -vs 6"
    #, "world", "error", {}, False)
    # s = ShellRunner("toto", "echo world", os.path.join(os.path.dirname(os.path.realpath(__file__)), 'src/tests/testimg'),
    # "world", "error", {}, False)
    #d.run()
    #print(d.run_stdout)
    # s.run()


cmdaction = args.cmdaction
kwargs = vars(args)
del kwargs['cmdaction']
cmdactions[cmdaction](kwargs)
