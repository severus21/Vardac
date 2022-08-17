#!/usr/bin/python3
# coding: utf-8

import asyncio
from collections import defaultdict
import argparse
import logging
import coloredlogs
import os

os.environ.setdefault("DJANGO_SETTINGS_MODULE", "orm.settings")
import django
django.setup()

from config.figures import FIGURES
from config.benchmarks import BENCHMARKS


coloredlogs.install(level='DEBUG')
logging.basicConfig(format='%(levelname)s:%(message)s',
                    encoding='utf-8', level=logging.DEBUG)
logging.getLogger('matplotlib').setLevel(logging.INFO)


parser = argparse.ArgumentParser()

subparsers = parser.add_subparsers(help='sub-command help', dest="cmdaction")
parser_run = subparsers.add_parser('run', help='TODO')
parser_run.add_argument("--bench-selector", default="all",
                        help=f"all or name1:name2:")
parser_render = subparsers.add_parser('render', help='TODO')
parser_render.add_argument(
    "--save", action='store_true', default=False, help='save figure to disk')
parser_render.add_argument("--fig-selector", default="all",
                           help=f"all or name1:name2:")
parser_test = subparsers.add_parser('test', help='TODO')

args = parser.parse_args()

cmdactions = defaultdict(lambda *args: parser.print_help())
cmdactions['run'] = lambda kwargs: do_run(**kwargs)
cmdactions['render'] = lambda kwargs: do_render(**kwargs)
cmdactions['test'] = lambda kwargs: do_test(**kwargs)

FIGURESDIR = 'figures'


def do_render(save, fig_selector):
    if fig_selector == "all":
        figures = FIGURES
    else:
        selected_fig_names = set(fig_selector.split(":"))
        figures = [f for f in FIGURES if f.title in selected_fig_names]

    if not figures:
        print("No selected figures!")
        print("\t- "+"\n\t- ".join([b.title for b in FIGURES]))

    for fig in figures:
        if save:
            from src.provenance import get_current_stamp
            import json
            fig.filename = os.path.join(FIGURESDIR, fig.title+'.png')
            with open(os.path.join(FIGURESDIR, f'{fig.title}.json'), 'w') as f:
                # TODO write more provenance data
                json.dump({'stamp': get_current_stamp()}, f)
        fig.render()


def do_run(bench_selector):
    if bench_selector == "all":
        benchmarks = BENCHMARKS
    else:
        selected_bench_names = set(bench_selector.split(":"))
        benchmarks = [b for b in BENCHMARKS if b.name in selected_bench_names]

    if not benchmarks:
        print("No selected benchmarks!")
        print("\t- "+"\n\t- ".join([b.name for b in BENCHMARKS]))

    n_error = 0
    print(benchmarks)

    loop = asyncio.get_event_loop()
    try:
        for bench in benchmarks:
            tmp_flag = bench.start()

            n_error += int(not tmp_flag)

        if n_error > 0:
            logging.error(
                f"{n_error} benchmark{'s' if n_error > 1 else ''} ha{'ve' if n_error > 1 else 's'} failed !")
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
