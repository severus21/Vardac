#!/usr/bin/python3
# coding: utf-8

from cairo import STATUS_JBIG2_GLOBAL_MISSING
import django
import matplotlib.pyplot as plt
import numpy as np

from collections import defaultdict

import statistics


import os
os.environ.setdefault("DJANGO_SETTINGS_MODULE", "orm.settings")
django.setup()

from src.models import *


def gen_curve(data, destfile, **kwargs):
    x = np.array(list(data.keys()))
    y = np.array(list(map(lambda x: x["avg"], data.values())))
    ye = np.array(list(map(lambda x: x["stdev"]/2 if "stdev" in x else 0, data.values())))

    fig, ax = plt.subplots()
    ax.errorbar(x, y, yerr=ye, fmt='o')

    ax.set(**kwargs)
    ax.grid()

    #fig.savefig(destfile)
    plt.show()


def gen_curve2(data1, data2, destfile, **kwargs):
    xs1 = np.array(list(data1.keys()))
    ys1 = np.array(list(map(lambda x: x["avg"], data1.values())))
    ye1 = np.array(list(map(lambda x: x["stdev"]/2 if "stdev" in x else 0, data1.values())))
    xs2 = np.array(list(data2.keys()))
    ys2 = np.array(list(map(lambda x: x["avg"], data2.values())))
    ye2 = np.array(list(map(lambda x: x["stdev"]/2 if "stdev" in x else 0, data2.values())))

    fig, ax = plt.subplots()
    ax.errorbar(xs1, ys1, yerr=ye1, fmt='o', label="A")
    ax.errorbar(xs2, ys2, yerr=ye2, fmt='o', label="B")
    #ax.plot(xs2, ys2, 'b', label="A")
    #ax.plot(xs1, ys1, 'r', label="B")

    ax.set(**kwargs)

    # ax.grid()
    ax.legend()

    #fig.savefig(destfile)
    plt.show()


q1 = Bench.objects.filter(name="simpl-com-jvm-varda", id=41)
assert(len(q1) == 1)
results1 = q1[0].results.exclude(run_config__regex='"n": 100000')
#results1 = q1[0].results.all()
print(len(results1))

q2 = Bench.objects.filter(name="simpl-com-jvm-akka", id=30) 
assert(len(q2) == 1)
results2 = q2[0].results.all()
print(len(results2))


def hash_run_config(rcfg):
    return '__'.join(map(lambda x: f"{x[0]}_{x[1]}", rcfg.items()))



def stats_of_results(results):
    # group results per args
    grps = defaultdict(list)
    for r in results:
        grps[hash_run_config(r.run_config)].append(r)

    print(grps)

    # compute avgs per grps
    stats = defaultdict(dict)
    for k, items in grps.items():
        for item in items:
            for m_name, m_body in item.results.items():
                # remove N/A, TODO biais ?
                if not m_body["value"] == "N/A":
                    if m_name not in stats[k]:
                        stats[k][m_name]= {
                            "unit"      : m_body["unit"], 
                            "items"     : [float(m_body["value"])],
                        }
                    else: 
                        stats[k][m_name]["items"].append(float(m_body["value"]))

    print()
    print(stats)
    print()


    for k in stats.keys():
        for m_name in stats[k].keys():
            stats[k][m_name]["avg"] = statistics.mean(stats[k][m_name]["items"] )
            if len(stats[k][m_name]["items"]) > 1:
                stats[k][m_name]["stdev"] = statistics.stdev(stats[k][m_name]["items"] )

    return stats

def extract_metrics(stats, wanted_metric):
    nstats = {}
    for k in stats.keys():
        nstats[k] = stats[k][wanted_metric]
    return nstats

def map_on_param(results, stats, wanted_param):
    params = {hash_run_config(r.run_config): r.run_config for r in results}
    nstats = {}
    for k in stats.keys():
        nstats[params[k][wanted_param]] = stats[k]
    return nstats


stats1 = stats_of_results(results1)
stats2 = stats_of_results(results2)
print(stats1)
print(stats2)

#gen_curve(map_on_param(results1, extract_metrics(stats1, "duration"), "n"), "toto.png")
#gen_curve(map_on_param(results2, extract_metrics(stats2, "duration"), "n"), "toto.png")

gen_curve2(
    map_on_param(results1, extract_metrics(stats1, "duration"), "n"),
    map_on_param(results2, extract_metrics(stats2, "duration"), "n"),
    "toto.png")