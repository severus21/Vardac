#!/usr/bin/python3
# coding: utf-8

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
    ys1 = np.array(list(data1.values()))
    xs2 = np.array(list(data2.keys()))
    ys2 = np.array(list(data2.values()))

    fig, ax = plt.subplots()
    ax.plot(xs2, ys2, 'b', label="H")
    ax.plot(xs1, ys1, 'r', label="F")

    ax.set(**kwargs)

    # ax.grid()
    ax.legend()

    fig.savefig(destfile)
    # plt.show()


q1 = Bench.objects.filter(name="simpl-com")
assert(len(q1) == 1)
results = q1[0].results.all()


def hash_run_config(rcfg):
    return '__'.join(map(lambda x: f"{x[0]}_{x[1]}", rcfg.items()))


params = {hash_run_config(r.run_config): r.run_config for r in results}

# group results per args
grps = defaultdict(list)
for r in results:
    grps[hash_run_config(r.run_config)].append(r)

print(grps)

# compute avgs per grps
stats = defaultdict(dict)
for k, items in grps.items():
    for m_name, m_body in items[0].results.items():
        if m_name not in stats[k]:
            stats[k][m_name]= {
                "unit"      : m_body["unit"], 
                "items"     : [float(m_body["value"])],
            }
        else: 
            stats[k][m_name]["items"]    += m_body["value"]

for k in stats.keys():
    for m_name in stats[k].keys():
        stats[k][m_name]["avg"] = statistics.mean(stats[k][m_name]["items"] )
        if len(stats[k][m_name]["items"]) > 1:
            stats[k][m_name]["stdev"] = statistics.stdev(stats[k][m_name]["items"] )

def extract_metrics(stats, wanted_metric):
    nstats = {}
    for k in stats.keys():
        nstats[k] = stats[k][wanted_metric]
    return nstats

def map_on_param(stats, wanted_param):
    nstats = {}
    for k in stats.keys():
        nstats[params[k][wanted_param]] = stats[k]
    return nstats

print(stats)

gen_curve(map_on_param(extract_metrics(stats, "duration"), "n"), "toto.png")
