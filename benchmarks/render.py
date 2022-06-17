#!/usr/bin/python3
# coding: utf-8

from cairo import STATUS_JBIG2_GLOBAL_MISSING
import django



import os
os.environ.setdefault("DJANGO_SETTINGS_MODULE", "orm.settings")
django.setup()

from src.models import *
from src.postprocess.curves import *
from src.postprocess.postprocess import *




q1 = Bench.objects.filter(name="simpl-com-jvm-varda", id=77)
assert(len(q1) == 1)
#results1 = q1[0].results.exclude(run_config__regex='"n": 100000')
results1 = q1[0].results.all()

q2 = Bench.objects.filter(name="simpl-com-jvm-akka", id=69) 
assert(len(q2) == 1)
results2 = q2[0].results.all()


stats1 = stats_of_results(results1)
stats2 = stats_of_results(results2)

#gen_curve(map_on_param(results1, extract_metrics(stats1, "duration"), "n"), "toto.png")
#gen_curve(map_on_param(results2, extract_metrics(stats2, "duration"), "n"), "toto.png")
#gen_curve(map_on_param(results2, extract_metrics(stats2, "rtt"), "n"), "toto.png")

gen_curve2(
    map_on_param(results1, extract_metrics(stats1, "duration"), "n"),
    map_on_param(results2, extract_metrics(stats2, "duration"), "n"),
    "toto.png")

gen_curve2(
    map_on_param(results1, extract_metrics(stats1, "rtt"), "n"),
    map_on_param(results2, extract_metrics(stats2, "rtt"), "n"),
    "toto.png")