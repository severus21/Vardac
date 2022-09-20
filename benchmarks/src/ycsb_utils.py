#
# Custom script to extract results and plot graphs
# python extract-ycsb-results.py <output-dir>
# 	Qualifiers: $readtype, $workload, $repetition, $thread, $machine
#

import sys
import os
import numpy as np
from pathlib import Path

from src.figures import Figure, Curve


#YCSB_PATH = f"{YCSB_DIR}/xps-2019.$workload.$repetition/$thread/ycsb-results"
YCSB_PARAMS = [
    {"id": "[OVERALL], Throughput(ops/sec), ", "title": "Throughput", "unit": 'ops/sec'},
    # { "id" : "[UPDATE], Operations, ", "title" : "Update Operations" },
    {"id": "[UPDATE], AverageLatency(us), ",
     "title": "Avg Update Latency", "unit": 'us'},
    {"id": "[UPDATE], 95thPercentileLatency(us), ",
     "title": "95th%ile Update Latency", "unit":"us"},
    {"id": "[UPDATE], 99thPercentileLatency(us), ",
     "title": "99th%ile Update Latency", "unit": "us"},
    # { "id" : "[UPDATE-FAILED], Operations, ", "title" : "Failed Update Operations" },
    # { "id" : "[READ], Operations, ", "title" : "Read Operations" },
    {"id": "[READ], AverageLatency(us), ", "title": "Avg Read Latency", "unit":"us"},
    {"id": "[READ], 95thPercentileLatency(us), ",
     "title": "95th%ile Read Latency", "unit":"us"},
    {"id": "[READ], 99thPercentileLatency(us), ",
     "title": "99th%ile Read Latency", "unit":"us"},
    # { "id" : "[READ-FAILED], Operations, ", "title" : "Failed Read Operations" },
]


def extractYcsbParam(file, param_id):
    for line in open(file):
        if line.startswith(param_id):
            result = line[len(param_id):].rstrip()
            return float(result)
    return 0


def extractYcsbParams(file):
    try:
        results = {}
        for param in YCSB_PARAMS:
            param_id = param["id"]
            results[param['title']] = {"unit":param['unit'], "value":extractYcsbParam(file, param_id)}
        return results
    except Exception as e:
        print("Error in parsing " + file +"\n"+str(e))
        return None


def my(path_exp, extract_func):
    all_paths = set()
    for workload in WORKLOADS:
        for thread in NUM_THREADS:
            # for lhfallback in LHFALLBACK_PROBS:
            path = path_exp.replace("$workload", workload)
            path = path.replace("$thread", str(thread))
            all_paths.add(path)
    results = dict()
    for path in all_paths:
        toMean = []
        for repetition in range(REPETITIONS):
            file = path.replace("$repetition", str(repetition))
            result = extract_func(file)
            if result is not None:
                toMean.append(result)
        results[path] = np.mean(toMean, axis=0).tolist()
    return results


def dump_raw_results(results, params, filename, delimiter=':\t'):
    with open(filename, 'w') as f:
        for file, result in results.items():
            f.write(file + '\n')
            for i, param in enumerate(params):
                f.write(param["title"] + delimiter + str(result[i]) + '\n')
            f.write('\n')


def dump_ycsb_graphs(results):
    path_exp = YCSB_PATH

    for workload in WORKLOADS:
        path = path_exp.replace("$workload", workload)
        xlabel = "Threads"
        for i, param in enumerate(YCSB_PARAMS):
            ys = []
            xs = NUM_THREADS
            for thread in NUM_THREADS:
                curr = path.replace("$thread", str(thread))
                ys.append(results[curr][i])
            filename = OUTPUT_DIR + "/" + param["title"]

            fig = Figure(
                f"{workload} on xps-2019",
                xlabel,
                param["title"],
                [
                    Curve(
                        "toto",
                        (xs, ys),
                        "mean",
                        None,
                    )
                ],
                #filename = filename
            )
            fig.render()

# if __name__ == '__main__':
#	os.mkdir(OUTPUT_DIR)
#
#	ycsb_results = my(YCSB_PATH, extractYcsbParams)
#	print(ycsb_results)
    #dump_raw_results(ycsb_results, YCSB_PARAMS, OUTPUT_DIR + "/" + "raw-ycsb-results.txt")
    # dump_ycsb_graphs(ycsb_results)
