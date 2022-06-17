from collections import defaultdict

import logging
import re
import statistics

class Measure:
    # value_s can be of type T or type list[T]
    def __init__(self, unit, value_s):
        self.unit   = unit
        self.value_s = value_s

class StatMetric:
    pattern = re.compile('^_[a-zA-Z].*$')
    def __init__(self, unit : str, items : list, f = float) -> None:
        self.unit   = unit
        self.items  = list(map(f, items)) if type(items) == list else [f(items)]

        self.f      = f 

    def add_item_s(self, item_s):
        if type(item_s) == list:
            self.items.extend(list(map(self.f, item_s)))
        else:
            self.items.append(self.f(item_s))

        for a in dir(self):
            if StatMetric.pattern.search(a):
                delattr(self, a)


    @property
    def avg(self):
        if hasattr(self, "_avg"):
            return self._avg
        else:
            self._avg    = statistics.mean(self.items) 
            return self._avg
    
    @property
    def stdev(self):
        if hasattr(self, "_stdev"):
            return self._stdev
        else:
            if len(self.items) > 1:
                self._stdev    = statistics.stdev(self.items)
                return self._stdev
            else: 
                return None
#stats
#{ config : {
#    metric: StatMetric 
#}

def hash_run_config(rcfg):
    return '__'.join(map(lambda x: f"{x[0]}_{x[1]}", rcfg.items()))

def stats_of_results(results):
    # group results per config 
    grps = defaultdict(list)
    for r in results:
        grps[hash_run_config(r.run_config)].append(r)

    # per group, group per measure
    stats = defaultdict(dict)
    for k, items in grps.items():
        for item in items:
            for m_name, m_body in item.results.items():
                if m_body["value"] == "N/A":
                    # remove N/A, TODO biais ?
                    continue

                if m_name not in stats[k]:
                    stats[k][m_name]= StatMetric(m_body["unit"], m_body["value"])
                else: 
                    stats[k][m_name].add_item_s(m_body["value"])

    return stats

def extract_metrics(stats, wanted_metric):
    nstats = {}
    for k in stats.keys():
        if wanted_metric in stats[k]:
            nstats[k] = stats[k][wanted_metric]
        else:
            logging.warning(f"Metric {wanted_metric} not found in stats[{k}] !")
    return nstats

def map_on_param(results, stats, wanted_param):
    params = {hash_run_config(r.run_config): r.run_config for r in results}
    nstats = {}
    for k in stats.keys():
        nstats[params[k][wanted_param]] = stats[k]
    return nstats