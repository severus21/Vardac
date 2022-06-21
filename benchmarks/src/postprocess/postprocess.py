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

    # Center
    @property
    def mean(self):
        if hasattr(self, "_mean"):
            return self._mean
        else:
            self._mean    = statistics.mean(self.items) 
            return self._mean

    @property
    def median(self):
        if hasattr(self, "_median"):
            return self._median
        else:
            self._median    = statistics.median(self.items) 
            return self._median
    
    # Dispersion 

    @property
    def min(self):
        if hasattr(self, "_min"):
            return self._min
        else:
            self._min    = min(self.items) 
            return self._min

    @property
    def max(self):
        if hasattr(self, "_max"):
            return self._max
        else:
            self._max    = max(self.items) 
            return self._max

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

class Stats:
    def __init__(self, results=[]):
        #{ config : {
        #    metric: StatMetric 
        #}
        self.data = defaultdict(dict)
        self.results = []
        self.params = {}

        self.of_results(results)

    def hash_run_config(self, rcfg):
        return '__'.join(map(lambda x: f"{x[0]}_{x[1]}", rcfg.items()))

    def of_results(self, results):
        self.results.extend(results)

        # group results per config 
        grps = defaultdict(list)
        for r in results:
            grps[self.hash_run_config(r.run_config)].append(r)

        # per group, group per measure
        for k, items in grps.items():
            for item in items:
                for m_name, m_body in item.results.items():
                    if m_body["value"] == "N/A":
                        # remove N/A, TODO biais ?
                        continue

                    if m_name not in self.data[k]:
                        self.data[k][m_name]= StatMetric(m_body["unit"], m_body["value"])
                    else: 
                        self.data[k][m_name].add_item_s(m_body["value"])

        for r in results:
            self.params[self.hash_run_config(r.run_config)] = r.run_config

    def extract_metrics(self, wanted_metric):
        nstats = {}
        for k in self.data.keys():
            if wanted_metric in self.data[k]:
                nstats[k] = self.data[k][wanted_metric]
            else:
                logging.warning(f"Metric {wanted_metric} not found in stats[{k}] !")
        return nstats

    def map_on_param(self, data, wanted_param):
        nstats = {}
        for k in data.keys():
            nstats[self.params[k][wanted_param]] = data[k]
        return nstats

    def extract(self, wanted_param, wanted_metric):
        return self.map_on_param(self.extract_metrics(wanted_metric), wanted_param)
