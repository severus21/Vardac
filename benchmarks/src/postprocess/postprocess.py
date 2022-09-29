from collections import defaultdict

import logging
import re
from sre_parse import State
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

    def __str__(self):
        return f'unit: {self.unit}, {self.items}'

    def __repr__(self) -> str:
        return str(self)

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
            # Eliminate NaN
            items = list(filter(lambda x: x == x,self.items))
            if len(items) > 1:
                self._stdev    = statistics.stdev(items)
                return self._stdev
            else: 
                return 0

    def concat(self, obj):
        assert(type(obj) == StatMetric)

        assert(obj.unit == self.unit)
        return StatMetric(self.unit, self.items+obj.items)
    
    def chain(objs):
        assert(len(objs) > 0)

        obj_res = StatMetric(objs[0].unit, items=[])
        for obj in objs:
            obj_res = obj_res.concat(obj)

        return obj_res

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

    def extract_metric(self, wanted_metric):
        nstats = {}
        for k in self.data.keys():
            if wanted_metric in self.data[k]:
                nstats[k] = self.data[k][wanted_metric]
            else:
                logging.warning(f"Metric {wanted_metric} not found in stats[{k}] !")
        return nstats

    def extract_metrics(self, wanted_metrics):
        nstats = {}

        set_wanted_metrics =  set(wanted_metrics)

        for k in self.data.keys():
            set_keys = set(self.data[k].keys())
            if set_wanted_metrics.issubset(set_keys):
                nstats[k] = {wanted_metric: self.data[k][wanted_metric] for wanted_metric in set_wanted_metrics}
            else:
                logging.warning(f"Metric {wanted_metrics} not found in stats[{k}] !")
        return nstats

    def map_on_param(self, data, wanted_param, selector, ylabeling):
        nstats = {}
        for k in data.keys():
            if selector(self.params[k]):
                if ylabeling:
                    kk = ylabeling(self,k)
                else:
                    kk = self.params[k][wanted_param]
                nstats[kk] = data[k]
        return nstats

    def extract(self, wanted_param, wanted_metric, selector, ylabeling):
        return self.map_on_param(self.extract_metric(wanted_metric), wanted_param, selector, ylabeling)

        #else:
        #    #'''where param is in fact a result category, and a bench parameter'''

        #    data = self.extract_metrics([wanted_param, wanted_metric])

        #    nstats = defaultdict(list)
        #    for k in data.keys():
        #        if selector(self.params[k]):
        #            print(self.data[k])
        #            x = (data[k][wanted_param])
        #            assert(x.min == x.max)
        #            nstats[x.min].append(data[k][wanted_metric])

        #    for k in nstats.keys():
        #        nstats[k] = StatMetric.chain(nstats[k])
        #        print(k, len(nstats[k].items))

        #    return nstats
