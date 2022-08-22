import copy
import json
import re

def get_elapse_time(stdout):
    res = re.search('Time elapse (\d+) ms', stdout)
    return {"duration": {"unit":"ms", "value":
            res.group(1) if res else "N/A"},
    }

def get_rtts(filename):
    with open(filename) as fp:
        data = json.load(fp)
        return {"rtt": {"unit": "ms", "value":data['rtts']}}

def get_pp_size(filename):
    '''ping+pong size'''
    with open(filename) as fp:
        data = json.load(fp)
        return {"pp_size": {"unit": "bytes", "value":data['ping_size']+data['pong_size']}}

def remove_dict(d, k_s):
    d = copy.copy(d)
    if type(k_s) == list:
        for k in k_s:
            del d[k]
    else:
        del d[k_s]
    return d