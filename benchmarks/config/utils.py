import json
import re

def get_elapse_time(stdout):
    res = re.search('Time elapse (\d+) ms', stdout)
    return {"duration": {"unit":"ms", "value":
            res.group(1) if res else "N/A"},
    }

def get_rtts(filename):
    with open(filename) as fp:
        return {"rtt": {"unit": "ms", "value":json.load(fp)}}
