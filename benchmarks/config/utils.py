from src.ycsb_utils import *
import copy
import json
import re


def get_elapse_time(stdout):
    res = re.search('Time elapse (\d+) ms', stdout)
    return {"duration": {"unit": "ms", "value":
            res.group(1) if res else "N/A"},
            }


def get_rtts(filename):
    with open(filename) as fp:
        data = json.load(fp)
        return {"rtt": {"unit": "ms", "value": data['rtts']}}


def get_traces(prefix=""):
    def aux(filename):
        with open(filename) as fp:
            data = json.load(fp)
            if "durationsReception2OnMessage":
                return {
                    f"{prefix}durationsReception2OnMessage": {"unit": "ns", "value": [x["_1"] for x in data['durationsReception2OnMessage']]},
                    f"{prefix}durationsReception2Dispatcher": {"unit": "ns", "value": data['durationsReception2Dispatcher']},
                    f"{prefix}durationsPing": {"unit": "ns", "value": data['durationsPing']},#TODO rename durationsPing to durationsPhase1/Sending/Firing+initsession
                    f"{prefix}durationsPingAfterSessionInit": {"unit": "ns", "value": data['durationsPingAfterSessionInit']},#TODO rename durationsPing to durationsPhase1/Sending/Firing+initsession
                    f"{prefix}durationsPingAfterComputeSize": {"unit": "ns", "value": data['durationsPingAfterComputeSize']},#TODO rename durationsPing to durationsPhase1/Sending/Firing+initsession
                    f"{prefix}durationsReception2Callback": {"unit": "ns", "value": data['durationsReception2Callback']},
                    f"{prefix}durationsReception2EndCallback": {"unit": "ns", "value": data['durationsReception2EndCallback']},
                    
                    # Session statistics
                    f"{prefix}durationsFire": {"unit": "ns", "value": data['durationsFire']},
                    f"{prefix}durationsFireAfterChecks": {"unit": "ns", "value": data['durationsFireAfterChecks']},
                    f"{prefix}durationsFireAfterHydratation": {"unit": "ns", "value": data['durationsFireAfterHydratation']},
                    f"{prefix}durationsFireAfterTell": {"unit": "ns", "value": data['durationsFireAfterTell']},
                }
            else:
                # Tracing is disable or the tracing data are not exported
                return {
                    f"{prefix}durationsReception2OnMessage": {"unit": "ns", "value": []},
                    f"{prefix}durationsReception2Dispatcher": {"unit": "ns", "value": []},
                    f"{prefix}durationsReception2Callback": {"unit": "ns", "value": []},
                    f"{prefix}durationsReception2EndCallback": {"unit": "ns", "value": []},
                    f"{prefix}durationsPing": {"unit": "ns", "value": []},
                    f"{prefix}durationsPingAfterSessionInit": {"unit": "ns", "value": []},
                    f"{prefix}durationsPingAfterComputeSize": {"unit": "ns", "value": []},

                    # Session statistics
                    f"{prefix}durationsFire": {"unit": "ns", "value": []},
                    f"{prefix}durationsFireAfterChecks": {"unit": "ns", "value": []},
                    f"{prefix}durationsFireAfterHydratation": {"unit": "ns", "value": []},
                    f"{prefix}durationsFireAfterTell": {"unit": "ns", "value": []},
                }
    return aux


def get_ycsb_result(filename):
    with open(filename) as fp:
        data = extractYcsbParams(filename)
        return data


def get_pp_size(filename):
    '''ping+pong size'''
    with open(filename) as fp:
        data = json.load(fp)
        return {"pp_size": {"unit": "bytes", "value": data['ping_size']+data['pong_size']}}


def remove_dict(d, k_s):
    d = copy.copy(d)
    if type(k_s) == list:
        for k in k_s:
            del d[k]
    else:
        del d[k_s]
    return d
