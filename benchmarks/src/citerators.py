import numpy
from copy import copy

class RangeIterator:
    # def = {
    #   pname_1: range
    # }
    #
    def __init__(self, defs):
        self.defs = defs
        self.closed = set()

        for k in self.defs:
            if type(self.defs[k]) == int:
                self.defs[k] = list(range(self.defs[k], self.defs[k]+1))
            else:
                self.defs[k] = list(self.defs[k])

        self.snapshots   = [ [ 0 for _ in self.defs.keys()] ]
        self.pos2key    = { i: k for i, k in enumerate(self.defs.keys())}

    def prepare(self, snapshot):
        return { list(self.defs.keys())[i]: self.defs[list(self.defs.keys())[i]][v] for i, v in enumerate(snapshot)}

    def __iter__(self):
        if self.defs:
            return self
        else: # one empty config
            return [{}].__iter__()

    def next(self, snapshot, i):
        if snapshot[i] < len(self.defs[self.pos2key[i]])-1:
            snapshot[i] = snapshot[i]+1
            return snapshot
        else:
            return None

    def __next__(self):
        if not self.snapshots:
            raise StopIteration

        snapshot = self.snapshots.pop(0)
        while tuple(snapshot) in self.closed and self.snapshots:
            snapshot = self.snapshots.pop(0)
        self.closed.add(tuple(snapshot))

        if not snapshot:
            raise StopIteration

        new_snapshots = [
            self.next(copy(snapshot), i) for i in range(len(snapshot))
        ]
        new_snapshots = filter(lambda x: x != None, new_snapshots)
        self.snapshots.extend(new_snapshots)
        return self.prepare(snapshot)

def logrange(start, end, base):
    for x in numpy.logspace(start,end,base=10, num = end-start+1, dtype='int'):
        yield x
