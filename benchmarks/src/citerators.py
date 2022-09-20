import numpy

class RangeIterator:
    # def = {
    #   pname_1: range
    # }
    #
    def __init__(self, defs):
        self.defs = defs

        self.keys = self.defs.keys()
        self.last = 0
        self.init = True
        self.closed = set()


        # arg1: iterators
        # arg2: n -> iterate over {n}
        for k in self.defs:
            if type(self.defs[k]) == int:
                self.defs[k] = range(self.defs[k], self.defs[k]+1).__iter__()

        self.snapshot   = [ r.__next__() for k, r in self.defs.items()]
        self.pos2key    = { i: k for i, k in enumerate(self.defs.keys())}

    def prepare(self, snapshot):
        return { list(self.defs.keys())[i]: v for i, v in enumerate(snapshot)}

    def __iter__(self):
        if self.defs:
            return self
        else: # one empty config
            return [{}].__iter__()

    def __next__(self):
        if len(self.closed) == len(self.defs):
            raise StopIteration
        if self.last in self.closed:
            return self.__next__()
        if self.init:
            self.init = False
            return self.prepare(self.snapshot)

        try:
            self.snapshot[self.last] = self.defs[self.pos2key[self.last]].__next__()
            return self.prepare(self.snapshot)
        except StopIteration:
            self.closed.add(self.last)
            self.last = (self.last + 1) % len(self.keys)
            return self.__next__()

def logrange(start, end, base):
    for x in numpy.logspace(start,end,base=10, num = end-start+1, dtype='int'):
        yield x
