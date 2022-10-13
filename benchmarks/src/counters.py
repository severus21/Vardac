from collections import defaultdict
from src.settings import * 
from src.utils import *
from checksumdir import dirhash
import subprocess
import json
from abc import ABC, abstractmethod
import glob

class Counter(ABC):
    def __init__(self, name, target) -> None:
       self.name = name 
       self.target = target

    def stamp(self):
        if os.path.isdir(self.target):
            hash = dirhash(self.target, 'md5')
        else:
            #TODO
            assert(False)
        
        stamp = {
            "path": str(self.target),
            "md5": hash
        }

        with open(COUNTSDIR/f"{normalize_path(self.name)}.stamp.json", "w") as f:
            json.dump(stamp, f)

    @abstractmethod
    def count(self):
        pass 

    @abstractmethod      
    def export(self):
        pass

    def run(self):
        self.count()
        self.stamp()
        self.export()


class CallbackCounter(Counter):
    SUFFIX = "cb"
    EXTS = [] 
    TOKENS = []
    "returns the number of user-defined callbacks"
    def __init__(self, name, target) -> None:
        super().__init__(name, target)
        self.counter = 0
        self.suffix = self.SUFFIX

    def count(self):
        for ext in self.EXTS:
            for filename in glob.glob(ext, root_dir=self.target, recursive=True):
                path = os.path.join(self.target, filename)
                with open(path, "r") as fp:
                    for line in fp:
                        for token in self.TOKENS:
                            if token in line: 
                                self.counter += 1
    
    def export(self):
        with open(COUNTSDIR/f"{normalize_path(self.name)+'-'+self.suffix}.json", "w") as f:
            json.dump(self.counter, f)

        with open(COUNTSDIR/f"{normalize_path('loc-'+self.name)}-{self.suffix}.tex", "w") as f:
            f.write(f"\\newcommand{{\\loc{LoCCounter.normalize_tex_var(self.name+'-'+self.suffix)}}}{{ {self.counter} }}")



class VardaCallbackCounter(CallbackCounter):
    EXTS = ["**/*.varch"]
    TOKENS = ["inport", "eport"]

class AkkaCallbackCounter(CallbackCounter):
    EXTS = ["**/*.java"]
    TOKENS = ["onMessage"]

class LoCCounter(Counter):
    """
    exports = [(cloc_name, export_suffix)]
    e.g (Varda impl, vimpl)
    """
    def __init__(self, name, target, exports) -> None:
        super().__init__(name, target)
        self.exports = exports

        self.data = None 

    def count(self):
        res = subprocess.run(
            f"cloc . --json --force-lang-def={CLOC_DEFINITIONS} --exclude-list-file={CLOC_EXCLUDES}", 
            capture_output=True, 
            encoding='utf-8',
            shell=True,
            cwd=self.target)

        if res.returncode != 0:
            print(res.stdout)
            print(res.stderr)
        assert(res.returncode == 0)

        self.data = json.loads(res.stdout)

    def normalize_tex_var(x):
        for c in ' -_[](),':
            x = x.replace(c, '')
        return x 


    def export(self):
        with open(COUNTSDIR/f"{normalize_path(self.name)}.json", "w") as f:
            json.dump(self.data, f)

        LoC = defaultdict(lambda: 0) 
        for cloc_name, suffix in self.exports:
            if cloc_name in self.data:
                LoC[suffix] += self.data[cloc_name]['code']

        for cloc_name, suffix in self.exports:
            with open(COUNTSDIR/f"{normalize_path('loc-'+self.name)}-{suffix}.tex", "w") as f:
                if cloc_name in self.data:
                    res =  f"${str(LoC[suffix])}$"
                else:
                    res = "$\emptyset$"

                f.write(f"\\newcommand{{\\loc{LoCCounter.normalize_tex_var(self.name+'-'+suffix)}}}{{ {res} }}")
