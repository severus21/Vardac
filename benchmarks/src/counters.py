from src.settings import * 
from src.utils import *
from checksumdir import dirhash
import subprocess
import json
from abc import ABC, abstractmethod

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
            "path": self.target,
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
            f"cloc {self.target} --vcs=git --json --read-lang-def={CLOC_DEFINITIONS}", 
            capture_output=True, 
            encoding='utf-8',
            shell=True)


        if res.returncode != 0:
            print(res.stdout)
        assert(res.returncode == 0);

        self.data = json.loads(res.stdout)

    def normalize_tex_var(x):
        for c in ' -_[](),':
            x = x.replace(c, '')
        return x 


    def export(self):
        with open(COUNTSDIR/f"{normalize_path(self.name)}.json", "w") as f:
            json.dump(self.data, f)

        for cloc_name, suffix in self.exports:
            with open(COUNTSDIR/f"{normalize_path('loc-'+self.name)}-{suffix}.tex", "w") as f:
                if cloc_name in self.data:
                    res =  f"${str(self.data[cloc_name]['code'])}$"
                else:
                    res = "$\emptyset$"

                f.write(f"\\newcommand{{\\loc{LoCCounter.normalize_tex_var(self.name+'-'+suffix)}}}{{ {res} }}")
