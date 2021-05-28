import unittest

import subprocess, shlex
import os

prog = os.path.join(os.path.dirname(os.path.realpath(__file__)), 'joujou')

def decorator(cls):
    for (name, cmd, checkres, descr) in cls.tests:
        def wrapper(self):
            p = subprocess.run(cmd, shell=True,  stdout=subprocess.PIPE,  stderr=subprocess.PIPE)
            self.assertTrue(p.returncode == 0 and checkres(p), msg="%s : FAILURE\n%s\n%s" % (name, descr, p.stderr))
        setattr(cls, "test_" + name, wrapper)
    return cls

@decorator
class TestStringMethods(unittest.TestCase):
    #name,file to compile, expected output,description 
    tests = [
            ('test1', '%s --places place.yml tests/functtests/data/test1.spec && cd /tmp/compiler-build && make' % prog, lambda x : True, ''),
            ('component-args', '%s --places place.yml tests/functtests/data/test2.spec && cd /tmp/compiler-build && make' % prog, lambda x : True, '')
            ] 

if __name__ == '__main__':
    unittest.main()
