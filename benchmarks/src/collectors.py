import logging
import os
import subprocess
from tkinter import W

from src.runners import DockerRunner

from .models import *

class AbstractCollector:
    def __init__(self) -> None:
        pass

    def collect(self, runner):
        pass

    def clean(self):
        """Clean possible remaining data from already collected runs"""
        pass

class ShellCollector(AbstractCollector):
    def __init__(self, collect_cmd, collect_cwd=None) -> None:
        super().__init__()
        self.collect_cmd    = collect_cmd
        self.collect_cwd    = collect_cwd
        
        self.collect_stdout           = ""
        self.collect_stderr           = ""
    
    def collect(self, runner):
        res = subprocess.collect(
            self.collect_cmd, 
            capture_output=True, 
            encoding='utf-8', 
            cwd=self.collect_cwd, 
            shell=True)

        self.collect_stdout = res.stdout
        self.collect_stderr = res.stderr
        assert(res.returncode == 0) 
        return True

class StdoutCollector(AbstractCollector):
    def __init__(self, stdlambda) -> None:
        super().__init__()
        self.stdcollect = stdlambda

    def collect(self, runner):
       return self.stdcollect(runner.run_stdout) 

class FileCollector(AbstractCollector):
    def __init__(self, filename, flambda) -> None:
        super().__init__()
        self.filename = filename
        self.flambda = flambda

    def collect(self, runner):
        if os.path.exists(self.filename):
            return self.flambda(self.filename)
        else:
            logging.warning(f"filename {self.filename} does not exists !!")
            return []

    def clean(self):
        if os.path.exists(self.filename):
            os.remove(self.filename)

import docker
from tempfile import TemporaryDirectory

class VolumeCollector(AbstractCollector):
    def __init__(self, collectors) -> None:
        super().__init__()
        self.collectors = collectors

    def repatriate(self, tmpdir, runner : DockerRunner):
        client = docker.from_env()
        r = client.containers.run(
            'ubuntu:latest',
            ['cp', '-av', '/data/container/', '/data/host/'],
            mounts= [
                docker.types.Mount(
                    target = "/data/container",
                    source = runner.currentdatavolume['Name'],
                    type = "volume",
                    read_only = True
                ),
                docker.types.Mount(
                    target = "/data/host",
                    source = tmpdir,
                    type = "bind",
                ),
            ],
            user = os.getuid()
        )

        #print("Result:", r)

    def collect(self, runner : DockerRunner):
        with TemporaryDirectory() as tmpdir:
            # repatriate data from container to host
            self.repatriate(tmpdir, runner)

            # apply collectors on repatriated data
            res = {} 
            for collector in self.collectors:
                past = collector.filename

                collector.filename = os.path.join(tmpdir, os.path.join('container', collector.filename))
                tmp = collector.collect(runner)
                if tmp:
                    res = res | tmp

                collector.filename = past
            return res