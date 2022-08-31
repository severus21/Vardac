from abc import abstractmethod
import logging
import os
import subprocess
import tempfile
from tempfile import TemporaryDirectory
import re

from src.runners import DockerRunner
from src.settings import *

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

class ChainCollector(AbstractCollector):
    def __init__(self, collectors) -> None:
        super().__init__()
        self.collectors = collectors 

    @abstractmethod
    def repatriate(tmpdir, runner):
        pass

    def collect(self, runner):
        with TemporaryDirectory() as tmpdir:
            # repatriate data from container/remote/... to host
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

class RemoteCollector(ChainCollector):
    def __init__(self, remote, collectors) -> None:
        super().__init__(collectors)
        self.remote = remote

        self.collect_stdout = ""
        self.collect_stderr = "" 

    def repatriate(self, tmpdir):
        res = subprocess.collect(
            "scp -r {self.remote} {tmpdir}/container", 
            capture_output=True, 
            encoding='utf-8', 
            shell=True)

        self.collect_stdout += res.stdout
        self.collect_stderr += res.stderr
        assert(res.returncode == 0) 

import docker
from execo import *

class VolumeCollector(ChainCollector):
    def __init__(self, collectors) -> None:
        super().__init__(collectors)

    def repatriate(self, tmpdir, runner : DockerRunner):
        if DEFAULT_DOCKER_REMOTE:
            client = docker.DockerClient(
                base_url=DEFAULT_DOCKER_REMOTE,
                tls= docker.tls.TLSConfig(
                    verify=True,
                    ca_cert = DOCKER_CERT_PATH/"ca.pem",
                    client_cert = (DOCKER_CERT_PATH/"cert.pem", DOCKER_CERT_PATH/"key.pem")
                ))
        else:
            client = docker.from_env()

        # Pre hook
        if DEFAULT_DOCKER_REMOTE:
            p = SshProcess(f"rm -fr {tmpdir} && mkdir -p {tmpdir} && ls /tmp", DEFAULT_HOST)
            p.start().wait() 
            assert(p.exit_code == 0)


            p = SshProcess(f"id", DEFAULT_HOST)
            p.start().wait()
            assert(p.exit_code == 0)
            
            uid = int(re.match(r'uid=(\d+)', p.stdout).group(1))
            print(uid)
            
        else:
            uid = os.getuid()
         
        # Blocking call
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
            user = uid 
        )

        # Post hook
        if DEFAULT_DOCKER_REMOTE:
            # Get file locally
            action.Get([DEFAULT_HOST], 
                tmpdir,
                tmpdir
            ).run().wait()

            # Clean remote
            p = SshProcess(f"rm -fr {tmpdir}", DEFAULT_HOST)
            p.start().wait()