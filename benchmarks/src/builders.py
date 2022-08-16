import logging
import os
import subprocess
import os
import re

from pathlib import Path

from checksumdir import dirhash
from src.models import *

class AbstractBuilder:
    def __init__(self, name, project_dir=None, build_dir=None, build_cmd=None, build_cwd=None) -> None:
        self.name= name
        self.project_dir = project_dir
        self.build_dir = build_dir

        self.build_cmd = build_cmd
        self.build_cwd = build_cwd

        self.build_stdout           = ""
        self.build_stderr           = ""

        self._is_build = False


    def get_bench_model(self):
        bench, flag = Bench.objects.get_or_create(
            name = self.name,
            host_spec = HostSpec.objects.get_or_create(name = HostSpec.get_hostname())[0],    
            soft_spec = SoftSpec.objects.get_or_create(
                os_version = SoftSpec.get_os_version(),
                docker_version = SoftSpec.get_docker_version(),
                varda_version = SoftSpec.get_varda_version(),
                )[0],    
            project_hash = dirhash(self.project_dir, 'md5'),
            build_hash = dirhash(self.build_dir, 'md5'),
            build_cmd = self.build_cmd 
        )

        if flag:
            logging.debug(f"Bench {self.name}> Create bench model {bench.id} in DB !")
        else:
            logging.debug(f"Bench {self.name}> Load bench model {bench.id} from DB !\n\t(new results will be append to it)")

        return bench, flag

    @property
    def is_build(self):
        if not self._is_build and self.build_dir and self.project_dir:
            _, is_created = self.get_bench_model()
            if not is_created:
                self._is_build = True
        return self._is_build

    def _build(self) -> bool:
        res = subprocess.run(
            self.build_cmd, 
            capture_output=True, 
            encoding='utf-8',
            cwd = self.build_cwd,
            shell=True)

        self.build_stdout = res.stdout
        self.build_stderr = res.stderr
        return res.returncode == 0


    def build(self, *args, **kwargs):
        if not self.is_build:
            logging.info(f"Bench {self.name}> Building !")
            tmp = self._build(*args, **kwargs)
            self._is_build = True
            b, _ = self.get_bench_model()
            if b:
                logging.info(f"Bench {self.name}> Built !")
            return tmp, b
        else:
            logging.info(f"Bench {self.name}> Already built !")
            return True, self.get_bench_model()[0]

class VardaBuilder(AbstractBuilder):
    def __init__(self, name, project_dir, build_cmd, build_cwd, build_dir=Path(os.getcwd())/"compiler-build") -> None:
        super().__init__(name, project_dir, build_dir, build_cmd, build_cwd)

class AkkaBuilder(AbstractBuilder):
    def __init__(self, name, project_dir, build_cmd, build_cwd, build_dir=Path(os.getcwd())/"compiler-build") -> None:
        super().__init__(name, project_dir, build_dir, build_cmd, build_cwd)