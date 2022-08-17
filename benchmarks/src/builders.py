import logging
import os
import subprocess
import os
import re

from abc import ABC, abstractmethod
from pathlib import Path
from unittest import result

from checksumdir import dirhash
from src.models import *

class AbstractBuilder(ABC):
    def __init__(self, name, project_dir=None) -> None:
        self.name= name
        self.project_dir = project_dir

        self.build_stdout           = ""
        self.build_stderr           = ""

        self._is_build = False

    @abstractmethod
    def _get_bench_model(self):
        pass

    def get_bench_model(self):
        bench, flag = self._get_bench_model()
        if flag:
            logging.debug(f"Bench {self.name}> Create bench model {bench.id} in DB !")
        else:
            logging.debug(f"Bench {self.name}> Load bench model {bench.id} from DB !\n\t(new results will be append to it)")

        return bench, flag

    @property
    def is_build(self):
        if not self._is_build:
            return self.aux_is_build()
        else:
            return self._is_build

    @abstractmethod
    def aux_is_build(self):
        pass

    @abstractmethod
    def _build(self) -> bool:
        pass

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

class ShellBuilder(AbstractBuilder):
    def __init__(self, name, project_dir=None, build_dir=None, build_cmd=None, build_cwd=None) -> None:
        super().__init__(name, project_dir)
        self.build_dir = build_dir

        self.build_cmd = build_cmd
        self.build_cwd = build_cwd

    def _get_bench_model(self):
        return Bench.objects.get_or_create(
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

    def aux_is_build(self):
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

class VardaBuilder(ShellBuilder):
    def __init__(self, name, project_dir, build_cmd, build_cwd, build_dir=Path(os.getcwd())/"compiler-build") -> None:
        super().__init__(name, project_dir, build_dir, build_cmd, build_cwd)

class AkkaBuilder(ShellBuilder):
    def __init__(self, name, project_dir, build_cmd, build_cwd, build_dir=Path(os.getcwd())/"compiler-build") -> None:
        super().__init__(name, project_dir, build_dir, build_cmd, build_cwd)

from tempfile import NamedTemporaryFile
import aiodocker
import asyncio
import base64

class DockerBuilder(AbstractBuilder):
    def __init__(self, name, project_dir=None) -> None:
        super().__init__(name, project_dir)
        self.docker = aiodocker.Docker()

    def image_name(self):
        return self.name+"_"+dirhash(self.project_dir, 'md5')[:10]

    def __del__(self):
        asyncio.run(self.docker.close())

    def aux_is_build(self):
        async def aux():
            try:
                await self.docker.images.inspect(
                    self.image_name
                )
                return True
            except aiodocker.DockerError as e:
                if e.status == 400: 
                    #image does not exists
                    return False
                else:
                    raise e

        # Image name encode project hash 
        loop = asyncio.get_event_loop()
        result = loop.run_until_complete(aux())
        return result 

    def _get_bench_model(self):
        return Bench.objects.get_or_create(
            name = self.name,
            host_spec = HostSpec.objects.get_or_create(name = HostSpec.get_hostname())[0],    
            soft_spec = SoftSpec.objects.get_or_create(
                os_version = SoftSpec.get_os_version(),
                docker_version = SoftSpec.get_docker_version(),
                varda_version = SoftSpec.get_varda_version(),
                )[0],    
            project_hash = dirhash(self.project_dir, 'md5'),
            build_hash = "TODO hash image",
            build_cmd = "docker (build_hash: docker image hash)" 
        )

    def _build(self):
        loop = asyncio.get_event_loop()
        result = loop.run_until_complete(self.__build())
        return result

    async def __build(self) -> bool:
        with NamedTemporaryFile() as fpgz:
            # Make tar.gz at fpgz
            bname = os.path.basename(self.project_dir)
            print(bname)
            print(fpgz.name)
            print(self.image_name())
            subprocess.check_call(
                f'find {bname} -printf "%P\n" -type f -o -type l -o -type d | tar -czf {fpgz.name} --no-recursion -C {bname} -T -', 
                encoding='utf-8',
                cwd=os.path.dirname(self.project_dir),
                shell=True)

            fpgz.seek(0)
            try:
                res = await self.docker.images.build(
                    fileobj = fpgz,
                    tag = f'{self.image_name()}:latest',
                    #nocache=True,
                    rm = True,
                    encoding="gzip"
                )
                buffer = [] 
                for item in res:
                    if 'stream' in item:
                        buffer.append(item['stream'])
                    if 'errorDetail' in item:
                        print('\n'.join(buffer))
                        print(item['error'])
                        return False
            except aiodocker.DockerError as e:
                raise e
        return True 