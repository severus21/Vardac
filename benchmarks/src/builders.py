from distutils.command.clean import clean
from importlib.abc import ExecutionLoader
from inspect import trace
import logging
import os
import subprocess
import os
import shutil

from abc import ABC, abstractmethod
from pathlib import Path
from unittest import result

#from checksumdir import dirhash
from dirhash import dirhash
from src.models import *
from src.settings import * 

def mydirhash(path):
    return dirhash(path, 'md5', ignore=["akka/build/", "akka/bin/", "akka/gradle/", "akka/.gradle/", "akka/gradlew", "akka/gradlew.bat", "*.json"])

class AbstractBuilder(ABC):
    def __init__(self, project_dir=None) -> None:
        self.name = None # MUST be set by the benchmark holding it 
        self.project_dir = project_dir

        self._build_stdout           = ""
        self._build_stderr           = ""

        self._is_build = False

    def __enter__(self):
        assert(self.name != None)
        return self

    def __exit__(self, type, value, traceback):
        pass

    def for_runner(self):
        return self

    def build_stdout(self):
        return self._build_stdout

    @property
    def build_stderr(self):
        return self._build_stderr

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
        cl_name = type(self).__name__

        if not self.is_build:
            logging.info(f"Bench {self.name}> Building ({cl_name})!")
            tmp = self._build(*args, **kwargs)
            self._is_build = True
            b, _ = self.get_bench_model()
            if b:
                logging.info(f"Bench {self.name}> Built ({cl_name})!")
            return tmp, b
        else:
            logging.info(f"Bench {self.name}> Already built ({cl_name})!")
            return True, self.get_bench_model()[0]

class NoBuilder(AbstractBuilder):
    """Build nothing, the project have already been built externally"""
    def __init__(self, project_dir=None) -> None:
        super().__init__(project_dir)

    def _get_bench_model(self):
        return Bench.objects.get_or_create(
            name = self.name,
            host_spec = HostSpec.objects.get_or_create(name = HostSpec.get_hostname())[0],    
            soft_spec = SoftSpec.objects.get_or_create(
                os_version = SoftSpec.get_os_version(),
                docker_version = SoftSpec.get_docker_version(),
                varda_version = SoftSpec.get_varda_version(),
                )[0],    
        )

    def aux_is_build(self):
        return True 

    def _build(self) -> bool:
        return True 

from jinja2 import Environment, BaseLoader, select_autoescape

class ShellBuilder(AbstractBuilder):
    def __init__(self, project_dir=None, build_dir=None, build_cmd=None, build_cwd=None, clean_cmd=None, build_each_time=False) -> None:
        super().__init__(project_dir)
        self.build_dir = build_dir

        self._build_cmd = build_cmd
        self.build_cwd = build_cwd

        self._clean_cmd = clean_cmd
        self.build_each_time = build_each_time

    @property
    def clean_cmd(self):
        env = Environment(loader=BaseLoader())
        template = env.from_string(self._clean_cmd)
        return template.render(build_dir=self.build_dir, project_dir=self.project_dir)

    def __exit__(self, type, value, traceback):
        if self._clean_cmd:
            res = subprocess.run(
                self.clean_cmd,
                capture_output=True, 
                encoding='utf-8',
                cwd = self.build_cwd,
                shell=True)
            if res.returncode != 0:
                logging.error(res.stdout)
                logging.error(res.stderr)

    @property
    def build_cmd(self):
        env = Environment(loader=BaseLoader())
        template = env.from_string(self._build_cmd)
        return template.render(build_dir=self.build_dir, project_dir=self.project_dir)

    def _get_bench_model(self):
        build_hash = mydirhash(self.build_dir) if self.build_dir else "no-build-dir"
        project_hash = mydirhash(self.project_dir)

        flag, bench = Bench.objects.get_or_create(
            name = self.name,
            host_spec = HostSpec.objects.get_or_create(name = HostSpec.get_hostname())[0],    
            soft_spec = SoftSpec.objects.get_or_create(
                os_version = SoftSpec.get_os_version(),
                docker_version = SoftSpec.get_docker_version(),
                varda_version = SoftSpec.get_varda_version(),
                )[0],    
            project_hash = project_hash,
            build_hash = build_hash,
            build_cmd = self.build_cmd 
        )
        print(bench)
        return flag, bench

    def aux_is_build(self):
        if self.build_each_time:
            return False

        if self._is_build:
            return True

        if not self.build_dir or not self.project_dir:
            return False

        if not os.path.isdir(self.build_dir) or not os.path.isdir(self.project_dir):
            return False

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
        print("_build", self.build_cmd, res.returncode)
        if res.returncode != 0:
            logging.error(res.stdout)
            logging.error(res.stderr)

        self._build_stdout = res.stdout
        self._build_stderr = res.stderr
        return res.returncode == 0

class VardaBuilder(ShellBuilder):
    def __init__(self, project_dir, build_cmd, build_cwd, build_dir=Path(os.getcwd())/"compiler-build") -> None:
        super().__init__(project_dir, build_dir, build_cmd, build_cwd)

    def __exit__(self, type, value, traceback):
        shutil.rmtree(self.build_dir)
        return super().__exit__(type, value, traceback)

class AkkaBuilder(ShellBuilder):
    def __init__(self, project_dir, build_cmd, build_cwd, build_dir=Path(os.getcwd())/"compiler-build") -> None:
        super().__init__(project_dir, build_dir, build_cmd, build_cwd)

from tempfile import NamedTemporaryFile
import aiodocker
import asyncio
import base64

class ChainBuilder(AbstractBuilder):
    def __init__(self, builders, stamp_strategy, exposed_builder):
        super().__init__(None)

        self.builders = builders
        self.stamp_strategy = stamp_strategy
        self.exposed_builder = exposed_builder(self.builders)

    @property
    def run_stdout(self):
        return '\n'.join([self._run_stdout]+[r.run_stdout for r in self.builders])

    @property
    def run_stderr(self):
        return '\n'.join([self._run_stderr]+[r.run_stdout for r in self.builders])

    def for_runner(self):
        return self.exposed_builder

    def __enter__(self):
        for k in range(len(self.builders)):
            self.builders[k].name = self.name
            self.builders[k] = self.builders[k].__enter__()
        return super().__enter__()

    def __exit__(self, type, value, traceback):
        for builder in self.builders:
            builder.__exit__(type, value, traceback)
        return super().__exit__(type, value, traceback)

    def aux_is_build(self):
        for builder in self.builders:
            if not builder.is_build:
                return False
            print(type(builder), "is built")
        return True 

    def _get_bench_model(self):
        return self.stamp_strategy(self.builders)

    def _build(self):
        for builder in self.builders:
            if not builder.build():
                logging.error(f"Bench {self.name}> Intermediaire built failure {type(builder)}!")
                print(builder.build_stdout)
                print(builder.build_stderr)
                return False
        return True


class DockerComposeBuilder(AbstractBuilder):
    def __init__(self, project_dir=None, build_cwd=None) -> None:
        super().__init__(project_dir)
        self.build_cwd = build_cwd

    def __exit__(self, type, value, traceback):
        res = subprocess.run(
            "docker-compose down && docker-compose rm",
            capture_output=True, 
            encoding='utf-8',
            cwd = self.build_cwd,
            shell=True)
        if res.returncode != 0:
            logging.error(res.stdout)
            logging.error(res.stderr)

    @property
    def build_cmd(self):
        return "docker-compose up -d --build" 

    def _get_bench_model(self):
        return Bench.objects.get_or_create(
            name = self.name,
            host_spec = HostSpec.objects.get_or_create(name = HostSpec.get_hostname())[0],    
            soft_spec = SoftSpec.objects.get_or_create(
                os_version = SoftSpec.get_os_version(),
                docker_version = SoftSpec.get_docker_version(),
                varda_version = SoftSpec.get_varda_version(),
                )[0],    
            project_hash = mydirhash(self.project_dir),
        )

    def aux_is_build(self):
        return False 

    def _build(self) -> bool:
        res = subprocess.run(
            self.build_cmd, 
            capture_output=True, 
            encoding='utf-8',
            cwd = self.build_cwd,
            shell=True)
        print("_build", self.build_cmd, res.returncode)
        if res.returncode != 0:
            logging.error(res.stdout)
            logging.error(res.stderr)

        self._build_stdout = res.stdout
        self._build_stderr = res.stderr
        return res.returncode == 0

class DockerBuilder(AbstractBuilder):
    def __init__(self, project_dir=None, remote=DEFAULT_DOCKER_REMOTE) -> None:
        super().__init__(project_dir)
        self.docker = aiodocker.Docker(remote) if remote else aiodocker.Docker()

        self._image_name = None

    @property
    def image_name(self):
        if not self._image_name:
            self._image_name = self.name+"_"+mydirhash(self.project_dir)[:10]
        return self._image_name

    def __exit__(self, type, value, traceback):
        asyncio.get_event_loop().run_until_complete(asyncio.gather(self.docker.close()))

    def aux_is_build(self):
        print(self.project_dir)
        assert(os.path.isdir(self.project_dir))

        async def aux():
            try:
                await self.docker.images.inspect(
                    self.image_name
                )
                return True
            except aiodocker.DockerError as e:
                if e.status == 400 or e.status == 404: 
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
            project_hash = mydirhash(self.project_dir),
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
            print(self.image_name)
            subprocess.check_call(
                f'find {bname} -printf "%P\n" -type f -o -type l -o -type d | tar -czf {fpgz.name} --no-recursion -C {bname} -T -', 
                encoding='utf-8',
                cwd=os.path.dirname(self.project_dir),
                shell=True)

            fpgz.seek(0)
            try:
                res = await self.docker.images.build(
                    fileobj = fpgz,
                    tag = f'{self.image_name}:latest',
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