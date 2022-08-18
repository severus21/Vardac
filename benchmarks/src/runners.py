import shlex
import subprocess
from threading import TIMEOUT_MAX
from abc import ABC, abstractmethod
import time
import select
import os
import signal
import asyncio
import logging
import uuid
import aiodocker

from .models import *
from .settings import *
from .pods import *

"""fix yelling at me error"""
from functools import wraps

from asyncio.base_subprocess import BaseSubprocessTransport


def silence_event_loop_closed(func):
    @wraps(func)
    def wrapper(self, *args, **kwargs):
        try:
            return func(self, *args, **kwargs)
        except RuntimeError as e:
            if str(e) != 'Event loop is closed':
                raise
    return wrapper


BaseSubprocessTransport.__del__ = silence_event_loop_closed(
    BaseSubprocessTransport.__del__)
"""fix yelling at me error end"""


async def display_time(stop_display_time, start_time, last_elapse=0):
    if stop_display_time.is_set():
        return True
    else:
        if int(time.time() - start_time) > last_elapse:
            last_elapse = int(time.time() - start_time)
            print(f"Ran for {last_elapse} s [Timeout: {RUN_TIMEOUT}s]")
        await asyncio.sleep(0.1)  # at least
        await display_time(stop_display_time, start_time, last_elapse)

class Runner(ABC):
    def __init__(self, name) -> None:
        self.name = name

        self._run_stdout = ""
        self._run_stderr = ""

    def __enter__(self):
        logging.debug(f"Entering runner {self.name}")
        return self

    def __exit__(self, type, value, traceback):
        logging.debug(f"Exiting runner {self.name}")

    @property
    def run_stdout(self):
        return self._run_stdout

    @property
    def run_stderr(self):
        return self._run_stderr

    @abstractmethod
    async def run_async(self, stop_event, stop_display_time):
        pass

    @abstractmethod
    def has_failed(self, buffer):
        pass

    @abstractmethod
    def is_terminated(self, buffer):
        pass

    async def run_async_with_displaytime(self):
        stop_display_time = asyncio.Event()
        return await asyncio.gather(
            self.run_async(None, stop_display_time),
            display_time(stop_display_time, time.time()),
            return_exceptions=True)

    def run(self):
        loop = asyncio.get_event_loop()
        result = loop.run_until_complete(self.run_async_with_displaytime())

        # For both
        if result:
            result = result[0]
        return result

class OrderedMultiShellRunner(Runner):
    def __init__(self, name, runners, config):
        super().__init__(name)
        self.runners = runners
        self.config = config


    @property
    def run_stdout(self):
        return '\n'.join([self._run_stdout]+[r.run_stdout for r in self.runners])

    @property
    def run_stderr(self):
        return '\n'.join([self._run_stderr]+[r.run_stdout for r in self.runners])

    def is_terminated(self, buffer):
        return not False in [runner.is_terminated(buffer) for runner in self.runners]

    def has_failed(self, buffer):
        return not False in [runner.has_failed(buffer) for runner in self.runners]

    def terminate(self, results):
        for res in results:
            os.killpg(os.getpgid(res.pid), signal.SIGTERM)
        for res in results:
            res.wait()
        time.sleep(1)

    async def _run_async(self, stop_event, stop_display_time):
        return await asyncio.gather(
            *[runner.run_async(stop_event, stop_display_time)
              for runner in self.runners],
            display_time(stop_display_time, time.time()),
            return_exceptions=True
        )

    def run_async(self):
        stop_event = asyncio.Event()
        stop_display_time = asyncio.Event()

        results = asyncio.run(self._run_async(stop_event, stop_display_time))
        if results:
            results = False not in results[:-1]

        return results

class BaseRunner(Runner):
    def __init__(self, name, stdout_termination_token, error_token, config, set_stop_event) -> None:
        super().__init__(name)
        self.stdout_termination_token = stdout_termination_token
        self.error_token = error_token
        self.config = config
        self.set_stop_event = set_stop_event

    def is_terminated(self, buffer):
        return self.stdout_termination_token and self.stdout_termination_token in buffer

    async def terminate(self, pod, stop_event, stop_display_time):
        print(f"Try terminate {self.name}")
        stop_display_time.set()
        if stop_event != None and self.set_stop_event:
            stop_event.set()
        await pod.terminate()
        print(f"End terminate {self.name}")

    def has_failed(self, buffer):
        return self.error_token and self.error_token in buffer

    async def run_async_skeleton(self, pod, stop_event, stop_display_time):
        self._run_stdout = ""
        self._run_stderr = ""
        print("run_async_skeleton")

        while True:
            # Parent event
            if stop_event != None and stop_event.is_set():
                logging.debug(f"{self.name}> Cancelled from parent!")
                await self.terminate(pod, stop_event, stop_display_time)

                # stdout_termination_token == None => this task never terminates by itself
                return self.stdout_termination_token == None or self.is_terminated(self._run_stdout)

            try:
                done, pending = await asyncio.wait(
                    [    
                        pod.nextstdout(),
                        pod.is_terminated(),
                    ], 
                    timeout = RUN_TIMEOUT, return_when=asyncio.FIRST_COMPLETED)

                statuscode = None
                lines = []
                for task in done:
                    res = await task
                    if 'code' in res:
                        statuscode = res['code'] 
                    elif 'stdout' in res:
                        lines = res['stdout'] 
                    else: 
                        raise Exception("TODO")

                for task in pending:
                    task.cancel()

                ## Processing statuscode
                if statuscode != None:
                    if statuscode == 0:
                        logging.debug(f"{self.name}> Is terminated")
                        await self.terminate(pod, stop_event, stop_display_time)
                        return True
                    else:
                        logging.error(f"{self.name}> Has crash")
                        self._run_stderr = await pod.stderr()
                        print(self._run_stdout)
                        print(self.run_stderr)
                        return False

                ## Processing stdout
                self._run_stdout += ''.join(lines)

                for line in lines:
                    if not line:  # EOF
                        logging.debug(f"{self.name}> EOF")
                        await self.terminate(pod, stop_event, stop_display_time)
                        return True
                    elif self.is_terminated(line):
                        logging.debug(f"{self.name}> Is terminated")
                        await self.terminate(pod, stop_event, stop_display_time)
                        return True
                    elif self.has_failed(line):
                        logging.error(f"{self.name}> Has failed")
                        self._run_stderr = await pod.stderr()
                        print(self._run_stdout)
                        print(self.run_stderr)
                        await self.terminate(pod, stop_event, stop_display_time)
                        return False

            except asyncio.TimeoutError:
                logging.error(f"{self.name}> Timeout !")
                print(self._run_stdout)
                await self.terminate(pod, stop_event, stop_display_time)
                return False
            except asyncio.CancelledError:
                logging.error(f"{self.name}> Cancelled !")
                await self.terminate(pod, stop_event, stop_display_time)
                return False

class ShellRunner(BaseRunner):
    def __init__(self, name, run_cmd, run_cwd, stdout_termination_token, error_token, config, set_stop_event) -> None:
        super().__init__(name, stdout_termination_token, error_token, config, set_stop_event)
        self.run_cmd = run_cmd
        self.run_cwd = run_cwd

        self.config = config
        self.set_stop_event = set_stop_event

    def render(self, snapshot):
        return " ".join([f"-{k} {shlex.quote(str(v))}" for k, v in snapshot.items()])

    async def run_async(self, stop_event, stop_display_time):
        # Start child process
        process = await asyncio.create_subprocess_shell(
            self.run_cmd+" "+self.render(self.config),
            stdout=asyncio.subprocess.PIPE,
            stderr=asyncio.subprocess.PIPE,
            cwd=self.run_cwd,
            shell=True,
            preexec_fn=os.setpgrp
        )

        return await self.run_async_skeleton(ProcessPod(process), stop_event, stop_display_time)


class DockerRunner(BaseRunner):
    #TODO create img with correct stamp
    def __init__(self, name, image, run_cmd, stdout_termination_token, error_token, config, set_stop_event) -> None:
        super().__init__(name, stdout_termination_token, error_token, config, set_stop_event)
        self.image = image
        self.run_cmd = run_cmd

        self.docker = aiodocker.Docker()
        self.volumes = []
        self.containers = []

    @property
    def currentdatavolume(self):
        return self.volumes[-1]#['Mountpoint']/'Name

    def __exit__(self, type, value, traceback):
        super().__exit__(type, value, traceback)
        async def aux():
            for container in self.containers:
                try:
                    await container.kill()
                    await container.delete()
                except aiodocker.DockerError as e:
                    if e.status in [404, 409]:
                        # container no longer exists
                        pass 
                    else:
                        raise e

            for volume in self.volumes:
                try:
                    pass
                    #await aiodocker.docker.DockerVolume(self.docker, volume['Name']).delete()
                except aiodocker.DockerError as e:
                    if e.status == 404:
                        # volume no longer exists
                        pass 
                    else:
                        raise e

            await self.docker.close()

        asyncio.get_event_loop().run_until_complete(asyncio.gather(aux()))

    def render(self, snapshot):
        return " ".join([f"-{k} {shlex.quote(str(v))}" for k, v in snapshot.items()])

    def collect_from(self):
        import docker
        client = docker.from_env()
        client.containers.run(
            'ubuntu:latest',
            'cp -r /data/container/* /data/host/',
            mounts= [
                docker.types.Mount(
                    target = "/data/container",
                    source = self.currentdatavolume['Name'],
                    type = "bind",
                ),
                docker.types.Mount(
                    target = "/data/host",
                    source = self.datadir,
                    type = "bind",
                ),
            ]
        )

    async def run_async(self, stop_event, stop_display_time):
        import uuid
        volume_name = f"bench_varda{str(uuid.uuid4())[0:16]}"
        container_name = f"bench_varda{str(uuid.uuid4())[0:16]}"

        container = await self.docker.containers.run(
            config={
                'Cmd': [self.run_cmd+" "+self.render(self.config)],
                'Image': self.image,
                'Binds': [
                    f'{volume_name}:/data'
                ] 
            },
            name = container_name,
        )
        self.containers.append(container)


        volumes = await self.docker.volumes.list()
        volume = None
        for v in volumes['Volumes']:
            if v['Name'] == volume_name:
                volume = v
        assert(volume != None)
        self.volumes.append(volume)

        logging.debug(f"Starting container {container.id} with image {self.image} and cmd {self.run_cmd}")
        res = await self.run_async_skeleton(DockerPod(container), stop_event, stop_display_time)

        return res
        
#class DockerComposeRunner(Runner):
#class DockerSwarmRunner
#class DockerComposeFactory

#class RemoteShellRunner -> ssh + setup machine test avec ansible
# RemoteDocker -> custom registry