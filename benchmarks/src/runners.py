import shlex
import subprocess
from threading import TIMEOUT_MAX
import time
import select
import os
import signal
import asyncio
import logging
import uuid

from .models import *
from .settings import *

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
 
BaseSubprocessTransport.__del__ = silence_event_loop_closed(BaseSubprocessTransport.__del__)
"""fix yelling at me error end"""

async def display_time(stop_display_time, start_time, last_elapse=0):
    if stop_display_time.is_set():
        return True
    else:
        if int(time.time() - start_time) > last_elapse:
            last_elapse = int(time.time() - start_time)
            print(f"{id}Ran for {last_elapse} s [Timeout: {RUN_TIMEOUT}s]")
        await asyncio.sleep(0.1) #at least
        await display_time(stop_display_time, start_time, last_elapse) 

class Runner:
    def __init__(self, name) -> None:
        self.name   = name

class RunnerFactory:
    def __init__(self, name, config_adaptor=lambda x: x) -> None:
        self.name                       = name
        self.config_adaptor             = config_adaptor

class ShellRunnerFactory(RunnerFactory):
    def __init__(self, name, run_cmd, run_cwd=None, stdout_termination_token=None, error_token="[ERROR]", config_adaptor=lambda x: x, set_stop_event=False) -> None:
        super().__init__(name, config_adaptor)
        self.run_cmd                    = run_cmd
        self.run_cwd                    = run_cwd
        self.stdout_termination_token   = stdout_termination_token
        self.error_token                = error_token
        self.set_stop_event             = set_stop_event

    def make(self, config):
        return ShellRunner(self.name, self.run_cmd, self.run_cwd, self.stdout_termination_token, self.error_token, self.config_adaptor(config), self.set_stop_event)

class MultiShellRunnerFactory(RunnerFactory):
    # factories order by start order
    def __init__(self, name, factories, config_adaptor=lambda x: x):
        super().__init__(name, config_adaptor)
        self.factories = factories

    def make(self, config):
        config = self.config_adaptor(config)
        runners = [ factory.make(config) for factory in self.factories]
        return OrderedMultiShellRunner(self.name, runners, config)

class OrderedMultiShellRunner(Runner):
    def __init__(self, name, runners, config):
        super().__init__(name)
        self.runners    = runners
        self.config     = config

        self._run_stdout           = ""
        self._run_stderr           = ""

    @property
    def run_stdout(self):
        return '\n'.join([self._run_stdout]+[r.run_stdout for r in self.runners])

    @property
    def run_stderr(self):
        return '\n'.join([self._run_stderr]+[r.run_stdout for r in self.runners])


    def is_terminated(self, buffer):
        return not False in [ runner.is_terminated(buffer) for runner in self.runners ]  

    def has_failed(self, buffer):
        return not False in [ runner.has_failed(buffer) for runner in self.runners ]   

    def terminate(self, results):
        for res in results:
            os.killpg(os.getpgid(res.pid),signal.SIGTERM)
        for res in results:
            res.wait()
        time.sleep(1)

    async def _run_async(self, stop_event, stop_display_time):
        return await asyncio.gather(
            *[runner.run_async_shell(stop_event, stop_display_time) for runner in self.runners],
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

class ShellRunner(Runner):
    def __init__(self, name, run_cmd, run_cwd, stdout_termination_token, error_token, config, set_stop_event) -> None:
        super().__init__(name)
        self.run_cmd    = run_cmd
        self.run_cwd    = run_cwd
        self.stdout_termination_token = stdout_termination_token
        self.error_token    = error_token

        self.config =   config
        self.set_stop_event = set_stop_event
        
        self.run_stdout           = ""
        self.run_stderr           = ""

    def render(self, snapshot):
        return " ".join([f"-{k} {shlex.quote(str(v))}" for k,v in snapshot.items()])

    async def run_async_shell(self, stop_event, stop_display_time):
        # Start child process
        process = await asyncio.create_subprocess_shell(
            self.run_cmd+" "+self.render(self.config),
            stdout=asyncio.subprocess.PIPE, 
            stderr=asyncio.subprocess.PIPE,
            cwd=self.run_cwd,
            shell=True,
            preexec_fn=os.setpgrp
            )

        stdout_buffer = ""

        # Read line (sequence of bytes ending with b'\n') asynchronously
        while True:
            # Parent event
            if stop_event != None and stop_event.is_set():
                logging.debug(f"{self.name}> Cancelled from parent!")
                await self.terminate(process, stop_event, stop_display_time)

                # stdout_termination_token == None => this task never terminates by itself
                return self.stdout_termination_token == None or self.is_terminated(stdout_buffer) 

            try:
                line = await asyncio.wait_for(process.stdout.readline(), RUN_TIMEOUT)
            except asyncio.TimeoutError:
                logging.error(f"{self.name}> Timeout !")
                print(stdout_buffer)
                await self.terminate(process, stop_event, stop_display_time)
                return False
            except asyncio.CancelledError:
                logging.error(f"{self.name}> Cancelled !")
                await self.terminate(process, stop_event, stop_display_time)
                return False
            else:
                line = line.decode()
                stdout_buffer += line 

                if not line: # EOF
                    logging.debug(f"{self.name}> EOF")
                    await self.terminate(process, stop_event, stop_display_time)
                    return True
                elif self.is_terminated(line):
                    logging.debug(f"{self.name}> Is terminated")
                    await self.terminate(process, stop_event, stop_display_time)
                    self.run_stdout = stdout_buffer
                    return True
                elif self.has_failed(line):
                    logging.error(f"{self.name}> Has failed")
                    self._run_stdout = stdout_buffer
                    self._run_stderr = process.stderr.read()
                    print(stdout_buffer)
                    print(self.run_stderr)
                    await self.terminate(process, stop_event, stop_display_time)
                    return False

    async def run_async_with_displaytime(self):
        stop_display_time = asyncio.Event()
        return await asyncio.gather( 
            self.run_async_shell(None, stop_display_time), 
            display_time(stop_display_time, time.time()), 
            return_exceptions=True)

    def run_async(self):
        result = asyncio.run(self.run_async_with_displaytime())
        if result:
            result = result[0]
        return result

    def is_terminated(self, buffer):
        return self.stdout_termination_token and self.stdout_termination_token in buffer 

    def has_failed(self, buffer):
        return self.error_token and self.error_token in buffer 

    async def terminate(self, result, stop_event, stop_display_time):
        print(f"Try terminate{self.name} {result.pid}")
        stop_display_time.set()
        if stop_event != None and self.set_stop_event:
            stop_event.set()
        result.terminate()
        os.killpg(os.getpgid(result.pid), signal.SIGTERM)
        await result.wait()
        await asyncio.sleep(1) #Needed to be able to bind ports afterwards
        print(f"End terminate {self.name}")