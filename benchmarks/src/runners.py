import shlex
import subprocess
import time
import select
import os
import signal

from .models import *
from .settings import *

class ShellRunnerFactory:
    def __init__(self, run_cmd, run_cwd=None, stdout_termination_token=None, error_token="[ERROR]") -> None:
        self.run_cmd                    = run_cmd
        self.run_cwd                    = run_cwd
        self.stdout_termination_token   = stdout_termination_token
        self.error_token                = error_token

    def make(self, config):
        return ShellRunner(self.run_cmd, self.run_cwd, self.stdout_termination_token, self.error_token, config)

class MultiShellRunnerFactory:
    # factories order by start order
    def __init__(self, factories):
        self.factories = factories

    def make(self, config):
        runners = [ factory.make(config) for factory in self.factories]
        return OrderedMultiShellRunner(runners)

class OrderedMultiShellRunner:
    def __init__(self, runners):
        self.runners = runners

    def is_terminated(self, buffer):
        return not False in [ runner.is_terminated(buffer) for runner in self.runners ]  

    def has_failed(self, buffer):
        return not False in [ runner.has_failed(buffer) for runner in self.runners ]   

    def terminate(self, results):
        for res in results:
            os.killpg(os.getpgid(res.pid),signal.SIGTERM)
        for res in results:
            res.wait()

    def run(self) -> bool:
        start_time = time.time()
        poll_obj = select.poll()

        results = []
        for runner in self.runners:
            res = runner.popen()
            results.append(res)
            poll_obj.register(res.stdout, select.POLLIN)

        # All stdout are aggregated due to the poll
        stdout_buffer = ""
        last_elapse = 0

        def returncodes ():
            return [ res.returncodes for res in results ]

        while None in returncodes() and not time.time() - start_time > RUN_TIMEOUT:
            poll_result = poll_obj.poll(0)
            if int(time.time() - start_time) > last_elapse:
                last_elapse = int(time.time() - start_time)
                print(f"Ran for {last_elapse} s [Timeout: {RUN_TIMEOUT}s]")
            if poll_result:
                stdout_buffer += res.stdout.readline()

                if self.is_terminated(stdout_buffer):
                    self.terminate(results)
                    self.run_stdout = stdout_buffer
                    return True
                if self.has_failed(stdout_buffer):
                    self.terminate(results)
                    self.run_stdout = stdout_buffer
                    self.run_stderr = res.stderr.read()
                    print(stdout_buffer)
                    print(self.run_stderr)
                    return False

        if not None in returncodes():
            self.run_stdout = stdout_buffer 
            self.run_stderr = res.stderr.read()
            return sum(returncodes()) == 0
        else:
            self.terminate(res)
            print(stdout_buffer)
            print("Timeout !")
            return False

class ShellRunner:
    def __init__(self, run_cmd, run_cwd, stdout_termination_token, error_token, config) -> None:
        self.run_cmd    = run_cmd
        self.run_cwd    = run_cwd
        self.stdout_termination_token = stdout_termination_token
        self.error_token    = error_token

        self.config =   config
        
        self.run_stdout           = ""
        self.run_stderr           = ""

    def render(self, snapshot):
        return " ".join([f"-{k} {shlex.quote(str(v))}" for k,v in snapshot.items()])
    

    def popen(self):
        return subprocess.Popen(
            self.run_cmd+" "+self.render(self.config), 
            stdout= subprocess.PIPE,
            stderr= subprocess.PIPE,
            encoding='utf-8', 
            cwd=self.run_cwd,
            shell=True,
            preexec_fn=os.setpgrp,
        ) 

    def is_terminated(self, buffer):
        return self.stdout_termination_token and self.stdout_termination_token in buffer 

    def has_failed(self, buffer):
        return self.error_token and self.error_token in buffer 

    def terminate(self, result):
        os.killpg(os.getpgid(result.pid),signal.SIGTERM)
        result.wait()


    def run(self) -> bool:
        start_time = time.time()
        res = self.popen() 

        poll_obj = select.poll()
        poll_obj.register(res.stdout, select.POLLIN)

        stdout_buffer = ""
        last_elapse = 0
        while res.returncode == None and not time.time() - start_time > RUN_TIMEOUT :
            poll_result = poll_obj.poll(0)
            if int(time.time() - start_time) > last_elapse:
                last_elapse = int(time.time() - start_time)
                print(f"Ran for {last_elapse} s [Timeout: {RUN_TIMEOUT}s]")
            if poll_result:
                stdout_buffer += res.stdout.readline()
                if self.is_terminated(stdout_buffer):
                    self.terminate(res)
                    self.run_stdout = stdout_buffer
                    return True
                if self.has_failed(stdout_buffer):
                    self.terminate(res)
                    self.run_stdout = stdout_buffer
                    self.run_stderr = res.stderr.read()
                    print(stdout_buffer)
                    print(self.run_stderr)
                    return False

        if res.returncode:
            self.run_stdout = stdout_buffer 
            self.run_stderr = res.stderr.read()
            return res.returncode == 0
        else:
            self.terminate(res)
            print(stdout_buffer)
            print("Timeout !")
            return False