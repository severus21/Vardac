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
    
    def run(self) -> bool:
        start_time = time.time()
        res = subprocess.Popen(
            self.run_cmd+" "+self.render(self.config), 
            stdout= subprocess.PIPE,
            stderr= subprocess.PIPE,
            encoding='utf-8', 
            cwd=self.run_cwd,
            shell=True,
            preexec_fn=os.setpgrp,
            )

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
                if self.stdout_termination_token and self.stdout_termination_token in stdout_buffer:
                    os.killpg(os.getpgid(res.pid),signal.SIGTERM)
                    res.wait()
                    self.run_stdout = stdout_buffer
                    return True
                if self.error_token and self.error_token in stdout_buffer:
                    os.killpg(os.getpgid(res.pid),signal.SIGTERM)
                    res.wait()
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
            os.killpg(os.getpgid(res.pid),signal.SIGTERM)
            res.wait()
            print(stdout_buffer)
            print("Timeout !")
            return False