from abc import ABC, abstractmethod

from .runners import *

class RunnerFactory(ABC):
    def __init__(self, config_adaptor=lambda x: x) -> None:
        self.config_adaptor = config_adaptor

    @abstractmethod
    def make(self, builder, config):
        pass


class ShellRunnerFactory(RunnerFactory):
    def __init__(self, run_cmd, run_cwd=None, stdout_termination_token=None, error_token="[ERROR]", config_adaptor=lambda x: x, set_stop_event=False) -> None:
        super().__init__(config_adaptor)
        self.run_cmd = run_cmd
        self.run_cwd = run_cwd
        self.stdout_termination_token = stdout_termination_token
        self.error_token = error_token
        self.set_stop_event = set_stop_event

    def make(self, builder, config):
        return ShellRunner(self.run_cmd, self.run_cwd, self.stdout_termination_token, self.error_token, self.config_adaptor(config), self.set_stop_event)

class DockerRunnerFactory(RunnerFactory):
    def __init__(self, run_cmd, stdout_termination_token=None, error_token="[ERROR]", config_adaptor=lambda x: x, set_stop_event=False) -> None:
        super().__init__(config_adaptor)
        self.run_cmd = run_cmd
        self.stdout_termination_token = stdout_termination_token
        self.error_token = error_token
        self.set_stop_event = set_stop_event

    def make(self, builder, config):
        return DockerRunner(builder.image_name, self.run_cmd, self.stdout_termination_token, self.error_token, self.config_adaptor(config), self.set_stop_event)

class MultiShellRunnerFactory(RunnerFactory):
    # factories order by start order
    def __init__(self, factories, config_adaptor=lambda x: x):
        super().__init__(config_adaptor)
        self.factories = factories

    def make(self, config):
        config = self.config_adaptor(config)
        runners = [factory.make(config) for factory in self.factories]
        return OrderedMultiShellRunner(runners, config)