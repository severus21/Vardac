from abc import ABC, abstractmethod

from .runners import *

class RunnerFactory(ABC):
    def __init__(self, config_adaptor=lambda x: x, name=None) -> None:
        self.config_adaptor = config_adaptor
        self.name = name

    @abstractmethod
    def make(self, name, builder, config, i_run):
        pass


class ShellRunnerFactory(RunnerFactory):
    def __init__(self, run_cmd, run_cwd=None, stdout_termination_token=None, error_token="[ERROR]", config_adaptor=lambda x: x, set_stop_event=False, name=None, environment={}) -> None:
        super().__init__(config_adaptor, name = name)
        self.run_cmd = run_cmd
        self.run_cwd = run_cwd
        self.stdout_termination_token = stdout_termination_token
        self.error_token = error_token
        self.set_stop_event = set_stop_event
        self.environment = environment

    def make(self, name, builder, config, i_run):
        if self.name:
            name = self.name # user-defined has higher priority
        return ShellRunner(name, i_run, self.run_cmd, self.run_cwd, self.stdout_termination_token, self.error_token, self.config_adaptor(config), self.set_stop_event, self.environment)

class DockerRunnerFactory(RunnerFactory):
    def __init__(self, run_cmd, stdout_termination_token=None, error_token="[ERROR]", config_adaptor=lambda x: x, set_stop_event=False, name=None) -> None:
        super().__init__(config_adaptor, name = name)
        self.run_cmd = run_cmd
        self.stdout_termination_token = stdout_termination_token
        self.error_token = error_token
        self.set_stop_event = set_stop_event

    def make(self, name, builder, config, i_run):
        if self.name:
            name = self.name # user-defined has higher priority
        return DockerRunner(name, i_run, builder.image_name, self.run_cmd, self.stdout_termination_token, self.error_token, self.config_adaptor(config), self.set_stop_event)

class MultiShellRunnerFactory(RunnerFactory):
    # factories order by start order
    def __init__(self, factories, config_adaptor=lambda x: x, name=None):
        super().__init__(config_adaptor, name=name)
        self.factories = factories

    def make(self, name, builder, config, i_run):
        if self.name:
            name = self.name # user-defined has higher priority
        config = self.config_adaptor(config)
        runners = [factory.make(name, builder, config, i_run) for factory in self.factories]
        return OrderedMultiShellRunner(name, i_run, runners, config)