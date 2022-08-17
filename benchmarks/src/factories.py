from .runners import *

class RunnerFactory:
    def __init__(self, name, config_adaptor=lambda x: x) -> None:
        self.name = name
        self.config_adaptor = config_adaptor


class ShellRunnerFactory(RunnerFactory):
    def __init__(self, name, run_cmd, run_cwd=None, stdout_termination_token=None, error_token="[ERROR]", config_adaptor=lambda x: x, set_stop_event=False) -> None:
        super().__init__(name, config_adaptor)
        self.run_cmd = run_cmd
        self.run_cwd = run_cwd
        self.stdout_termination_token = stdout_termination_token
        self.error_token = error_token
        self.set_stop_event = set_stop_event

    def make(self, config):
        return ShellRunner(self.name, self.run_cmd, self.run_cwd, self.stdout_termination_token, self.error_token, self.config_adaptor(config), self.set_stop_event)

class DockerRunnerFactory(RunnerFactory):
    def __init__(self, name, image, run_cmd, stdout_termination_token=None, error_token="[ERROR]", config_adaptor=lambda x: x, set_stop_event=False) -> None:
        super().__init__(name, config_adaptor)
        self.image = image
        self.run_cmd = run_cmd
        self.stdout_termination_token = stdout_termination_token
        self.error_token = error_token
        self.set_stop_event = set_stop_event

    def make(self, config):
        return DockerRunner(self.name, self.image, self.run_cmd, self.stdout_termination_token, self.error_token, self.config_adaptor(config), self.set_stop_event)

class MultiShellRunnerFactory(RunnerFactory):
    # factories order by start order
    def __init__(self, name, factories, config_adaptor=lambda x: x):
        super().__init__(name, config_adaptor)
        self.factories = factories

    def make(self, config):
        config = self.config_adaptor(config)
        runners = [factory.make(config) for factory in self.factories]
        return OrderedMultiShellRunner(self.name, runners, config)