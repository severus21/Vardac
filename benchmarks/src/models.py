from django.db import models  # type: ignore
from jsonfield import JSONField

import collections

import platform


class HostSpec(models.Model):
    name = models.CharField(max_length=64)

    creation = models.DateTimeField(auto_now_add=True)
    last_modification = models.DateTimeField(auto_now=True)

    def __str__(self) -> str:
        return self.name

    def get_hostname():
        return platform.node()


class SoftSpec(models.Model):
    '''Config'''
    os_version = models.CharField(max_length=64)
    varda_version = models.JSONField()
    docker_version = models.CharField(max_length=64)

    creation = models.DateTimeField(auto_now_add=True)
    last_modification = models.DateTimeField(auto_now=True)

    def get_os_version():
        return platform.version()

    def get_docker_version():
        import subprocess
        res = subprocess.run("docker version",
                             capture_output=True,
                             encoding='utf-8',
                             shell=True,
                             )
        assert(res.returncode == 0)
        return res.stdout

    def get_varda_version():
        import subprocess
        res = subprocess.run("dune exec --profile release -- compspec info --codegen-plgs --check-plgs", 
            capture_output=True, 
            encoding='utf-8',
            shell=True,
        )
        assert(res.returncode == 0)
        return res.stdout


class BenchResult(models.Model):
    run_config = JSONField(
        load_kwargs={'object_pairs_hook': collections.OrderedDict})
    results = JSONField(
        load_kwargs={'object_pairs_hook': collections.OrderedDict})

    creation = models.DateTimeField(auto_now_add=True)
    last_modification = models.DateTimeField(auto_now=True)

    def __str__(self):
        return f"[{self.creation}] {self.run_config}"


class Bench(models.Model):
    name = models.CharField(max_length=64)

    host_spec = models.ForeignKey(
        HostSpec, on_delete=models.CASCADE, default=None, blank=True, null=True)
    soft_spec = models.ForeignKey(
        SoftSpec, on_delete=models.CASCADE, default=None, blank=True, null=True)

    build_hash = models.CharField(max_length=64, default="")
    project_hash = models.CharField(max_length=64, default="")

    build_cmd = models.CharField(max_length=4196, default="")

    results = models.ManyToManyField(BenchResult, blank=True)

    creation = models.DateTimeField(auto_now_add=True)
    last_modification = models.DateTimeField(auto_now=True)

    def __str__(self) -> str:
        return self.name
