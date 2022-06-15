import logging
from django.db import models # type: ignore
from django.core.files.base import ContentFile # type: ignore
from django.db.utils import OperationalError
from jsonfield import JSONField

import collections
import unidecode # type: ignore
import os.path
import mimetypes


class HostSpec(models.Model):
    name = models.CharField(max_length=64)

    creation = models.DateTimeField(auto_now_add=True) 
    last_modification = models.DateTimeField(auto_now=True)

    def __str__(self) -> str:
        return self.name

class ConfigSpec(models.Model):
    '''Config'''
    varda_version = models.CharField(max_length=64)
    akka_version = models.CharField(max_length=64)
    docker_version = models.CharField(max_length=64)
    host = models.ForeignKey(HostSpec, on_delete=models.CASCADE)

    creation = models.DateTimeField(auto_now_add=True) 
    last_modification = models.DateTimeField(auto_now=True)

    def __str__(self) -> str:
        return str(dict(self))

class BenchResult(models.Model):
    run_config  = JSONField(load_kwargs={'object_pairs_hook': collections.OrderedDict})
    results     = JSONField(load_kwargs={'object_pairs_hook': collections.OrderedDict})

    creation = models.DateTimeField(auto_now_add=True) 
    last_modification = models.DateTimeField(auto_now=True)

    def __str__(self):
        return f"[{self.creation}] {self.run_config}"

class Bench(models.Model):
    name = models.CharField(max_length=64)
    build_hash = models.CharField(max_length=64, default="")
    project_hash = models.CharField(max_length=64, default="")

    results = models.ManyToManyField(BenchResult, blank=True)
    
    creation = models.DateTimeField(auto_now_add=True) 
    last_modification = models.DateTimeField(auto_now=True)

    def __str__(self) -> str:
        return self.name

