import logging
from django.db import models # type: ignore
from django.core.files.base import ContentFile # type: ignore
from django.db.utils import OperationalError
import unidecode # type: ignore
import os.path
import mimetypes


class HostSpec(models.Model):
    name = models.CharField(max_length=64)

    def __str__(self) -> str:
        return self.name

class ConfigSpec(models.Model):
    '''Config'''
    varda_version = models.CharField(max_length=64)
    akka_version = models.CharField(max_length=64)
    docker_version = models.CharField(max_length=64)
    host = models.ForeignKey(HostSpec, on_delete=models.CASCADE)

    def __str__(self) -> str:
        return str(dict(self))

class Bench(models.Model):
    name = models.CharField(max_length=64)
    build_hash = models.CharField(max_length=64, default="")
    project_hash = models.CharField(max_length=64, default="")
    
    def __str__(self) -> str:
        return self.name

class BenchResult(models.Model):
    '''Use for archiving the raw material'''