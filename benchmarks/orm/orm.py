#https://stackoverflow.com/questions/937742/use-django-orm-as-standalone
#https://github.com/masnun/django-orm-standalone/blob/master/main.py

from django.conf import settings
from .settings import DATABASES
settings.configure( DATABASES = DATABASES )

from django.db import models
from src.models import *
