import os
os.environ.setdefault("DJANGO_SETTINGS_MODULE", "orm.settings")

#Ensure django settings are read
from django.core.wsgi import get_wsgi_application
from django.db.models import Q
application = get_wsgi_application()

#Load models
from src.models import *

def pprint(entries):
    for e in entries:
        print(e)

script_helps = '''

pprint -
'''

print(script_helps)
