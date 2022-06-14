'''
Django ORM settings for sqlite3.
'''
import os
# Build paths inside the project like this: os.path.join(BASE_DIR, ...)
BASE_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

SECRET_KEY = 'eephaeNgahQu5FaiToh3weit9chiedeNaingeu7eejee3neigeitheicaCh4ceequa6your8ahShaToh5taoSheeWeen5uphei7w'

INSTALLED_APPS = ['src']

DATABASES = {
    'default': {
        'ENGINE': 'django.db.backends.sqlite3',
        'NAME': os.path.join(BASE_DIR, 'db.sqlite3'),
    }
}

DEFAULT_AUTO_FIELD = 'django.db.models.AutoField'