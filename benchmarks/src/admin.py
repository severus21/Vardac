from re import search
from django.contrib import admin
from modeltranslation.admin import TranslationAdmin

from .models import *

# Register your models here.

class BenchAdmin(admin.ModelAdmin):
        search_fields = ("name",)



admin.site.register(HostSpec)
admin.site.register(SoftSpec)
admin.site.register(BenchResult)
admin.site.register(Bench, BenchAdmin)
