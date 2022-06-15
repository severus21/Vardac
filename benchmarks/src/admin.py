from django.contrib import admin
from modeltranslation.admin import TranslationAdmin

from .models import *

# Register your models here.

class ArticleAdmin(admin.ModelAdmin):
        exclude = ("toc", "html", "pdf_file")

admin.site.register(HostSpec)
admin.site.register(ConfigSpec)
admin.site.register(BenchResult)
admin.site.register(Bench)
