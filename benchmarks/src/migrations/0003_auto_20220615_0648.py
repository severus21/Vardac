# Generated by Django 3.2.5 on 2022-06-15 06:48

import collections
from django.db import migrations, models
import django.utils.timezone
import jsonfield.fields


class Migration(migrations.Migration):

    dependencies = [
        ('src', '0002_auto_20220614_0552'),
    ]

    operations = [
        migrations.AddField(
            model_name='bench',
            name='creation',
            field=models.DateField(auto_now_add=True, default=django.utils.timezone.now),
            preserve_default=False,
        ),
        migrations.AddField(
            model_name='bench',
            name='last_modification',
            field=models.DateField(auto_now=True),
        ),
        migrations.AddField(
            model_name='bench',
            name='results',
            field=models.ManyToManyField(blank=True, to='src.BenchResult'),
        ),
        migrations.AddField(
            model_name='benchresult',
            name='creation',
            field=models.DateField(auto_now_add=True, default=django.utils.timezone.now),
            preserve_default=False,
        ),
        migrations.AddField(
            model_name='benchresult',
            name='last_modification',
            field=models.DateField(auto_now=True),
        ),
        migrations.AddField(
            model_name='benchresult',
            name='results',
            field=jsonfield.fields.JSONField(default=django.utils.timezone.now, load_kwargs={'object_pairs_hook': collections.OrderedDict}),
            preserve_default=False,
        ),
        migrations.AddField(
            model_name='benchresult',
            name='run_config',
            field=jsonfield.fields.JSONField(default=django.utils.timezone.now, load_kwargs={'object_pairs_hook': collections.OrderedDict}),
            preserve_default=False,
        ),
        migrations.AddField(
            model_name='configspec',
            name='creation',
            field=models.DateField(auto_now_add=True, default=django.utils.timezone.now),
            preserve_default=False,
        ),
        migrations.AddField(
            model_name='configspec',
            name='last_modification',
            field=models.DateField(auto_now=True),
        ),
        migrations.AddField(
            model_name='hostspec',
            name='creation',
            field=models.DateField(auto_now_add=True, default=django.utils.timezone.now),
            preserve_default=False,
        ),
        migrations.AddField(
            model_name='hostspec',
            name='last_modification',
            field=models.DateField(auto_now=True),
        ),
    ]
