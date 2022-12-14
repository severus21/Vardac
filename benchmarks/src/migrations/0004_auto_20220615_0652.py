# Generated by Django 3.2.5 on 2022-06-15 06:52

from django.db import migrations, models


class Migration(migrations.Migration):

    dependencies = [
        ('src', '0003_auto_20220615_0648'),
    ]

    operations = [
        migrations.AlterField(
            model_name='bench',
            name='creation',
            field=models.DateTimeField(auto_now_add=True),
        ),
        migrations.AlterField(
            model_name='bench',
            name='last_modification',
            field=models.DateTimeField(auto_now=True),
        ),
        migrations.AlterField(
            model_name='benchresult',
            name='creation',
            field=models.DateTimeField(auto_now_add=True),
        ),
        migrations.AlterField(
            model_name='benchresult',
            name='last_modification',
            field=models.DateTimeField(auto_now=True),
        ),
        migrations.AlterField(
            model_name='configspec',
            name='creation',
            field=models.DateTimeField(auto_now_add=True),
        ),
        migrations.AlterField(
            model_name='configspec',
            name='last_modification',
            field=models.DateTimeField(auto_now=True),
        ),
        migrations.AlterField(
            model_name='hostspec',
            name='creation',
            field=models.DateTimeField(auto_now_add=True),
        ),
        migrations.AlterField(
            model_name='hostspec',
            name='last_modification',
            field=models.DateTimeField(auto_now=True),
        ),
    ]
