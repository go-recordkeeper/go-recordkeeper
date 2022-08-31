# Generated by Django 4.1 on 2022-08-31 20:13

from django.conf import settings
import django.core.validators
from django.db import migrations, models
import django.db.models.deletion


class Migration(migrations.Migration):

    initial = True

    dependencies = [
        migrations.swappable_dependency(settings.AUTH_USER_MODEL),
    ]

    operations = [
        migrations.CreateModel(
            name='Game',
            fields=[
                (
                    'id',
                    models.BigAutoField(
                        auto_created=True, primary_key=True, serialize=False, verbose_name='ID'
                    ),
                ),
                (
                    'owner',
                    models.ForeignKey(
                        on_delete=django.db.models.deletion.CASCADE, to=settings.AUTH_USER_MODEL
                    ),
                ),
            ],
        ),
        migrations.CreateModel(
            name='Move',
            fields=[
                (
                    'id',
                    models.BigAutoField(
                        auto_created=True, primary_key=True, serialize=False, verbose_name='ID'
                    ),
                ),
                (
                    'x',
                    models.IntegerField(
                        validators=[
                            django.core.validators.MinValueValidator(0),
                            django.core.validators.MaxValueValidator(18),
                        ]
                    ),
                ),
                (
                    'y',
                    models.IntegerField(
                        validators=[
                            django.core.validators.MinValueValidator(0),
                            django.core.validators.MaxValueValidator(18),
                        ]
                    ),
                ),
                (
                    'color',
                    models.CharField(
                        choices=[('B', 'Black'), ('W', 'White'), ('P', 'Pass')], max_length=1
                    ),
                ),
                ('move', models.PositiveIntegerField()),
                (
                    'game',
                    models.ForeignKey(
                        on_delete=django.db.models.deletion.CASCADE,
                        related_name='moves',
                        to='record.game',
                    ),
                ),
            ],
        ),
        migrations.AddIndex(
            model_name='move',
            index=models.Index(fields=['game', 'move'], name='record_move_game_id_6d3ec7_idx'),
        ),
        migrations.AddConstraint(
            model_name='move',
            constraint=models.UniqueConstraint(fields=('game', 'move'), name='unique_move'),
        ),
    ]
