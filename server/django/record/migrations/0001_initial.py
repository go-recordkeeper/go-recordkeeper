# Generated by Django 4.1 on 2022-09-07 22:54

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
            name='Record',
            fields=[
                (
                    'id',
                    models.BigAutoField(
                        auto_created=True, primary_key=True, serialize=False, verbose_name='ID'
                    ),
                ),
                ('board_size', models.IntegerField()),
                ('created', models.DateTimeField()),
                ('name', models.CharField(blank=True, max_length=200, null=True)),
                ('black_player', models.CharField(default='Black', max_length=200)),
                ('white_player', models.CharField(default='White', max_length=200)),
                ('comment', models.CharField(default='', max_length=400)),
                (
                    'handicap',
                    models.IntegerField(
                        default=0, validators=[django.core.validators.MinValueValidator(0)]
                    ),
                ),
                ('integer_komi', models.IntegerField(default=7)),
                (
                    'ruleset',
                    models.CharField(
                        choices=[('AGA', 'AGA'), ('JAP', 'Japanese'), ('CHN', 'Chinese')],
                        max_length=3,
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
                    'position',
                    models.IntegerField(
                        blank=True,
                        null=True,
                        validators=[
                            django.core.validators.MinValueValidator(0),
                            django.core.validators.MaxValueValidator(360),
                        ],
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
                    'record',
                    models.ForeignKey(
                        on_delete=django.db.models.deletion.CASCADE,
                        related_name='moves',
                        to='record.record',
                    ),
                ),
            ],
            options={'ordering': ['move']},
        ),
        migrations.AddIndex(
            model_name='move',
            index=models.Index(fields=['record', 'move'], name='record_move_record__eced10_idx'),
        ),
        migrations.AddConstraint(
            model_name='move',
            constraint=models.UniqueConstraint(fields=('record', 'move'), name='unique_move'),
        ),
    ]
