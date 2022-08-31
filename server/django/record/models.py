from django.db import models
from django.contrib.auth.models import User
from django.core.validators import MinValueValidator, MaxValueValidator
from django.utils.translation import gettext_lazy as _

class Game(models.Model):
    owner = User()

class Move(models.Model):

    class Color(models.TextChoices):
        BLACK = 'B', _('Black')
        WHITE = 'W', _('White')
        PASS = 'P', _('Pass')

    game = models.ForeignKey(Game, related_name='moves', on_delete=models.CASCADE)
    x = models.IntegerField(validators=[MinValueValidator(0), MaxValueValidator(18)])
    y = models.IntegerField(validators=[MinValueValidator(0), MaxValueValidator(18)])
    color = models.CharField(max_length=1, choices=Color.choices)
    move = models.PositiveIntegerField()

    class Meta:
        indexes = [
            models.Index(fields=['game', 'move'])
        ]
        constraints = [
            models.UniqueConstraint(name='unique_move', fields=['game', 'move'])
        ]