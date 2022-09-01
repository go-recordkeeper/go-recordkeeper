from django.contrib.auth.models import User
from django.core.validators import MaxValueValidator, MinValueValidator
from django.db import models
from django.utils.translation import gettext_lazy as _


class Game(models.Model):
    owner = models.ForeignKey(User, on_delete=models.CASCADE)

    @property
    def next_move_number(self):
        if self.moves.exists():
            return self.moves.last().move + 1
        return 1

    @property
    def next_move_color(self):
        if self.moves.exists():
            last_color = self.moves.last().color
            if last_color == 'B':
                return 'W'
        return 'B'

    def next_move(self, x, y) -> 'Move':
        return Move(
            game=self,
            x=x,
            y=y,
            color=self.next_move_color,
            move=self.next_move_number,
        )


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
    # captured_by = models.ForeignKey('Move', blank=True, null=True, on_delete=models.CASCADE)

    class Meta:
        indexes = [models.Index(fields=['game', 'move'])]
        constraints = [models.UniqueConstraint(name='unique_move', fields=['game', 'move'])]
        ordering = ['move']
