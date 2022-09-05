from django.contrib.auth.models import User
from django.core.validators import MaxValueValidator, MinValueValidator
from django.db import models
from django.utils.translation import gettext_lazy as _

from .go import Board, Stone


class Record(models.Model):
    owner = models.ForeignKey(User, on_delete=models.CASCADE)
    board_size = models.IntegerField()

    @property
    def last_move(self):
        return self.moves.last()

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
            record=self,
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

    record = models.ForeignKey(Record, related_name='moves', on_delete=models.CASCADE)
    x = models.IntegerField(validators=[MinValueValidator(0), MaxValueValidator(18)])
    y = models.IntegerField(validators=[MinValueValidator(0), MaxValueValidator(18)])
    color = models.CharField(max_length=1, choices=Color.choices)
    move = models.PositiveIntegerField()
    # captured_by = models.ForeignKey('Move', blank=True, null=True, on_delete=models.CASCADE)

    class Meta:
        indexes = [models.Index(fields=['record', 'move'])]
        constraints = [models.UniqueConstraint(name='unique_move', fields=['record', 'move'])]
        ordering = ['move']

    @property
    def board_state(self):
        board = Board(self.record.board_size)
        for (color, x, y) in self.record.moves.filter(move__lte=self.move).values_list(
            'color', 'x', 'y'
        ):
            board.place_stone(Stone(color), x, y)
        return board
