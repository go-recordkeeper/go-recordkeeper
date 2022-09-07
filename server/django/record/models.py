from django.contrib.auth.models import User
from django.core.validators import MaxValueValidator, MinValueValidator
from django.db import models
from django.utils.translation import gettext_lazy as _

from .go import Board, Stone


class Record(models.Model):
    owner = models.ForeignKey(User, on_delete=models.CASCADE)
    board_size = models.IntegerField()
    created = models.DateTimeField(auto_now_add=True)
    name = models.CharField(max_length=200, blank=True, null=True)
    black_player = models.CharField(max_length=200, default='Black')
    white_player = models.CharField(max_length=200, default='White')
    comment = models.CharField(max_length=400, default='')
    handicap = models.IntegerField(default=0, validators=[MinValueValidator(0)])
    # 0.5 less than the actual komi
    integer_komi = models.IntegerField(default=7)
    ruleset = models.CharField(
        max_length=3, choices=[('AGA', 'AGA'), ('JAP', 'Japanese'), ('CHN', 'Chinese')]
    )

    @property
    def komi(self) -> float:
        return self.integer_komi + 0.5

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
        # TODO throw a useful exception
        assert x >= 0
        assert y >= 0
        assert x < self.board_size
        assert y < self.board_size
        return Move(
            record=self,
            position=x + self.board_size * y,
            color=self.next_move_color,
            move=self.next_move_number,
        )

    def pass_turn(self) -> 'Move':
        return Move(
            record=self,
            position=None,
            color=self.next_move_color,
            move=self.next_move_number,
        )


class Move(models.Model):
    class Color(models.TextChoices):
        BLACK = 'B', _('Black')
        WHITE = 'W', _('White')
        PASS = 'P', _('Pass')

    record = models.ForeignKey(Record, related_name='moves', on_delete=models.CASCADE)
    position = models.IntegerField(
        validators=[MinValueValidator(0), MaxValueValidator((19 * 19) - 1)], blank=True, null=True
    )
    color = models.CharField(max_length=1, choices=Color.choices)
    move = models.PositiveIntegerField()
    # captured_by = models.ForeignKey('Move', blank=True, null=True, on_delete=models.CASCADE)

    class Meta:
        indexes = [models.Index(fields=['record', 'move'])]
        constraints = [models.UniqueConstraint(name='unique_move', fields=['record', 'move'])]
        ordering = ['move']

    @property
    def x(self):
        return self.position % self.record.board_size

    @property
    def y(self):
        return self.position // self.record.board_size

    @property
    def board_state(self):
        board = Board(self.record.board_size)
        for (color, position) in self.record.moves.filter(move__lte=self.move).values_list(
            'color',
            'position',
        ):
            if position is not None:
                board.place_stone(Stone(color), position % board.size, position // board.size)
        return board
